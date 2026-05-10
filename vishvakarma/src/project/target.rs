use std::{
    borrow::Borrow,
    collections::HashSet,
    ffi::OsString,
    fmt::Write,
    path::{Path, PathBuf},
    process::Command,
    rc::Rc,
};

use arachne::{Graph, MapGraph};
use wohpe_env::wohpe;

use crate::{
    RcCmp,
    parser::{
        ast::{self, ExecutableKind, ItemPath, TargetKind},
        span::{Location, Spanned},
    },
    project::{EvalError, Interpreter},
};

const EDITION: &str = "2024";
const BARE_RV64: &str = "riscv64gc-unknown-none-elf";

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum TargetArch {
    Native,
    BareRV64,
}

impl TargetArch {
    pub fn as_str(&self) -> &'static str {
        match self {
            TargetArch::Native => "native",
            TargetArch::BareRV64 => "bare-rv64",
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum Profile {
    Release,
    Debug,
    Check,
}

#[derive(Debug, Clone, Copy)]
enum Language {
    Rust,
}

#[derive(Debug)]
pub struct Target {
    name: Rc<str>,
    module: PathBuf,
    module_path: ItemPath,
    kind: TargetKind,
    language: Language,
    root: PathBuf,
    dependencies: Rc<[Rc<Target>]>,
    linker_script: Option<PathBuf>,
    panic: Option<Rc<str>>,
    definition: PathBuf,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
struct RealizedTarget {
    arch: TargetArch,
    test: bool,
    target: RcCmp<Target>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct BorrowedRealizedTarget<'a> {
    arch: TargetArch,
    test: bool,
    target: &'a RcCmp<Target>,
}

trait BorrowRealizedTarget {
    fn as_borrow(&self) -> BorrowedRealizedTarget<'_>;
}

impl<'a> std::fmt::Debug for dyn BorrowRealizedTarget + 'a {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.as_borrow())
    }
}

impl BorrowRealizedTarget for BorrowedRealizedTarget<'_> {
    fn as_borrow(&self) -> BorrowedRealizedTarget<'_> {
        *self
    }
}

impl BorrowRealizedTarget for RealizedTarget {
    fn as_borrow(&self) -> BorrowedRealizedTarget<'_> {
        BorrowedRealizedTarget {
            arch: self.arch,
            test: self.test,
            target: &self.target,
        }
    }
}

impl<'a> std::hash::Hash for dyn BorrowRealizedTarget + 'a {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_borrow().hash(state);
    }
}

impl<'a> Borrow<dyn 'a + BorrowRealizedTarget> for RealizedTarget {
    fn borrow(&self) -> &(dyn 'a + BorrowRealizedTarget) {
        self
    }
}

impl<'a> PartialEq for dyn BorrowRealizedTarget + 'a {
    fn eq(&self, other: &Self) -> bool {
        self.as_borrow() == other.as_borrow()
    }
}
impl<'a> Eq for dyn BorrowRealizedTarget + 'a {}

impl<'a> ToOwned for dyn BorrowRealizedTarget + 'a {
    type Owned = RealizedTarget;

    fn to_owned(&self) -> Self::Owned {
        let tgt = self.as_borrow();
        RealizedTarget {
            arch: tgt.arch,
            test: tgt.test,
            target: tgt.target.clone(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct TargetStatus {
    up_to_date: bool,
}

struct TargetRequest<'a> {
    target: &'a RcCmp<Target>,
    test: bool,
}

impl<'a> TargetRequest<'a> {
    fn new(target: &'a RcCmp<Target>) -> Self {
        TargetRequest {
            target,
            test: false,
        }
    }

    fn test(target: &'a RcCmp<Target>) -> Self {
        TargetRequest { target, test: true }
    }
}

fn build_target_graph<'a, I>(
    targets: I,
    project_root: &Path,
    build_root: &Path,
    profile: Profile,
) -> Result<MapGraph<RealizedTarget, TargetStatus>, EvalError>
where
    I: Iterator<Item = TargetRequest<'a>>,
{
    let mut target_graph = MapGraph::new_empty();

    fn insert_target(
        graph: &mut MapGraph<RealizedTarget, TargetStatus>,
        project_root: &Path,
        build_root: &Path,
        profile: Profile,
        target: &RcCmp<Target>,
        parent_arch: TargetArch,
        test: bool,
    ) -> Result<(), EvalError> {
        wohpe::trace!(
            "Attempting to insert {} (test={:?},parent arch={:?})",
            target.name(),
            test,
            parent_arch,
        );

        let arch = target.inferred_arch(parent_arch);
        let borrowed_target =
            &BorrowedRealizedTarget { arch, test, target } as &dyn BorrowRealizedTarget;

        if graph.contains(borrowed_target) {
            wohpe::trace!("{} was already present", target.name());
            return Ok(());
        }

        let should_build = target
            .should_build(project_root, build_root, profile, arch, test)
            .map_err(|err| EvalError::Target {
                name: target.name.to_string(),
                err,
            })?;

        wohpe::debug!(
            "Target '{}' inserted, should build: {should_build:?}",
            target.name()
        );

        if graph
            .insert_weight(
                borrowed_target.to_owned(),
                TargetStatus {
                    up_to_date: !should_build,
                },
            )
            .is_none()
        {
            panic!("Failed to insert {borrowed_target:?}");
        };

        let mut deps_up_to_date = true;
        for dep in target.dependencies.iter() {
            // Dependencies are never tests
            insert_target(
                graph,
                project_root,
                build_root,
                profile,
                dep.borrow(),
                arch,
                false,
            )?;

            let dep_arch = dep.inferred_arch(arch);
            let borrowed_dep = &BorrowedRealizedTarget {
                arch: dep_arch,
                target: dep.borrow(),
                test: false,
            } as &dyn BorrowRealizedTarget;

            if !graph
                .get_weight(graph.get_ref(borrowed_dep))
                .unwrap()
                .up_to_date
            {
                deps_up_to_date = false
            }

            graph.link_ref(borrowed_target, borrowed_dep);
        }

        if !deps_up_to_date {
            wohpe::debug!("Target '{}' has stale dependencies", target.name());
            graph.set_weight_ref(borrowed_target, TargetStatus { up_to_date: false });
        }

        Ok(())
    }

    for request in targets {
        insert_target(
            &mut target_graph,
            project_root,
            build_root,
            profile,
            request.target,
            TargetArch::Native,
            request.test,
        )?;
    }

    Ok(arachne::reversed(&target_graph))
}

#[derive(Debug)]
pub enum TargetError {
    Spawn { exe: String, err: std::io::Error },
    Wait(std::io::Error),
    ReadFile { path: PathBuf, err: std::io::Error },
    BuildFailure,
    Unconstrained,
}

impl std::fmt::Display for TargetError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TargetError::Spawn { exe, .. } => write!(f, "Error running `{exe}`"),
            TargetError::Wait(_) => write!(f, "Could not wait for process termination"),
            TargetError::BuildFailure => write!(f, "Build failure"),
            TargetError::ReadFile { path, .. } => {
                write!(f, "Could not read {}", path.to_string_lossy())
            }
            TargetError::Unconstrained => write!(f, "Target architecture was not constrained"),
        }
    }
}

impl std::error::Error for TargetError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            TargetError::Spawn { err, .. }
            | TargetError::Wait(err)
            | TargetError::ReadFile { err, .. } => Some(err),
            _ => None,
        }
    }
}

fn should_build_makefile(project_root: &Path, makefile: &Path) -> Result<bool, TargetError> {
    if !makefile.exists() {
        wohpe::trace!("Makefile {makefile:?} does not exist");
        return Ok(true);
    }

    let data = std::fs::read_to_string(project_root.join(makefile)).map_err(|err| {
        TargetError::ReadFile {
            path: makefile.to_path_buf(),
            err,
        }
    })?;

    for line in data.lines() {
        if line.is_empty() || line.starts_with('#') {
            continue;
        }

        let Some((product, deps)) = line.split_once(':') else {
            eprintln!("Malformed makefile line: `{line}`");
            return Ok(true);
        };

        let Ok(product_meta) = Path::new(product).metadata() else {
            return Ok(true);
        };

        let product_modified = product_meta.modified().unwrap();

        for dep in deps.split_ascii_whitespace() {
            let Ok(dep_meta) = Path::new(dep).metadata() else {
                return Ok(true);
            };

            if dep_meta.modified().unwrap() > product_modified {
                return Ok(true);
            }
        }
    }

    // Nothing has been modified since the last time we built this
    Ok(false)
}

fn evaluate_crate(
    interpreter: &mut Interpreter,
    module_path: ItemPath,
    loc: Location,
    kind: TargetKind,
    args: &ast::Arguments,
) -> Result<Target, EvalError> {
    // Parse common argument
    let kind_str = match kind {
        TargetKind::Executable(exe) => match exe {
            ExecutableKind::Native => "executable",
            ExecutableKind::BareMetal => "bare-metal executable",
            ExecutableKind::Kernel => "kernel",
        },
        TargetKind::Library => "library",
        TargetKind::ProcMacro => "proc-macro",
        TargetKind::BareMetalLibrary => "bare-metal library",
        TargetKind::StandaloneTest => unreachable!(),
    };

    let mut name = None;
    let mut language = None;
    let mut dependencies = None;
    let mut root = None;
    let mut linker_script = None;
    let mut panic = None;

    for (arg, value) in args.iter() {
        match arg.v.as_str() {
            "name" => name = Some(interpreter.eval_string(value)?),
            "root" => root = Some(PathBuf::from(&*interpreter.eval_string(value)?)),
            "language" if kind != TargetKind::ProcMacro => {
                language = Some(
                    interpreter
                        .eval_string(value)?
                        .spanned(value.location.clone()),
                )
            }
            "panic" => panic = Some(interpreter.eval_string(value)?),
            "dependencies" => dependencies = Some(interpreter.eval_array(value)?),
            "linker_script" if matches!(kind, TargetKind::Executable(_)) => {
                linker_script = Some(PathBuf::from(&*interpreter.eval_string(value)?))
            }
            _ => {
                return Err(EvalError::UnsupportedArgument {
                    name: arg.v.clone(),
                    at: kind_str.into(),
                    location: arg.span(),
                });
            }
        }
    }

    macro_rules! required {
        ($name:ident) => {
            $name.ok_or_else(|| EvalError::MissingArgument {
                at: kind_str.to_owned(),
                name: stringify!($name).to_owned(),
                location: loc.clone(),
            })
        };
    }

    let name = required!(name)?;
    let root = required!(root)?;
    let dependencies = interpreter.evaluate_dependencies(dependencies)?;

    let language = if kind == TargetKind::ProcMacro {
        Language::Rust
    } else {
        let language = required!(language)?;
        match &*language.v {
            "rust" => Language::Rust,
            _ => {
                return Err(EvalError::InvalidLanguage {
                    language: language.v.to_string(),
                    location: language.span(),
                });
            }
        }
    };

    Ok(Target {
        panic,
        module_path,
        kind,
        name,
        language,
        dependencies,
        root,
        module: loc.source.path.clone().parent().unwrap().to_owned(),
        linker_script,
        definition: loc.source.path.clone(),
    })
}

fn evaluate_test(
    interpreter: &mut Interpreter,
    module_path: ItemPath,
    loc: Location,
    args: &ast::Arguments,
) -> Result<Target, EvalError> {
    let mut tested = None;
    let mut root = None;
    let mut dependencies = None;

    for (arg, value) in args.iter() {
        match arg.v.as_str() {
            "for" => tested = Some(interpreter.eval_target(value)?),
            "file" => root = Some(PathBuf::from(&*interpreter.eval_string(value)?)),
            "dependencies" => dependencies = Some(interpreter.eval_array(value)?),
            _ => {
                return Err(EvalError::UnsupportedArgument {
                    name: arg.v.clone(),
                    at: "test".into(),
                    location: arg.span(),
                });
            }
        }
    }

    macro_rules! required {
        ($name:ident) => {
            $name.ok_or_else(|| EvalError::MissingArgument {
                at: "test".into(),
                name: stringify!($name).to_owned(),
                location: loc.clone(),
            })
        };
    }

    let tested = required!(tested)?;
    let root = required!(root)?;
    let language = tested.language;
    let name = format!(
        "{}_test_{}",
        tested.name,
        root.file_stem().unwrap().to_string_lossy()
    )
    .into();

    let mut dependencies = interpreter.evaluate_dependencies(dependencies)?.to_vec();
    dependencies.push(tested);

    Ok(Target {
        panic: None,
        kind: TargetKind::StandaloneTest,
        name,
        root,
        language,
        dependencies: dependencies.into(),
        module: loc.source.path.clone().parent().unwrap().to_owned(),
        module_path,
        linker_script: None,
        definition: loc.source.path.clone(),
    })
}

impl Target {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn dependencies(&self) -> &[Rc<Self>] {
        &self.dependencies
    }

    pub fn field(
        &self,
        root: &Path,
        release: bool,
        field: &str,
    ) -> Result<Option<super::Value>, TargetError> {
        Ok(match field {
            "output" => Some(super::Value::String(
                self.build_output(
                    root,
                    if release {
                        Profile::Release
                    } else {
                        Profile::Debug
                    },
                    self.arch().ok_or(TargetError::Unconstrained)?,
                )
                .to_string_lossy()
                .into(),
            )),
            _ => None,
        })
    }

    fn arch(&self) -> Option<TargetArch> {
        match self.kind {
            TargetKind::Executable(ExecutableKind::Native) | TargetKind::ProcMacro => {
                Some(TargetArch::Native)
            }
            TargetKind::Executable(ExecutableKind::BareMetal | ExecutableKind::Kernel)
            | TargetKind::BareMetalLibrary => Some(TargetArch::BareRV64),
            _ => None,
        }
    }

    fn inferred_arch(&self, parent_arch: TargetArch) -> TargetArch {
        self.arch().unwrap_or(parent_arch)
    }

    fn should_build(
        &self,
        project_root: &Path,
        build_root: &Path,
        profile: Profile,
        arch: TargetArch,
        test: bool,
    ) -> Result<bool, TargetError> {
        let name = match (test, self.kind) {
            (false, _) | (_, TargetKind::StandaloneTest) => self.name.to_string(),
            (true, _) => format!("{}__vvk-test", self.name),
        };

        wohpe::trace!("Checking if {name} should be built");

        if should_build_makefile(
            project_root,
            &self
                .build_dir(build_root, profile, arch)
                .join(format!("{name}.d")),
        )? {
            wohpe::trace!("Makefile for {name} is not up to date");
            return Ok(true);
        }

        // If we are here then the makefile saild that we did not have to build, in particular this
        // means that inputs tracked by rustc are ok

        let build_meta = self
            .build_output(build_root, profile, arch)
            .metadata()
            .unwrap();

        if let Some(link) = self.linker_script_path(project_root) {
            let link_meta = link.metadata().unwrap();
            if link_meta.modified().unwrap() > build_meta.modified().unwrap() {
                wohpe::trace!("Linker script for {name} is not up to date");
                return Ok(true);
            }
        }

        Ok(false)
    }

    pub fn evaluate(
        interpreter: &mut Interpreter,
        loc: Location,
        module_path: ItemPath,
        kind: TargetKind,
        args: &ast::Arguments,
    ) -> Result<Self, EvalError> {
        match kind {
            TargetKind::Executable(_)
            | TargetKind::BareMetalLibrary
            | TargetKind::Library
            | TargetKind::ProcMacro => evaluate_crate(interpreter, module_path, loc, kind, args),
            TargetKind::StandaloneTest => evaluate_test(interpreter, module_path, loc, args),
        }
    }

    fn build_dir(&self, root: &Path, profile: Profile, arch: TargetArch) -> PathBuf {
        root.join(match profile {
            Profile::Release => "release",
            Profile::Debug => "debug",
            Profile::Check => "clippy",
        })
        .join(arch.as_str())
        .join(&self.module)
    }

    fn build_output(&self, root: &Path, profile: Profile, arch: TargetArch) -> PathBuf {
        let build_dir = self.build_dir(root, profile, arch);
        let file = match self.kind {
            TargetKind::Executable(_) | TargetKind::StandaloneTest => self.name.to_string(),
            // TargetKind::Test => format!("{}__vvk-test", self.name),
            TargetKind::Library | TargetKind::BareMetalLibrary => format!("lib{}.rlib", self.name),
            TargetKind::ProcMacro => format!("lib{}.so", self.name),
        };

        build_dir.join(file)
    }

    fn linker_script_path(&self, project_root: &Path) -> Option<PathBuf> {
        self.linker_script
            .as_ref()
            .map(|ls| project_root.join(&self.module).join(ls))
    }

    fn root_module(&self, project_root: &Path) -> PathBuf {
        project_root.join(&self.module).join(&self.root)
    }

    fn build_command(
        &self,
        project_root: &Path,
        build_root: &Path,
        profile: Profile,
        arch: TargetArch,
    ) -> Command {
        let crate_type = match self.kind {
            TargetKind::Executable(_) | TargetKind::StandaloneTest => "bin",
            TargetKind::Library | TargetKind::BareMetalLibrary => "lib",
            TargetKind::ProcMacro => "proc-macro",
        };

        // The build directory has already been created by the caller
        let module_build_dir = self.build_dir(build_root, profile, arch);

        let mut compiler = match profile {
            Profile::Check => std::process::Command::new("clippy-driver"),
            _ => std::process::Command::new("rustc"),
        };

        compiler
            .arg("--crate-name")
            .arg(&*self.name)
            .arg("--edition")
            .arg(EDITION)
            .arg("--emit=dep-info,link")
            .arg(self.root_module(project_root))
            .arg("--out-dir")
            .arg(&module_build_dir)
            .arg("--crate-type")
            .arg(crate_type)
            .arg("-g")
            .arg("--remap-path-prefix")
            .arg({
                let mut s = OsString::from(project_root);
                s.push("=");
                s
            });

        if let Some(panic) = &self.panic {
            compiler.arg("-C").arg(format!("panic={panic}"));
        }

        if let Some(link) = self.linker_script_path(project_root) {
            let mut s = OsString::from("link-arg=-T");
            s.push(link);

            compiler.arg("-C").arg(s);
        }

        match arch {
            TargetArch::Native => (),
            TargetArch::BareRV64 => {
                compiler.arg("--target").arg(BARE_RV64);
            }
        }

        if self.kind == TargetKind::StandaloneTest {
            compiler.arg("--test");
        }

        if matches!(profile, Profile::Release) {
            compiler
                .arg("-C")
                .arg("opt-level=3")
                .arg("-C")
                .arg("overflow-checks=no");
        }

        if self.kind == TargetKind::ProcMacro {
            compiler.arg("--extern").arg("proc_macro");
        }

        let mut dep_set = HashSet::<(_, &RcCmp<_>)>::new();

        for dep in self.dependencies.iter() {
            let mut arg = OsString::new();

            let dep_arch = dep.inferred_arch(arch);

            arg.push(&*dep.name);
            arg.push("=");
            arg.push(dep.build_output(build_root, profile, dep_arch));

            compiler.arg("--extern").arg(arg);

            dep_set.insert((dep_arch, dep.borrow()));
        }

        let mut current = dep_set.clone();
        loop {
            let mut new = HashSet::new();

            for (tgt_arch, tgt) in current.into_iter() {
                for dep in tgt.dependencies() {
                    let key = (dep.inferred_arch(tgt_arch), dep.borrow());
                    if !dep_set.contains(&key) {
                        new.insert(key);
                    }
                }
            }

            if new.is_empty() {
                break;
            } else {
                dep_set.extend(new.clone());
                current = new;
            }
        }

        for (dep_arch, dep) in dep_set {
            let mut spec = OsString::from("dependency=");
            spec.push(dep.build_dir(build_root, profile, dep_arch));

            compiler.arg("-L").arg(spec);
        }

        wohpe::debug!("Build command: {compiler:?}");

        compiler
    }

    fn check(
        &self,
        project_root: &Path,
        build_root: &Path,
        json: bool,
        arch: TargetArch,
    ) -> Result<(), TargetError> {
        eprintln!("Checking {} ({})", self.name, arch.as_str());

        assert!(matches!(self.language, Language::Rust));

        let mut rustc = self.build_command(project_root, build_root, Profile::Check, arch);
        if json {
            rustc
                .arg("--error-format=json")
                .arg("--json=diagnostic-rendered-ansi,artifacts,future-incompat");
        }
        let out = rustc
            .spawn()
            .map_err(|err| TargetError::Spawn {
                exe: rustc.get_program().to_string_lossy().to_string(),
                err,
            })?
            .wait()
            .map_err(TargetError::Wait)?;
        if !out.success() {
            return Err(TargetError::BuildFailure);
        }

        Ok(())
    }

    fn build(
        &self,
        project_root: &Path,
        build_root: &Path,
        profile: Profile,
        arch: TargetArch,
    ) -> Result<(), TargetError> {
        eprintln!("Building {} ({})", self.name, arch.as_str());

        assert!(matches!(self.language, Language::Rust));

        let mut rustc = self.build_command(project_root, build_root, profile, arch);
        let out = rustc
            .spawn()
            .map_err(|err| TargetError::Spawn {
                exe: rustc.get_program().to_string_lossy().to_string(),
                err,
            })?
            .wait()
            .map_err(TargetError::Wait)?;
        if !out.success() {
            return Err(TargetError::BuildFailure);
        }

        Ok(())
    }

    fn test(
        &self,
        project_root: &Path,
        build_root: &Path,
        profile: Profile,
        arch: TargetArch,
        up_to_date: bool,
    ) -> Result<PathBuf, TargetError> {
        let mut rustc = self.build_command(project_root, build_root, profile, arch);
        let test_path = match self.kind {
            TargetKind::StandaloneTest => {
                self.build_dir(build_root, profile, arch).join(&*self.name)
            }
            _ => {
                rustc
                    .arg("--test")
                    .arg("-C")
                    .arg("extra-filename=__vvk-test");

                self.build_dir(build_root, profile, arch)
                    .join(format!("{}__vvk-test", self.name))
            }
        };

        if up_to_date {
            return Ok(test_path);
        }

        eprintln!("Building {} (test, {})", self.name, arch.as_str());

        assert!(matches!(self.language, Language::Rust));

        let out = rustc
            .spawn()
            .map_err(|err| TargetError::Spawn {
                exe: rustc.get_program().to_string_lossy().to_string(),
                err,
            })?
            .wait()
            .map_err(TargetError::Wait)?;
        if !out.success() {
            return Err(TargetError::BuildFailure);
        }

        Ok(test_path)
    }

    pub fn executable_kind(&self) -> Result<ExecutableKind, EvalError> {
        match self.kind {
            TargetKind::Executable(exe) => Ok(exe),
            _ => Err(EvalError::NotABinary),
        }
    }

    pub fn get_runable(
        self: &Rc<Self>,
        mut implicit_deps: HashSet<RcCmp<Target>>,
        release: bool,
        project_root: &Path,
        build_root: &Path,
    ) -> Result<PathBuf, EvalError> {
        let exe = self.executable_kind()?;

        let arch = match exe {
            ExecutableKind::Native => TargetArch::Native,
            ExecutableKind::BareMetal | ExecutableKind::Kernel => TargetArch::BareRV64,
        };

        implicit_deps.insert(RcCmp(self.clone()));

        build_list(implicit_deps.iter(), project_root, build_root, release)?;

        let path = self
            .build_dir(
                build_root,
                if release {
                    Profile::Release
                } else {
                    Profile::Debug
                },
                arch,
            )
            .join(&*self.name);

        Ok(path)
    }
}

impl RealizedTarget {
    fn name(&self) -> String {
        match self.arch {
            TargetArch::Native => self.target.name().to_string(),
            TargetArch::BareRV64 => format!("{} (bare rv64)", self.target.name()),
        }
    }

    fn build(
        &self,
        project_root: &Path,
        build_root: &Path,
        profile: Profile,
    ) -> Result<(), TargetError> {
        self.target
            .build(project_root, build_root, profile, self.arch)
    }

    fn check(&self, project_root: &Path, build_root: &Path, json: bool) -> Result<(), TargetError> {
        self.target.check(project_root, build_root, json, self.arch)
    }

    fn test(
        &self,
        project_root: &Path,
        build_root: &Path,
        profile: Profile,
        up_to_date: bool,
    ) -> Result<PathBuf, TargetError> {
        self.target
            .test(project_root, build_root, profile, self.arch, up_to_date)
    }
}

pub fn build_list<'a, I>(
    targets: I,
    project_root: &Path,
    build_root: &Path,
    release: bool,
) -> Result<(), EvalError>
where
    I: Iterator<Item = &'a RcCmp<Target>>,
{
    let profile = if release {
        Profile::Release
    } else {
        Profile::Debug
    };

    let graph = build_target_graph(
        targets.map(TargetRequest::new),
        project_root,
        build_root,
        profile,
    )?;
    let sorted = arachne::topological(&graph);

    for (target, status) in sorted {
        wohpe::trace!("Building {}, {:?}", target.name(), status.unwrap());

        if status.unwrap().up_to_date {
            continue;
        }

        target
            .build(project_root, build_root, profile)
            .map_err(|err| EvalError::Target {
                name: target.name(),
                err,
            })?;
    }

    Ok(())
}

pub fn check_list<'a, I>(
    targets: I,
    project_root: &Path,
    build_root: &Path,
    json: bool,
) -> Result<(), EvalError>
where
    I: Iterator<Item = &'a RcCmp<Target>>,
{
    let targets: Vec<_> = targets
        .into_iter()
        .map(|b| RealizedTarget {
            arch: b.borrow().inferred_arch(TargetArch::Native),
            test: true,
            target: b.clone(),
        })
        .collect();

    let graph = build_target_graph(
        targets.iter().map(|v| TargetRequest::new(&v.target)),
        project_root,
        build_root,
        Profile::Check,
    )?;
    let sorted = arachne::topological(&graph);

    // Always check everything
    for (target, status) in sorted {
        if status.unwrap().up_to_date && !targets.contains(&target) {
            continue;
        }

        if let Err(e) =
            target
                .check(project_root, build_root, json)
                .map_err(|err| EvalError::Target {
                    name: target.name(),
                    err,
                })
        {
            eprintln!("Failure of {}: {}", target.name(), e);
        }
    }

    Ok(())
}

pub fn test_list<'a, I>(
    targets: I,
    build_targets: HashSet<RcCmp<Target>>,
    project_root: PathBuf,
    build_root: PathBuf,
    release: bool,
) -> Result<impl Iterator<Item = Result<(ExecutableKind, String, PathBuf), EvalError>>, EvalError>
where
    I: Iterator<Item = &'a RcCmp<Target>>,
{
    let profile = if release {
        Profile::Release
    } else {
        Profile::Debug
    };

    let targets: Vec<_> = targets
        .into_iter()
        .map(|b| RealizedTarget {
            arch: b.borrow().inferred_arch(TargetArch::Native),
            test: true,
            target: b.clone(),
        })
        .collect();
    let graph = build_target_graph(
        targets
            .iter()
            .map(|v| TargetRequest::test(&v.target))
            .chain(build_targets.iter().map(TargetRequest::new)),
        &project_root,
        &build_root,
        profile,
    )?;
    let sorted = arachne::topological(&graph)
        .into_iter()
        .map(|(s, status)| ((*s).clone(), *status.unwrap()))
        .collect::<Vec<_>>();

    Ok(sorted.into_iter().filter_map(move |(target, status)| {
        let w = graph.get_ref(&target);

        if graph.neighbours(w).count() > 0 && !status.up_to_date {
            // Execute the build step if it has dependencies
            if let Err(e) = target
                .build(&project_root, &build_root, profile)
                .map_err(|err| EvalError::Target {
                    name: target.name(),
                    err,
                })
            {
                return Some(Err(e));
            }
        }

        if targets.contains(&target) {
            let command = match target
                .test(&project_root, &build_root, profile, status.up_to_date)
                .map_err(|err| EvalError::Target {
                    name: target.name(),
                    err,
                }) {
                Ok(c) => c,
                Err(e) => return Some(Err(e)),
            };

            let kind = match target.target.kind {
                TargetKind::Executable(executable_kind) => executable_kind,
                TargetKind::Library => match target.arch {
                    TargetArch::Native => ExecutableKind::Native,
                    TargetArch::BareRV64 => {
                        unimplemented!("Cross library tests are not yet implemented")
                    }
                },
                TargetKind::ProcMacro => ExecutableKind::Native,
                TargetKind::StandaloneTest => ExecutableKind::Native,
                TargetKind::BareMetalLibrary => {
                    unimplemented!("Cross library tests are not yet implemented")
                }
            };

            Some(Ok((kind, target.name().to_string(), command)))
        } else {
            None
        }
    }))
}

pub fn generate_project_json<'a, I>(
    targets: I,
    project_root: &Path,
    build_root: &Path,
) -> Result<(), EvalError>
where
    I: Iterator<Item = &'a RcCmp<Target>>,
{
    #[derive(Debug)]
    struct Crate {
        display_name: String,
        root_module: PathBuf,
        edition: &'static str,
        deps: Vec<Dep>,
        target: Option<&'static str>,
        is_proc_macro: bool,
        proc_macro_dylib_path: Option<PathBuf>,
        build_info: BuildInfo,
    }

    impl Crate {
        fn render_json(&self, into: &mut String) {
            use std::fmt::Write;

            *into += "{";
            write!(into, r#""display_name": "{}","#, self.display_name).unwrap();
            write!(
                into,
                r#""root_module": "{}","#,
                self.root_module.to_str().unwrap()
            )
            .unwrap();
            write!(into, r#""edition": "{}","#, self.edition).unwrap();
            write!(into, r#""deps": ["#).unwrap();
            let mut first = true;
            for dep in &self.deps {
                if !first {
                    *into += ","
                }
                dep.render_json(into);
                first = false;
            }
            write!(into, "],").unwrap();
            if let Some(target) = &self.target {
                write!(into, r#""target": "{target}","#).unwrap();
            }
            if self.is_proc_macro {
                write!(into, r#""is_proc_macro": true,"#).unwrap();
            }
            if let Some(p) = &self.proc_macro_dylib_path {
                write!(
                    into,
                    r#""proc_macro_dylib_path": "{}","#,
                    p.to_str().unwrap()
                )
                .unwrap();
            }
            *into += r#""build":"#;
            self.build_info.render_json(into);
            *into += "}";
        }
    }

    #[derive(Debug)]
    struct Dep {
        crate_index: usize,
        name: String,
    }

    impl Dep {
        fn render_json(&self, into: &mut String) {
            use std::fmt::Write;

            *into += "{";
            write!(into, r#""crate": {},"#, self.crate_index).unwrap();
            write!(into, r#""name": "{}""#, self.name).unwrap();
            *into += "}";
        }
    }

    #[derive(Debug)]
    struct BuildInfo {
        label: String,
        build_file: PathBuf,
        target_kind: &'static str,
    }

    impl BuildInfo {
        fn render_json(&self, into: &mut String) {
            use std::fmt::Write;

            *into += "{";
            write!(into, r#""label": "{}","#, self.label).unwrap();
            write!(
                into,
                r#""build_file": "{}","#,
                self.build_file.to_str().unwrap()
            )
            .unwrap();
            write!(into, r#""target_kind": "{}""#, self.target_kind).unwrap();
            *into += "}";
        }
    }

    let graph = build_target_graph(
        targets.map(TargetRequest::new),
        project_root,
        build_root,
        Profile::Debug,
    )?;

    let dependency_graph = arachne::reversed(&graph);
    let nodes: Vec<_> = dependency_graph.nodes().collect();

    let mut crates = Vec::with_capacity(nodes.len());

    for &target in &nodes {
        crates.push(Crate {
            display_name: target.name(),
            root_module: target.target.root_module(project_root),
            edition: EDITION,
            deps: dependency_graph
                .neighbours(target)
                .map(|t| {
                    nodes
                        .iter()
                        .enumerate()
                        .find_map(|(i, &o)| {
                            (o == t).then(|| Dep {
                                crate_index: i,
                                name: t.target.name().to_string(),
                            })
                        })
                        .expect("Dependency was not found")
                })
                .collect(),
            target: match target.arch {
                TargetArch::Native => None,
                TargetArch::BareRV64 => Some(BARE_RV64),
            },
            is_proc_macro: target.target.kind == TargetKind::ProcMacro,
            proc_macro_dylib_path: match target.target.kind {
                TargetKind::ProcMacro => Some(target.target.build_output(
                    build_root,
                    Profile::Debug,
                    target.arch,
                )),
                _ => None,
            },
            build_info: BuildInfo {
                label: target
                    .target
                    .module_path
                    .to_string()
                    .strip_prefix("::")
                    .unwrap()
                    .replace("::", "/"),
                build_file: target.target.definition.clone(),
                target_kind: match target.target.kind {
                    TargetKind::Executable(_) => "bin",
                    TargetKind::Library | TargetKind::ProcMacro | TargetKind::BareMetalLibrary => {
                        "lib"
                    }
                    TargetKind::StandaloneTest => "test",
                },
            },
        })
    }

    let mut json = format!(
        r#"{{"sysroot_src": "{}/lib/rustlib/src/rust/library/", "crates":["#,
        std::str::from_utf8(
            &std::process::Command::new("rustc")
                .args(["--print", "sysroot"])
                .output()
                .map_err(EvalError::CreateProjectJson)?
                .stdout
        )
        .unwrap()
        .trim()
    );
    let mut first = true;
    for c in crates {
        if !first {
            json += ",";
        }
        c.render_json(&mut json);
        first = false;
    }
    json.write_fmt(format_args!(
        r#"],"runnables":[{{"program": "vvk", "args": ["check", "{{label}}", "--json"], "cwd": "{}", "kind": "flycheck"}}]}}"#,
        project_root.to_str().unwrap()
    )).unwrap();

    std::fs::write(build_root.join("rust-project.json"), json)
        .map_err(EvalError::CreateProjectJson)?;

    Ok(())
}
