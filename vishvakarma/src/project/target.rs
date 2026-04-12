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

use crate::{
    RcCmp, RunableKind, Runnable,
    parser::{
        ast::{self, ItemPath, TargetKind},
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
    target: RcCmp<Target>,
}

trait BorrowedRealizedTarget {
    fn as_borrow(&self) -> (TargetArch, &RcCmp<Target>);
}

impl BorrowedRealizedTarget for (TargetArch, &RcCmp<Target>) {
    fn as_borrow(&self) -> (TargetArch, &RcCmp<Target>) {
        (self.0, self.1)
    }
}

impl BorrowedRealizedTarget for RealizedTarget {
    fn as_borrow(&self) -> (TargetArch, &RcCmp<Target>) {
        (self.arch, &self.target)
    }
}

impl<'a> std::hash::Hash for dyn BorrowedRealizedTarget + 'a {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let (arch, target) = self.as_borrow();
        arch.hash(state);
        target.hash(state);
    }
}

impl<'a> Borrow<dyn 'a + BorrowedRealizedTarget> for RealizedTarget {
    fn borrow(&self) -> &(dyn 'a + BorrowedRealizedTarget) {
        self
    }
}

impl<'a> PartialEq for dyn BorrowedRealizedTarget + 'a {
    fn eq(&self, other: &Self) -> bool {
        self.as_borrow() == other.as_borrow()
    }
}
impl<'a> Eq for dyn BorrowedRealizedTarget + 'a {}

impl<'a> ToOwned for dyn BorrowedRealizedTarget + 'a {
    type Owned = RealizedTarget;

    fn to_owned(&self) -> Self::Owned {
        let (arch, target) = self.as_borrow();
        RealizedTarget {
            arch,
            target: target.clone(),
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct TargetStatus {
    up_to_date: bool,
}

fn build_target_graph<I, B>(
    targets: I,
    project_root: &Path,
    build_root: &Path,
    profile: Profile,
) -> Result<MapGraph<RealizedTarget, TargetStatus>, EvalError>
where
    I: Iterator<Item = B>,
    B: Borrow<RcCmp<Target>>,
{
    let mut target_graph = MapGraph::new_empty();

    fn insert_target(
        graph: &mut MapGraph<RealizedTarget, TargetStatus>,
        project_root: &Path,
        build_root: &Path,
        profile: Profile,
        target: &RcCmp<Target>,
        arch: TargetArch,
    ) -> Result<(), EvalError> {
        let arch = target.arch().unwrap_or(arch);

        let should_build = target
            .should_build(project_root, build_root, profile, arch)
            .map_err(|err| EvalError::Target {
                name: target.name.to_string(),
                err,
            })?;

        let borrowed_target = &(arch, target) as &dyn BorrowedRealizedTarget;
        graph.insert_weight(
            borrowed_target.to_owned(),
            TargetStatus {
                up_to_date: !should_build,
            },
        );

        let mut deps_up_to_date = true;
        for dep in target.dependencies.iter() {
            let borrowed_dep =
                &(dep.arch().unwrap_or(arch), dep.borrow()) as &dyn BorrowedRealizedTarget;

            if !graph.contains(borrowed_dep) {
                insert_target(graph, project_root, build_root, profile, dep.borrow(), arch)?;
            }

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
            graph.set_weight_ref(borrowed_target, TargetStatus { up_to_date: false });
        }

        Ok(())
    }

    for target in targets {
        if !target_graph
            .contains(&(TargetArch::Native, target.borrow()) as &dyn BorrowedRealizedTarget)
        {
            insert_target(
                &mut target_graph,
                project_root,
                build_root,
                profile,
                target.borrow(),
                target.borrow().arch().unwrap_or(TargetArch::Native),
            )?;
        }
    }

    Ok(arachne::reversed(&target_graph))
}

#[derive(Debug)]
pub enum TargetError {
    Spawn { exe: String, err: std::io::Error },
    Wait(std::io::Error),
    ReadFile { path: PathBuf, err: std::io::Error },
    BuildFailure,
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
        TargetKind::Executable => "executable",
        TargetKind::Library => "library",
        TargetKind::ProcMacro => "proc-macro",
        TargetKind::BareMetalBin => "bare-metal executable",
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
            "linker_script"
                if matches!(kind, TargetKind::Executable | TargetKind::BareMetalBin) =>
            {
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
    let dependencies = dependencies
        .as_deref()
        .unwrap_or(&[])
        .iter()
        .cloned()
        .map(|v| interpreter.eval_lazy(v).and_then(|v| v.to_target()))
        .collect::<Result<_, _>>()?;

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

    for (arg, value) in args.iter() {
        match arg.v.as_str() {
            "for" => tested = Some(interpreter.eval_target(value)?),
            "file" => root = Some(PathBuf::from(&*interpreter.eval_string(value)?)),
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

    Ok(Target {
        panic: None,
        kind: TargetKind::StandaloneTest,
        name: format!(
            "{}_test_{}",
            tested.name,
            root.file_stem().unwrap().to_string_lossy()
        )
        .into(),
        root,
        language: tested.language,
        dependencies: vec![tested].into(),
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

    pub fn arch(&self) -> Option<TargetArch> {
        match self.kind {
            TargetKind::Executable | TargetKind::ProcMacro => Some(TargetArch::Native),
            TargetKind::BareMetalBin => Some(TargetArch::BareRV64),
            _ => None,
        }
    }

    fn should_build(
        &self,
        project_root: &Path,
        build_root: &Path,
        profile: Profile,
        arch: TargetArch,
    ) -> Result<bool, TargetError> {
        if should_build_makefile(
            project_root,
            &self
                .build_dir(build_root, profile, arch)
                .join(format!("{}.d", self.name)),
        )? {
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
            TargetKind::Executable
            | TargetKind::Library
            | TargetKind::ProcMacro
            | TargetKind::BareMetalBin => evaluate_crate(interpreter, module_path, loc, kind, args),
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
            TargetKind::Executable | TargetKind::BareMetalBin | TargetKind::StandaloneTest => {
                self.name.to_string()
            }
            // TargetKind::Test => format!("{}__vvk-test", self.name),
            TargetKind::Library => format!("lib{}.rlib", self.name),
            TargetKind::ProcMacro => format!("lib{}.so", self.name),
        };

        build_dir.join(file)
    }

    fn linker_script_path(&self, project_root: &Path) -> Option<PathBuf> {
        self.linker_script
            .as_ref()
            .map(|ls| project_root.join(&self.module).join(ls))
    }

    fn dep_paths(&self, build_root: &Path, profile: Profile, arch: TargetArch) -> HashSet<PathBuf> {
        self.dependencies
            .iter()
            .map(|v| v.build_dir(build_root, profile, arch))
            .collect()
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
            TargetKind::Executable | TargetKind::BareMetalBin | TargetKind::StandaloneTest => "bin",
            TargetKind::Library => "lib",
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

        let mut dep_paths = HashSet::new();

        for dep in self.dependencies.iter() {
            let mut arg = OsString::new();

            arg.push(&*dep.name);
            arg.push("=");
            arg.push(dep.build_output(build_root, profile, arch));

            compiler.arg("--extern").arg(arg);

            dep_paths.extend(dep.dep_paths(build_root, profile, arch));
        }

        for dep_path in dep_paths {
            let mut spec = OsString::from("dependency=");
            spec.push(dep_path);

            compiler.arg("-L").arg(spec);
        }

        compiler
    }

    fn check(
        &self,
        project_root: &Path,
        build_root: &Path,
        json: bool,
        arch: TargetArch,
    ) -> Result<(), TargetError> {
        println!("Checking {} ({})", self.name, arch.as_str());

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
        println!("Building {} ({})", self.name, arch.as_str());

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
    ) -> Result<Command, TargetError> {
        println!("Building {} (test, {})", self.name, arch.as_str());

        assert!(matches!(self.language, Language::Rust));

        let mut rustc = self.build_command(project_root, build_root, profile, arch);
        let command_path = match self.kind {
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

        Ok(Command::new(command_path))
    }

    pub fn get_runable(
        self: &Rc<Self>,
        release: bool,
        project_root: &Path,
        build_root: &Path,
    ) -> Result<Runnable, EvalError> {
        let (arch, kind) = match self.kind {
            TargetKind::Executable => (TargetArch::Native, RunableKind::Native),
            TargetKind::BareMetalBin => (TargetArch::BareRV64, RunableKind::BareMetal),
            _ => return Err(EvalError::NotABinary),
        };

        build_list(
            std::iter::once(self.borrow()),
            project_root,
            build_root,
            release,
        )?;

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

        Ok(Runnable { binary: path, kind })
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
    ) -> Result<Command, TargetError> {
        self.target
            .test(project_root, build_root, profile, self.arch)
    }
}

pub fn build_list<I, B>(
    targets: I,
    project_root: &Path,
    build_root: &Path,
    release: bool,
) -> Result<(), EvalError>
where
    I: IntoIterator<Item = B>,
    B: Borrow<RcCmp<Target>>,
{
    let profile = if release {
        Profile::Release
    } else {
        Profile::Debug
    };

    let graph = build_target_graph(targets.into_iter(), project_root, build_root, profile)?;
    let sorted = arachne::topological(&graph);

    for (target, status) in sorted {
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

pub fn check_list<I, B>(
    targets: I,
    project_root: &Path,
    build_root: &Path,
    path: ItemPath,
    json: bool,
) -> Result<(), EvalError>
where
    I: IntoIterator<Item = B>,
    B: Borrow<RcCmp<Target>>,
{
    let graph = build_target_graph(
        targets.into_iter(),
        project_root,
        build_root,
        Profile::Check,
    )?;
    let sorted = arachne::topological(&graph);

    // Always check everything
    for (target, status) in sorted {
        if status.unwrap().up_to_date && !path.contains(&target.target.module_path) {
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

#[derive(Debug)]
pub struct Test {
    pub command: Command,
    pub name: String,
}

pub fn test_list<I, B>(
    targets: I,
    project_root: &Path,
    build_root: &Path,
    recursive: bool,
    release: bool,
) -> Result<impl Iterator<Item = Result<Test, EvalError>>, EvalError>
where
    I: IntoIterator<Item = B>,
    B: Borrow<RcCmp<Target>>,
{
    let profile = if release {
        Profile::Release
    } else {
        Profile::Debug
    };

    let targets: Vec<_> = targets
        .into_iter()
        .map(|b| RealizedTarget {
            arch: b.borrow().arch().unwrap_or(TargetArch::Native),
            target: b.borrow().clone(),
        })
        .collect();
    let graph = build_target_graph(
        targets.iter().map(|v| &v.target),
        project_root,
        build_root,
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
                .build(project_root, build_root, profile)
                .map_err(|err| EvalError::Target {
                    name: target.name(),
                    err,
                })
            {
                return Some(Err(e));
            }
        }

        if recursive || targets.contains(&target) {
            let command = match target
                .test(project_root, build_root, profile)
                .map_err(|err| EvalError::Target {
                    name: target.name(),
                    err,
                }) {
                Ok(c) => c,
                Err(e) => return Some(Err(e)),
            };

            Some(Ok(Test {
                command,
                name: target.name(),
            }))
        } else {
            None
        }
    }))
}

pub fn generate_project_json<I, B>(
    targets: I,
    project_root: &Path,
    build_root: &Path,
) -> Result<(), EvalError>
where
    I: IntoIterator<Item = B>,
    B: Borrow<RcCmp<Target>>,
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
        targets.into_iter(),
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
                    .to_owned(),
                build_file: target.target.definition.clone(),
                target_kind: match target.target.kind {
                    TargetKind::Executable | TargetKind::BareMetalBin => "bin",
                    TargetKind::Library | TargetKind::ProcMacro => "lib",
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
