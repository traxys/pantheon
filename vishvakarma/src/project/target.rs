use std::{
    borrow::Borrow,
    collections::HashSet,
    ffi::OsString,
    path::{Path, PathBuf},
    process::Command,
    rc::Rc,
};

use arachne::{Graph, MapGraph};

use crate::{
    RcCmp,
    parser::{
        ast::{self, TargetKind},
        span::{Location, Spanned},
    },
    project::{EvalError, Interpreter},
};

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

#[derive(Debug, Clone, Copy)]
enum Language {
    Rust,
}

#[derive(Debug)]
pub struct Target {
    name: Rc<str>,
    module: PathBuf,
    kind: TargetKind,
    language: Language,
    root: PathBuf,
    dependencies: Rc<[Rc<Target>]>,
    linker_script: Option<PathBuf>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
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

#[derive(Clone, Copy)]
struct TargetStatus {
    up_to_date: bool,
}

fn build_target_graph<I, B>(
    targets: I,
    project_root: &Path,
    build_root: &Path,
    release: bool,
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
        release: bool,
        target: &RcCmp<Target>,
        arch: TargetArch,
    ) -> Result<(), EvalError> {
        let arch = target.arch().unwrap_or(arch);

        let should_build = target
            .should_build(project_root, build_root, release, arch)
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
                insert_target(graph, project_root, build_root, release, dep.borrow(), arch)?;
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
                release,
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
        if line.is_empty() {
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
        TargetKind::Test => unreachable!(),
    };

    let mut name = None;
    let mut language = None;
    let mut dependencies = None;
    let mut root = None;
    let mut linker_script = None;

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
        kind,
        name,
        language,
        dependencies,
        root,
        module: loc.source.path.clone().parent().unwrap().to_owned(),
        linker_script,
    })
}

fn evaluate_test(
    interpreter: &mut Interpreter,
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
        kind: TargetKind::Test,
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
        linker_script: None,
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
        release: bool,
        arch: TargetArch,
    ) -> Result<bool, TargetError> {
        if should_build_makefile(
            project_root,
            &self
                .build_dir(build_root, release, arch)
                .join(format!("{}.d", self.name)),
        )? {
            return Ok(true);
        }

        // If we are here then the makefile saild that we did not have to build, in particular this
        // means that inputs tracked by rustc are ok

        let build_meta = self
            .build_output(build_root, release, arch)
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
        kind: TargetKind,
        args: &ast::Arguments,
    ) -> Result<Self, EvalError> {
        match kind {
            TargetKind::Executable
            | TargetKind::Library
            | TargetKind::ProcMacro
            | TargetKind::BareMetalBin => evaluate_crate(interpreter, loc, kind, args),
            TargetKind::Test => evaluate_test(interpreter, loc, args),
        }
    }

    fn build_dir(&self, root: &Path, release: bool, arch: TargetArch) -> PathBuf {
        root.join(if release { "release" } else { "debug" })
            .join(arch.as_str())
            .join(&self.module)
    }

    fn build_output(&self, root: &Path, release: bool, arch: TargetArch) -> PathBuf {
        let build_dir = self.build_dir(root, release, arch);
        let file = match self.kind {
            TargetKind::Executable | TargetKind::BareMetalBin => self.name.to_string(),
            TargetKind::Test => format!("{}__vvk-test", self.name),
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

    fn dep_paths(&self, build_root: &Path, release: bool, arch: TargetArch) -> HashSet<PathBuf> {
        self.dependencies
            .iter()
            .map(|v| v.build_dir(build_root, release, arch))
            .collect()
    }

    fn build_command(
        &self,
        project_root: &Path,
        build_root: &Path,
        release: bool,
        arch: TargetArch,
    ) -> Command {
        let crate_type = match self.kind {
            TargetKind::Executable | TargetKind::Test | TargetKind::BareMetalBin => "bin",
            TargetKind::Library => "lib",
            TargetKind::ProcMacro => "proc-macro",
        };

        // The build directory has already been created by the caller
        let module_build_dir = self.build_dir(build_root, release, arch);
        let root = project_root.join(&self.module).join(&self.root);

        let mut rustc = std::process::Command::new("rustc");
        rustc
            .arg("--crate-name")
            .arg(&*self.name)
            .arg("--edition=2024")
            .arg("--emit=dep-info,link")
            .arg(root)
            .arg("--out-dir")
            .arg(&module_build_dir)
            .arg("--crate-type")
            .arg(crate_type)
            .arg("--remap-path-prefix")
            .arg({
                let mut s = OsString::from(project_root);
                s.push("=");
                s
            });

        if let Some(link) = self.linker_script_path(project_root) {
            let mut s = OsString::from("link-arg=-T");
            s.push(link);

            rustc.arg("-C").arg(s);
        }

        match arch {
            TargetArch::Native => (),
            TargetArch::BareRV64 => {
                rustc.arg("--target").arg("riscv64gc-unknown-none-elf");
            }
        }

        if self.kind == TargetKind::Test {
            rustc.arg("--test");
        }

        if release {
            rustc
                .arg("-C")
                .arg("opt-level=3")
                .arg("-C")
                .arg("overflow-checks=no");
        }

        if self.kind == TargetKind::ProcMacro {
            rustc.arg("--extern").arg("proc_macro");
        }

        let mut dep_paths = HashSet::new();

        for dep in self.dependencies.iter() {
            let mut arg = OsString::new();

            arg.push(&*dep.name);
            arg.push("=");
            arg.push(dep.build_output(build_root, release, arch));

            rustc.arg("--extern").arg(arg);

            dep_paths.extend(dep.dep_paths(build_root, release, arch));
        }

        for dep_path in dep_paths {
            let mut spec = OsString::from("dependency=");
            spec.push(dep_path);

            rustc.arg("-L").arg(spec);
        }

        rustc
    }

    fn build(
        &self,
        project_root: &Path,
        build_root: &Path,
        release: bool,
        arch: TargetArch,
    ) -> Result<(), TargetError> {
        println!("Building {} ({})", self.name, arch.as_str());

        assert!(matches!(self.language, Language::Rust));

        let mut rustc = self.build_command(project_root, build_root, release, arch);
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
        release: bool,
        arch: TargetArch,
    ) -> Result<Command, TargetError> {
        println!("Building {} (test, {})", self.name, arch.as_str());

        assert!(matches!(self.language, Language::Rust));

        let mut rustc = self.build_command(project_root, build_root, release, arch);
        let command_path = match self.kind {
            TargetKind::Test => self.build_dir(build_root, release, arch).join(&*self.name),
            _ => {
                rustc
                    .arg("--test")
                    .arg("-C")
                    .arg("extra-filename=__vvk-test");

                self.build_dir(build_root, release, arch)
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

    pub fn run(
        self: &Rc<Self>,
        release: bool,
        project_root: &Path,
        build_root: &Path,
    ) -> Result<PathBuf, EvalError> {
        if self.kind != TargetKind::Executable {
            return Err(EvalError::NotABinary);
        }

        build_list(
            std::iter::once(self.borrow()),
            project_root,
            build_root,
            release,
        )?;

        Ok(self
            .build_dir(build_root, release, TargetArch::Native)
            .join(&*self.name))
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
        release: bool,
    ) -> Result<(), TargetError> {
        self.target
            .build(project_root, build_root, release, self.arch)
    }

    fn test(
        &self,
        project_root: &Path,
        build_root: &Path,
        release: bool,
    ) -> Result<Command, TargetError> {
        self.target
            .test(project_root, build_root, release, self.arch)
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
    let graph = build_target_graph(targets.into_iter(), project_root, build_root, release)?;
    let sorted = arachne::topological(&graph);

    for (target, status) in sorted {
        if status.unwrap().up_to_date {
            continue;
        }

        target
            .build(project_root, build_root, release)
            .map_err(|err| EvalError::Target {
                name: target.name(),
                err,
            })?;
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
        release,
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
                .build(project_root, build_root, release)
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
                .test(project_root, build_root, release)
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
