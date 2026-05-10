use std::{
    borrow::Borrow,
    collections::HashSet,
    ffi::OsString,
    path::{Path, PathBuf},
    process::Command,
    rc::Rc,
};

pub mod scheduling;

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
struct BaseTarget {
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

#[derive(Debug)]
pub struct Target {
    base: BaseTarget,
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

    let base = BaseTarget {
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
    };

    Ok(Target { base })
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
    let language = tested.base.language;
    let name = format!(
        "{}_test_{}",
        tested.base.name,
        root.file_stem().unwrap().to_string_lossy()
    )
    .into();

    let mut dependencies = interpreter.evaluate_dependencies(dependencies)?.to_vec();
    dependencies.push(tested);

    let base = BaseTarget {
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
    };

    Ok(Target { base })
}

impl BaseTarget {
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

            let dep_arch = dep.base.inferred_arch(arch);

            arg.push(&*dep.base.name);
            arg.push("=");
            arg.push(dep.base.build_output(build_root, profile, dep_arch));

            compiler.arg("--extern").arg(arg);

            dep_set.insert((dep_arch, dep.borrow()));
        }

        let mut current = dep_set.clone();
        loop {
            let mut new = HashSet::new();

            for (tgt_arch, tgt) in current.into_iter() {
                for dep in tgt.dependencies() {
                    let key = (dep.base.inferred_arch(tgt_arch), dep.borrow());
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
            spec.push(dep.base.build_dir(build_root, profile, dep_arch));

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
}

impl Target {
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

    pub fn name(&self) -> &str {
        &self.base.name
    }

    pub fn dependencies(&self) -> &[Rc<Self>] {
        &self.base.dependencies
    }

    pub fn field(
        &self,
        root: &Path,
        release: bool,
        field: &str,
    ) -> Result<Option<super::Value>, TargetError> {
        Ok(match field {
            "output" => Some(super::Value::String(
                self.base
                    .build_output(
                        root,
                        if release {
                            Profile::Release
                        } else {
                            Profile::Debug
                        },
                        self.base.arch().ok_or(TargetError::Unconstrained)?,
                    )
                    .to_string_lossy()
                    .into(),
            )),
            _ => None,
        })
    }

    pub fn executable_kind(&self) -> Result<ExecutableKind, EvalError> {
        match self.base.kind {
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

        scheduling::build_list(implicit_deps.iter(), project_root, build_root, release)?;

        let path = self
            .base
            .build_dir(
                build_root,
                if release {
                    Profile::Release
                } else {
                    Profile::Debug
                },
                arch,
            )
            .join(&*self.base.name);

        Ok(path)
    }
}
