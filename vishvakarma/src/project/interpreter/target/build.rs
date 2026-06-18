use std::{
    collections::HashSet,
    ffi::OsString,
    path::{Path, PathBuf},
    process::Command,
};

use wohpe_env::wohpe;

use crate::{
    parser::ast::TargetKind,
    project::interpreter::{
        target::{FinalizedTarget, Language, Profile, TargetError, BARE_RV64, EDITION},
        TargetArch,
    },
};

impl FinalizedTarget {
    fn build_dir(&self, build_root: &Path) -> PathBuf {
        self.realization.target.base.build_dir(
            build_root,
            self.realization.profile,
            self.realization.arch,
        )
    }

    fn build_output(&self, build_root: &Path) -> PathBuf {
        self.realization.target.base.build_output(
            build_root,
            self.realization.profile,
            self.realization.arch,
        )
    }

    fn build_command(&self, project_root: &Path, build_root: &Path) -> Command {
        let crate_type = match self.realization.target.base.kind {
            TargetKind::Executable(_) | TargetKind::StandaloneTest => "bin",
            TargetKind::Library | TargetKind::BareMetalLibrary => "lib",
            TargetKind::ProcMacro => "proc-macro",
        };

        // The build directory has already been created by the caller
        let module_build_dir = self.build_dir(build_root);

        let mut compiler = match self.realization.profile {
            Profile::Check => std::process::Command::new("clippy-driver"),
            _ => std::process::Command::new("rustc"),
        };

        compiler
            .arg("--crate-name")
            .arg(&*self.realization.target.base.name)
            .arg("--edition")
            .arg(EDITION)
            .arg("--emit=dep-info,link")
            .arg(self.realization.target.base.root_module(project_root))
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

        if let Some(panic) = &self.realization.target.base.panic {
            compiler.arg("-C").arg(format!("panic={panic}"));
        }

        if let Some(link) = self
            .realization
            .target
            .base
            .linker_script_path(project_root)
        {
            let mut s = OsString::from("link-arg=-T");
            s.push(link);

            compiler.arg("-C").arg(s);
        }

        match self.realization.arch {
            TargetArch::Native => (),
            TargetArch::BareRV64 => {
                compiler.arg("--target").arg(BARE_RV64);
            }
        }

        if self.realization.target.base.kind == TargetKind::StandaloneTest {
            compiler.arg("--test");
        }

        if matches!(self.realization.profile, Profile::Release) {
            compiler
                .arg("-C")
                .arg("opt-level=3")
                .arg("-C")
                .arg("overflow-checks=no");
        }

        if self.realization.target.base.kind == TargetKind::ProcMacro {
            compiler.arg("--extern").arg("proc_macro");
        }

        let mut dep_set = HashSet::new();

        for dep in self.dependencies.iter() {
            let mut arg = OsString::new();

            arg.push(&*dep.realization.target.base.name);
            arg.push("=");
            arg.push(dep.build_output(build_root));

            compiler.arg("--extern").arg(arg);

            dep_set.insert(dep);
        }

        let mut current = dep_set.clone();
        loop {
            let mut new = HashSet::new();

            for tgt in current.into_iter() {
                for dep in tgt.dependencies.iter() {
                    if !dep_set.contains(&dep) {
                        new.insert(dep);
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

        for dep in dep_set {
            let mut spec = OsString::from("dependency=");
            spec.push(dep.build_dir(build_root));

            compiler.arg("-L").arg(spec);
        }

        wohpe::debug!("Build command: {compiler:?}");

        compiler
    }

    pub fn build(&self, project_root: &Path, build_root: &Path) -> Result<(), TargetError> {
        eprintln!("Building {}", self.realization.name());

        assert!(matches!(
            self.realization.target.base.language,
            Language::Rust
        ));

        let mut rustc = self.build_command(project_root, build_root);
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

    pub fn check(
        &self,
        project_root: &Path,
        build_root: &Path,
        json: bool,
    ) -> Result<(), TargetError> {
        eprintln!("Checking {}", self.realization.name());

        assert!(matches!(
            self.realization.target.base.language,
            Language::Rust
        ));

        assert_eq!(self.realization.profile, Profile::Check);

        let mut rustc = self.build_command(project_root, build_root);
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

    pub fn test(&self, project_root: &Path, build_root: &Path) -> Result<PathBuf, TargetError> {
        let mut rustc = self.build_command(project_root, build_root);
        let test_path = match self.realization.target.base.kind {
            TargetKind::StandaloneTest => self
                .build_dir(build_root)
                .join(&*self.realization.target.base.name),
            _ => {
                rustc
                    .arg("--test")
                    .arg("-C")
                    .arg("extra-filename=__vvk-test");

                self.build_dir(build_root)
                    .join(format!("{}__vvk-test", self.realization.target.base.name))
            }
        };

        if self.status.up_to_date {
            return Ok(test_path);
        }

        eprintln!("Building {} (test)", self.realization.name());

        assert!(matches!(
            self.realization.target.base.language,
            Language::Rust
        ));

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
