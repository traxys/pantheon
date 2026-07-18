use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    fmt::Write,
    path::{Path, PathBuf},
};

use arachne::{map::MapRef, Graph, MapGraph};
use wohpe_env::wohpe;

use crate::{
    parser::ast::{ExecutableKind, TargetKind},
    project::{
        interpreter::target::{
            FinalizedTarget, Profile, Target, TargetArch, TargetStatus, BARE_RV64, EDITION,
        },
        EvalError,
    },
    RcCmp,
};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub(super) struct RealizedTarget {
    pub arch: TargetArch,
    pub test: bool,
    pub target: RcCmp<Target>,
    pub profile: Profile,
}

impl RealizedTarget {
    pub fn name(&self) -> String {
        match self.arch {
            TargetArch::Native => format!("{} ({})", self.target.name(), self.profile),
            TargetArch::BareRV64 => format!("{} ({} bare rv64)", self.target.name(), self.profile),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
struct BorrowedRealizedTarget<'a> {
    arch: TargetArch,
    test: bool,
    target: &'a RcCmp<Target>,
    profile: Profile,
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
            profile: self.profile,
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
            profile: tgt.profile,
        }
    }
}

struct TargetRequest<'a> {
    target: &'a RcCmp<Target>,
    profile: Profile,
    test: bool,
}

impl<'a> TargetRequest<'a> {
    fn new(target: &'a RcCmp<Target>, profile: Profile) -> Self {
        TargetRequest {
            target,
            profile,
            test: false,
        }
    }

    fn test(target: &'a RcCmp<Target>, profile: Profile) -> Self {
        TargetRequest {
            target,
            profile,
            test: true,
        }
    }
}

fn build_target_graph<'a, I>(
    targets: I,
    project_root: &Path,
    build_root: &Path,
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

        let arch = target.base.inferred_arch(parent_arch);
        let borrowed_target = &BorrowedRealizedTarget {
            arch,
            test,
            target,
            profile,
        } as &dyn BorrowRealizedTarget;

        if graph.contains(borrowed_target) {
            wohpe::trace!("{} was already present", target.name());
            return Ok(());
        }

        let should_build = target
            .base
            .should_build(project_root, build_root, profile, arch, test)
            .map_err(|err| EvalError::Target {
                name: target.base.name.to_string(),
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
        for dep in target.dependencies().iter() {
            let dep_profile = match dep.base.kind {
                TargetKind::ProcMacro => Profile::Release,
                _ => match profile {
                    Profile::Check => Profile::Debug,
                    _ => profile,
                },
            };

            // Dependencies are never tests
            insert_target(
                graph,
                project_root,
                build_root,
                dep_profile,
                dep.borrow(),
                arch,
                false,
            )?;

            let dep_arch = dep.base.inferred_arch(arch);
            let borrowed_dep = &BorrowedRealizedTarget {
                arch: dep_arch,
                target: dep.borrow(),
                test: false,
                profile: dep_profile,
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
            request.profile,
            request.target,
            TargetArch::Native,
            request.test,
        )?;
    }

    Ok(target_graph)
}

fn build_target_list<'a, I>(
    targets: I,
    project_root: &Path,
    build_root: &Path,
) -> Result<Vec<FinalizedTarget>, EvalError>
where
    I: Iterator<Item = TargetRequest<'a>>,
{
    let graph = build_target_graph(targets, project_root, build_root)?;
    let rev = arachne::reversed(&graph);

    let mut memo = HashMap::new();

    fn finalize_target<'a>(
        target: MapRef<'a, RealizedTarget>,
        status: TargetStatus,
        graph: &'a MapGraph<RealizedTarget, TargetStatus>,
        memo: &mut HashMap<MapRef<'a, RealizedTarget>, FinalizedTarget>,
    ) -> FinalizedTarget {
        let out = match memo.get(&target) {
            Some(f) => f.clone(),
            _ => {
                let dependencies = graph
                    .neighbours(target)
                    .map(|d| finalize_target(d, *graph.get_weight(d).unwrap(), graph, memo))
                    .collect();

                let finalized = FinalizedTarget {
                    realization: (*target).clone(),
                    dependencies,
                    status,
                };

                memo.insert(target, finalized.clone());

                finalized
            }
        };

        assert_eq!(out.status, status);

        out
    }

    Ok(arachne::topological(&rev)
        .iter()
        .map(|(v, s)| finalize_target(*v, *s.unwrap(), &graph, &mut memo))
        .collect())
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

    let sorted = build_target_list(
        targets.map(|v| TargetRequest::new(v, profile)),
        project_root,
        build_root,
    )?;

    for target in sorted {
        wohpe::trace!(
            "Building {}, {:?}",
            target.realization.name(),
            target.status
        );

        if target.status.up_to_date {
            continue;
        }

        target
            .build(project_root, build_root)
            .map_err(|err| EvalError::Target {
                name: target.realization.name(),
                err,
            })?;
    }

    Ok(())
}

pub fn check_list<'a, I>(
    targets: I,
    infered_dependencies: HashSet<RcCmp<Target>>,
    project_root: &Path,
    build_root: &Path,
    json: bool,
) -> Result<(), EvalError>
where
    I: Iterator<Item = &'a RcCmp<Target>>,
{
    let sorted = build_target_list(
        targets
            .map(|v| TargetRequest::new(v, Profile::Check))
            .chain(
                infered_dependencies
                    .iter()
                    .map(|v| TargetRequest::new(v, Profile::Debug)),
            ),
        project_root,
        build_root,
    )?;

    // Always check everything
    for target in sorted {
        match target.realization.profile {
            Profile::Check => {
                wohpe::trace!(
                    "Checking {}, {:?}",
                    target.realization.name(),
                    target.status
                );

                if let Err(e) =
                    target
                        .check(project_root, build_root, json)
                        .map_err(|err| EvalError::Target {
                            name: target.realization.name(),
                            err,
                        })
                {
                    eprintln!("Failure of {}: {}", target.realization.name(), e);
                }
            }
            _ => {
                wohpe::trace!(
                    "Building {}, {:?}",
                    target.realization.name(),
                    target.status
                );

                if target.status.up_to_date {
                    continue;
                }

                target
                    .build(project_root, build_root)
                    .map_err(|err| EvalError::Target {
                        name: target.realization.name(),
                        err,
                    })?;
            }
        }
    }

    Ok(())
}

pub enum TestHarness {
    Rust,
    Custom,
}

pub struct Test {
    pub harness: TestHarness,
    pub kind: ExecutableKind,
    pub name: String,
    pub binary: PathBuf,
    pub parallel: Option<bool>,
}

pub fn test_list<'a, I>(
    targets: I,
    build_targets: HashSet<RcCmp<Target>>,
    project_root: PathBuf,
    build_root: PathBuf,
    release: bool,
) -> Result<impl Iterator<Item = Result<Test, EvalError>>, EvalError>
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
            arch: b.borrow().base.inferred_arch(TargetArch::Native),
            test: true,
            target: b.clone(),
            profile,
        })
        .collect();

    let sorted = build_target_list(
        targets
            .iter()
            .map(|v| TargetRequest::test(&v.target, profile))
            .chain(build_targets.iter().map(|v| TargetRequest::new(v, profile))),
        &project_root,
        &build_root,
    )?;

    Ok(sorted.into_iter().filter_map(move |target| {
        if !target.dependencies.is_empty() && !target.status.up_to_date {
            // Execute the build step if it has dependencies
            if let Err(e) =
                target
                    .build(&project_root, &build_root)
                    .map_err(|err| EvalError::Target {
                        name: target.realization.name(),
                        err,
                    })
            {
                return Some(Err(e));
            }
        }

        if targets.contains(&target.realization) {
            let binary =
                match target
                    .test(&project_root, &build_root)
                    .map_err(|err| EvalError::Target {
                        name: target.realization.name(),
                        err,
                    }) {
                    Ok(c) => c,
                    Err(e) => return Some(Err(e)),
                };

            let (kind, harness) = match target.realization.target.base.kind {
                TargetKind::Executable(executable_kind) => match executable_kind {
                    ExecutableKind::Native => (executable_kind, TestHarness::Rust),
                    ExecutableKind::BareMetal | ExecutableKind::Kernel => {
                        (executable_kind, TestHarness::Custom)
                    }
                },
                TargetKind::Library => match target.realization.arch {
                    TargetArch::Native => (ExecutableKind::Native, TestHarness::Rust),
                    TargetArch::BareRV64 => {
                        unimplemented!("Cross library tests are not yet implemented")
                    }
                },
                TargetKind::ProcMacro | TargetKind::StandaloneTest => {
                    (ExecutableKind::Native, TestHarness::Rust)
                }
                TargetKind::BareMetalLibrary => {
                    unimplemented!("Cross library tests are not yet implemented")
                }
            };

            Some(Ok(Test {
                harness,
                kind,
                name: target.realization.name().to_string(),
                binary,
                parallel: match target.realization.target.specific {
                    super::TargetImpl::Basic => None,
                    super::TargetImpl::Test { parallel } => Some(parallel),
                },
            }))
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

    let targets = build_target_list(
        targets.map(|v| TargetRequest::new(v, Profile::Debug)),
        project_root,
        build_root,
    )?;

    let mut crates = Vec::with_capacity(targets.len());

    for target in &targets {
        crates.push(Crate {
            display_name: target.realization.name(),
            root_module: target.realization.target.base.root_module(project_root),
            edition: EDITION,
            deps: target
                .dependencies
                .iter()
                .map(|t| {
                    targets
                        .iter()
                        .enumerate()
                        .find_map(|(i, o)| {
                            (o.realization.target == t.realization.target).then(|| Dep {
                                crate_index: i,
                                name: t.realization.target.name().to_string(),
                            })
                        })
                        .expect("Dependency was not found")
                })
                .collect(),
            target: match target.realization.arch {
                TargetArch::Native => None,
                TargetArch::BareRV64 => Some(BARE_RV64),
            },
            is_proc_macro: target.realization.target.base.kind == TargetKind::ProcMacro,
            proc_macro_dylib_path: match target.realization.target.base.kind {
                TargetKind::ProcMacro => Some(target.build_output(build_root)),
                _ => None,
            },
            build_info: BuildInfo {
                label: target
                    .realization
                    .target
                    .base
                    .module_path
                    .to_string()
                    .strip_prefix("::")
                    .unwrap()
                    .replace("::", "/"),
                build_file: target.realization.target.base.definition.clone(),
                target_kind: match target.realization.target.base.kind {
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
