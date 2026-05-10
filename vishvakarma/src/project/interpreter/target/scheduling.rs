use std::{
    borrow::Borrow,
    collections::HashSet,
    fmt::Write,
    path::{Path, PathBuf},
};

use arachne::{Graph, MapGraph};
use wohpe_env::wohpe;

use crate::{
    RcCmp,
    parser::ast::{ExecutableKind, TargetKind},
    project::{
        EvalError,
        interpreter::target::{BARE_RV64, EDITION, Profile, Target, TargetArch, TargetError},
    },
};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
struct RealizedTarget {
    arch: TargetArch,
    test: bool,
    target: RcCmp<Target>,
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
            .base
            .build(project_root, build_root, profile, self.arch)
    }

    fn check(&self, project_root: &Path, build_root: &Path, json: bool) -> Result<(), TargetError> {
        self.target
            .base
            .check(project_root, build_root, json, self.arch)
    }

    fn test(
        &self,
        project_root: &Path,
        build_root: &Path,
        profile: Profile,
        up_to_date: bool,
    ) -> Result<PathBuf, TargetError> {
        self.target
            .base
            .test(project_root, build_root, profile, self.arch, up_to_date)
    }
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

        let arch = target.base.inferred_arch(parent_arch);
        let borrowed_target =
            &BorrowedRealizedTarget { arch, test, target } as &dyn BorrowRealizedTarget;

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

            let dep_arch = dep.base.inferred_arch(arch);
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
            arch: b.borrow().base.inferred_arch(TargetArch::Native),
            test: false,
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

pub struct Test {
    pub kind: ExecutableKind,
    pub name: String,
    pub binary: PathBuf,
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
            let binary = match target
                .test(&project_root, &build_root, profile, status.up_to_date)
                .map_err(|err| EvalError::Target {
                    name: target.name(),
                    err,
                }) {
                Ok(c) => c,
                Err(e) => return Some(Err(e)),
            };

            let kind = match target.target.base.kind {
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

            Some(Ok(Test {
                kind,
                name: target.name().to_string(),
                binary,
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
            root_module: target.target.base.root_module(project_root),
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
            is_proc_macro: target.target.base.kind == TargetKind::ProcMacro,
            proc_macro_dylib_path: match target.target.base.kind {
                TargetKind::ProcMacro => Some(target.target.base.build_output(
                    build_root,
                    Profile::Debug,
                    target.arch,
                )),
                _ => None,
            },
            build_info: BuildInfo {
                label: target
                    .target
                    .base
                    .module_path
                    .to_string()
                    .strip_prefix("::")
                    .unwrap()
                    .replace("::", "/"),
                build_file: target.target.base.definition.clone(),
                target_kind: match target.target.base.kind {
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
