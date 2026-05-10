use std::{
    borrow::Borrow,
    cell::RefCell,
    collections::{HashMap, HashSet},
    path::PathBuf,
    rc::Rc,
};

use crate::{
    Binary, RcCmp, RunableKind, Runnable,
    parser::{
        ast::{
            Arguments, Directive, ExecutableKind, Expression, ItemPath, Module, Statement,
            TargetExpr, TargetKind,
        },
        span::{Location, SpannedValue},
    },
    project::{LazyValue, Value, ValueInner, VariableTree, interpreter::target::TargetError},
};

mod target;

pub use target::{Target, TargetArch};

#[derive(Debug)]
pub enum EvalError {
    UnexpectedType {
        expected: String,
        got: String,
        location: Location,
    },
    UndefinedVariable {
        name: ItemPath,
        location: Location,
    },
    UnsupportedArgument {
        name: String,
        at: String,
        location: Location,
    },
    MissingArgument {
        name: String,
        at: String,
        location: Location,
    },
    InvalidLanguage {
        language: String,
        location: Location,
    },
    Target {
        name: String,
        err: TargetError,
    },
    NoMainTarget,
    MultipleMainTargets(Vec<String>),
    NoSuchBinary,
    NotABinary,
    CreateProjectJson(std::io::Error),
    MissingConfig(String),
    InvalidConfig {
        key: String,
        reason: String,
    },
    UnsupportedOperation {
        name: String,
        got: String,
        location: Location,
    },
    NoSuchField {
        source: String,
        field: Rc<str>,
        location: Location,
    },
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::UnexpectedType {
                expected,
                got,
                location,
            } => {
                write!(f, "Unexpected type, expected `{expected}` got `{got}`")?;

                writeln!(f, "\n")?;
                location.render_context(2, f)
            }
            EvalError::UndefinedVariable { name, location } => {
                write!(f, "Undefined variable {name}")?;

                writeln!(f, "\n")?;
                location.render_context(1, f)
            }
            EvalError::UnsupportedArgument { name, at, location } => {
                write!(f, "Unsupported argument {name} for {at}")?;

                writeln!(f, "\n")?;
                location.render_context(1, f)
            }
            EvalError::MissingArgument { name, at, location } => {
                write!(f, "Missing argument {name} for {at}")?;

                writeln!(f, "\n")?;
                location.render_context(1, f)
            }
            EvalError::InvalidLanguage { language, location } => {
                write!(f, "Invalid language {language}")?;

                writeln!(f, "\n")?;
                location.render_context(1, f)
            }
            EvalError::Target { name, .. } => write!(f, "Error evaluating target {name}"),
            EvalError::NoMainTarget => write!(f, "No main target found"),
            EvalError::MultipleMainTargets(m) => {
                write!(f, "Multiple main targets found: {}", m.join(", "))
            }
            EvalError::NoSuchBinary => write!(f, "The specified binary does not exist"),
            EvalError::NotABinary => write!(f, "The specified target is not a binary"),
            EvalError::CreateProjectJson(_) => write!(f, "Could not create rust-project.json"),
            EvalError::MissingConfig(key) => write!(f, "Missing configuration for `{key}`"),
            EvalError::InvalidConfig { key, reason } => {
                write!(f, "Invalid configuration for `{key}`: {reason}")
            }
            EvalError::UnsupportedOperation {
                name,
                got,
                location,
            } => {
                write!(f, "Unsupported operation `{name}` for type `{got}`")?;

                location.render_context(1, f)
            }
            EvalError::NoSuchField {
                source,
                field,
                location,
            } => {
                writeln!(f, "No field `{field}` on this expression of type {source}")?;

                location.render_context(1, f)
            }
        }
    }
}

impl std::error::Error for EvalError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            EvalError::Target { err, .. } => Some(err),
            EvalError::CreateProjectJson(err) => Some(err),
            _ => None,
        }
    }
}

pub struct Interpreter {
    variables: VariableTree,
    evaluated_targets: HashSet<RcCmp<Target>>,
    config: HashMap<Rc<str>, LazyValue>,
    targets: HashMap<Location, Rc<Target>>,
    build_root: PathBuf,
    project_root: PathBuf,
    release: bool,
}

pub struct TestRunnable {
    pub runnable: Runnable,
}

impl Interpreter {
    pub fn new(
        variables: VariableTree,
        config_definition: Option<Arguments>,
        release: bool,
        project_root: PathBuf,
        build_root: PathBuf,
    ) -> Self {
        let mut config = HashMap::new();
        for (arg, value) in config_definition.iter().flat_map(|arg| arg.iter()) {
            config.insert(arg.v.clone().into(), Rc::new(value.to_owned()).into());
        }

        Self {
            evaluated_targets: HashSet::new(),
            variables,
            config,
            release,
            build_root,
            project_root,
            targets: HashMap::new(),
        }
    }

    fn get_config_command(&mut self, cfg: &str) -> Result<Vec<Rc<str>>, EvalError> {
        let value = self
            .config
            .get(cfg)
            .ok_or_else(|| EvalError::MissingConfig(cfg.into()))?;

        let value = match self.eval_lazy(value.clone())? {
            Value::Array(a) => a,
            _ => {
                return Err(EvalError::InvalidConfig {
                    key: cfg.into(),
                    reason: "Value was not an array".into(),
                });
            }
        };

        let value = value
            .iter()
            .map(|v| match self.eval_lazy(v.clone())? {
                Value::String(s) => Ok(s),
                _ => Err(EvalError::InvalidConfig {
                    key: cfg.into(),
                    reason: "Value was not a string array".into(),
                }),
            })
            .collect::<Result<Vec<_>, _>>()?;

        if value.is_empty() {
            return Err(EvalError::InvalidConfig {
                key: cfg.into(),
                reason: "Runner must have at least one element".into(),
            });
        }

        if value.iter().find(|p| p.contains("{binary}")).is_none() {
            return Err(EvalError::InvalidConfig {
                key: cfg.into(),
                reason: "Runner must contain the {binary} substitution".into(),
            });
        }

        Ok(value)
    }

    fn bare_metal_runner(&mut self) -> Result<Vec<Rc<str>>, EvalError> {
        self.get_config_command("bare-metal-runner")
    }

    fn native_debugger_runner(&mut self) -> Result<Vec<Rc<str>>, EvalError> {
        self.get_config_command("debugger")
    }

    fn bare_metal_debugger(&mut self) -> Result<Vec<Rc<str>>, EvalError> {
        self.get_config_command("bare-metal-debugger")
    }

    fn kernel_runner(&mut self) -> Result<Vec<Rc<str>>, EvalError> {
        self.get_config_command("kernel-runner")
    }

    fn kernel_debugger(&mut self) -> Result<Vec<Rc<str>>, EvalError> {
        self.get_config_command("kernel-debugger")
    }

    fn evaluate_target(
        &mut self,
        loc: Location,
        module_path: &ItemPath,
        kind: TargetKind,
        args: &Arguments,
    ) -> Result<Rc<Target>, EvalError> {
        match self.targets.get(&loc) {
            None => {
                let target = Rc::new(Target::evaluate(
                    self,
                    loc.clone(),
                    module_path.clone(),
                    kind,
                    args,
                )?);
                self.targets.insert(loc, target.clone());
                Ok(target)
            }
            Some(v) => Ok(v.clone()),
        }
    }

    fn project_info(&mut self, info: &str, location: &Location) -> Result<LazyValue, EvalError> {
        match info {
            "root_path" => Ok(Value::String(self.project_root.to_string_lossy().into()).into()),
            _ => Err(EvalError::NoSuchField {
                source: "project-info".into(),
                field: "info".into(),
                location: location.clone(),
            }),
        }
    }

    fn eval_sum(
        &mut self,
        lhs: Value,
        lhs_loc: &Location,
        rhs: Value,
        rhs_loc: &Location,
    ) -> Result<LazyValue, EvalError> {
        match lhs {
            Value::Target(_) | Value::ProjectInfo | Value::Bool(_) => {
                Err(EvalError::UnsupportedOperation {
                    name: "+".into(),
                    got: lhs.type_name().to_string(),
                    location: lhs_loc.clone(),
                })
            }
            Value::String(l) => match rhs {
                Value::String(r) => Ok(Value::String(format!("{l}{r}").into()).into()),
                v => Err(EvalError::UnexpectedType {
                    expected: "string".into(),
                    got: v.type_name().to_string(),
                    location: rhs_loc.clone(),
                }),
            },
            Value::Array(l) => match rhs {
                Value::Array(r) => {
                    Ok(Value::Array(l.iter().chain(r.iter()).cloned().collect()).into())
                }
                v => Err(EvalError::UnexpectedType {
                    expected: "array".into(),
                    got: v.type_name().to_string(),
                    location: rhs_loc.clone(),
                }),
            },
        }
    }

    fn eval_expr(&mut self, value: &SpannedValue<Expression>) -> Result<LazyValue, EvalError> {
        match &value.v {
            &Expression::Bool(b) => Ok(Value::Bool(b).into()),
            Expression::String(s) => Ok(Value::String(s.clone()).into()),
            Expression::Array(expressions) => {
                Ok(Value::Array(expressions.iter().cloned().map(Into::into).collect()).into())
            }
            Expression::Identifier(path) => {
                let (root, var) = path.parts();
                if root.is_empty() && var == "vvk" {
                    Ok(Value::ProjectInfo.into())
                } else {
                    self.variables
                        .get(path)
                        .ok_or_else(|| EvalError::UndefinedVariable {
                            name: path.clone(),
                            location: value.span(),
                        })
                }
            }
            Expression::Sum { lhs, rhs } => {
                let lhse = self.eval_expr(lhs)?;
                let lhse = self.eval_lazy(lhse)?;
                let rhse = self.eval_expr(rhs)?;
                let rhse = self.eval_lazy(rhse)?;

                self.eval_sum(lhse, &lhs.location, rhse, &rhs.location)
            }
            Expression::Field { source, field } => {
                let sourcee = self.eval_expr(source)?;
                let sourcee = self.eval_lazy(sourcee)?;

                match sourcee {
                    Value::Target(b) => Ok(LazyValue(Rc::new(RefCell::new(ValueInner::Target {
                        value: b
                            .field(&self.build_root, self.release, field)
                            .map_err(|err| EvalError::Target {
                                name: b.name().into(),
                                err,
                            })?
                            .ok_or_else(|| EvalError::NoSuchField {
                                source: "target".into(),
                                field: field.clone(),
                                location: value.location.clone(),
                            })?,
                        target: b,
                    })))),
                    Value::ProjectInfo => self.project_info(field, &value.location),
                    _ => Err(EvalError::NoSuchField {
                        field: field.clone(),
                        source: sourcee.type_name().to_string(),
                        location: value.location.clone(),
                    }),
                }
            }
            Expression::Target(TargetExpr {
                kind,
                args,
                module_path,
                ..
            }) => {
                Ok(
                    Value::Target(self.evaluate_target(value.span(), module_path, *kind, args)?)
                        .into(),
                )
            }
        }
    }

    fn eval_string(&mut self, value: &SpannedValue<Expression>) -> Result<Rc<str>, EvalError> {
        let eval = self.eval_expr(value)?;
        match self.eval_lazy(eval)? {
            Value::String(s) => Ok(s.clone()),
            v => Err(EvalError::UnexpectedType {
                expected: "string".into(),
                got: v.type_name().into(),
                location: value.span(),
            }),
        }
    }

    fn eval_bool(&mut self, value: &SpannedValue<Expression>) -> Result<bool, EvalError> {
        let eval = self.eval_expr(value)?;
        match self.eval_lazy(eval)? {
            Value::Bool(b) => Ok(b),
            v => Err(EvalError::UnexpectedType {
                expected: "array".into(),
                got: v.type_name().into(),
                location: value.span(),
            }),
        }
    }

    fn eval_array(
        &mut self,
        value: &SpannedValue<Expression>,
    ) -> Result<Rc<[LazyValue]>, EvalError> {
        let eval = self.eval_expr(value)?;
        match self.eval_lazy(eval)? {
            Value::Array(s) => Ok(s.clone()),
            v => Err(EvalError::UnexpectedType {
                expected: "array".into(),
                got: v.type_name().into(),
                location: value.span(),
            }),
        }
    }

    fn eval_lazy(&mut self, lazy: LazyValue) -> Result<Value, EvalError> {
        let realized = {
            let mut v = lazy.clone();
            loop {
                let guard = v.0.borrow_mut();
                match &*guard {
                    ValueInner::Realized(value) => break value.clone(),
                    ValueInner::Target { target, value } => {
                        self.evaluated_targets.insert(RcCmp(target.clone()));
                        break value.clone();
                    }
                    ValueInner::Lazy(expression) => {
                        let realized = self.eval_expr(expression)?;
                        drop(guard);
                        v = realized;
                    }
                }
            }
        };

        *lazy.0.borrow_mut() = ValueInner::Realized(realized.clone());
        Ok(realized)
    }

    fn eval_target(&mut self, value: &SpannedValue<Expression>) -> Result<Rc<Target>, EvalError> {
        let eval = self.eval_expr(value)?;
        match self.eval_lazy(eval)? {
            Value::Target(s) => Ok(s.clone()),
            v => Err(EvalError::UnexpectedType {
                expected: "array".into(),
                got: v.type_name().into(),
                location: value.span(),
            }),
        }
    }

    fn evaluate_dependencies(
        &mut self,
        dependencies: Option<Rc<[LazyValue]>>,
    ) -> Result<Rc<[Rc<Target>]>, EvalError> {
        dependencies
            .as_deref()
            .unwrap_or(&[])
            .iter()
            .cloned()
            .map(|v| self.eval_lazy(v).and_then(|v| v.to_target()))
            .collect()
    }

    pub fn walk_target_expr(
        &mut self,
        expr: &SpannedValue<Expression>,
        mut matching: impl FnMut(&TargetExpr) -> bool,
        mut cb: impl FnMut(Rc<Target>),
    ) -> Result<(), EvalError> {
        match &expr.v {
            Expression::Target(target_expr) if matching(target_expr) => {
                cb(self.evaluate_target(
                    expr.span(),
                    &target_expr.module_path,
                    target_expr.kind,
                    &target_expr.args,
                )?);
            }
            _ => (),
        }

        Ok(())
    }

    fn walk_targets(
        &mut self,
        module: &Module,
        matching: impl Fn(&TargetExpr) -> bool + Copy,
        mut visit: &mut dyn FnMut(Rc<Target>),
    ) -> Result<(), EvalError> {
        for statement in &module.statements {
            match &statement.v {
                Statement::Module(m) => {
                    self.walk_targets(&module.children[m], matching, &mut visit)?;
                }
                Statement::Assign { value, .. } => {
                    self.walk_target_expr(value, matching, &mut visit)?
                }
                Statement::Config(_) => (),
                Statement::Expr(expr) => match &expr.v {
                    Expression::Target { .. } => {
                        self.walk_target_expr(expr, matching, &mut visit)?
                    }
                    _ => {
                        eprintln!(
                            "Warning: evaluation is lazy, bare expression won’t be evaluated"
                        );
                    }
                },
            }
        }

        Ok(())
    }

    pub fn collect_targets(
        &mut self,
        module: &Module,
        targets: &mut HashSet<RcCmp<Target>>,
        matching: impl Fn(&TargetExpr) -> bool + Copy,
    ) -> Result<(), EvalError> {
        self.walk_targets(module, matching, &mut |t| {
            targets.insert(RcCmp(t));
        })
    }

    pub fn build_module(mut self, module: &Module, all: bool) -> Result<(), EvalError> {
        let mut targets = HashSet::new();
        self.collect_targets(module, &mut targets, |t| {
            t.directives.contains(&Directive::Default) || all
        })?;
        targets.extend(self.evaluated_targets.drain());

        target::scheduling::build_list(
            targets.iter(),
            &self.project_root,
            &self.build_root,
            self.release,
        )
    }

    pub fn check_module(
        mut self,
        module: &Module,
        only_default: bool,
        json: bool,
    ) -> Result<(), EvalError> {
        let mut targets = HashSet::new();
        self.collect_targets(module, &mut targets, |t| {
            if only_default {
                t.directives.contains(&Directive::Default)
            } else {
                true
            }
        })?;
        targets.extend(self.evaluated_targets.drain());

        target::scheduling::check_list(targets.iter(), &self.project_root, &self.build_root, json)
    }

    fn collect_tests(
        &mut self,
        module: &Module,
        recursive: bool,
    ) -> Result<HashSet<RcCmp<Target>>, EvalError> {
        let mut targets = HashSet::new();

        fn add_target(targets: &mut HashSet<RcCmp<Target>>, t: RcCmp<Target>, recursive: bool) {
            if recursive {
                for dep in t.dependencies() {
                    if !targets.contains(dep.borrow()) {
                        add_target(targets, RcCmp(dep.clone()), recursive);
                    }
                }
            }

            targets.insert(t);
        }

        self.walk_targets(
            module,
            |t| t.directives.contains(&Directive::Default) || t.kind == TargetKind::StandaloneTest,
            &mut |t| add_target(&mut targets, RcCmp(t), recursive),
        )?;

        if recursive {
            for target in self.evaluated_targets.drain() {
                add_target(&mut targets, target, recursive)
            }
        }

        Ok(targets)
    }

    pub fn test_module(
        mut self,
        module: &Module,
        recursive: bool,
        debug: bool,
    ) -> Result<Vec<TestRunnable>, EvalError> {
        let targets = self.collect_tests(module, recursive)?;

        let mut output = Vec::new();

        for test in target::scheduling::test_list(
            targets.iter(),
            std::mem::take(&mut self.evaluated_targets),
            self.project_root.clone(),
            self.build_root.clone(),
            self.release,
        )? {
            let test = test?;

            output.push(TestRunnable {
                runnable: Runnable {
                    name: format!("{} (test)", test.name),
                    binary: test.binary,
                    kind: self.runable_kind(test.kind, debug)?,
                },
            })
        }

        target::scheduling::build_list(
            self.evaluated_targets.iter(),
            &self.project_root,
            &self.build_root,
            self.release,
        )?;

        Ok(output)
    }

    pub fn list_tests(mut self, module: &Module, recursive: bool) -> Result<(), EvalError> {
        let targets = self.collect_tests(module, recursive);

        for target in targets? {
            println!(" - {}", target.name());
        }

        Ok(())
    }

    pub fn find_main_expr(
        &mut self,
        expr: &SpannedValue<Expression>,
    ) -> Result<Option<Rc<Target>>, EvalError> {
        match &expr.v {
            Expression::Target(target_expr)
                if target_expr.directives.contains(&Directive::Main) =>
            {
                Ok(Some(self.evaluate_target(
                    expr.span(),
                    &target_expr.module_path,
                    target_expr.kind,
                    &target_expr.args,
                )?))
            }
            _ => Ok(None),
        }
    }

    pub fn find_main_target(
        &mut self,
        module: &Module,
    ) -> Result<Vec<(ItemPath, Rc<Target>)>, EvalError> {
        let mut targets = Vec::new();

        for statement in &module.statements {
            match &statement.v {
                Statement::Config(_) => (),
                Statement::Module(_) => (),
                Statement::Assign { value, name } => {
                    if let Some(target) = self.find_main_expr(value)? {
                        targets.push((ItemPath::from(vec![name.clone()]), target));
                    }
                }
                Statement::Expr(expr) => match &expr.v {
                    Expression::Target { .. } => {
                        if let Some(target) = self.find_main_expr(expr)? {
                            targets.push((ItemPath::from(vec![target.name().to_string()]), target));
                        }
                    }
                    _ => {
                        eprintln!(
                            "Warning: evaluation is lazy, bare expression won’t be evaluated"
                        );
                    }
                },
            }
        }

        if targets.is_empty() {
            for (name, child) in &module.children {
                let child_path = ItemPath::from(vec![name.to_string()]);
                targets.extend(
                    self.find_main_target(child)?
                        .into_iter()
                        .map(|(p, t)| (child_path.append(p.into()), t)),
                );
            }
        }

        Ok(targets)
    }

    fn runable_kind(
        &mut self,
        executable: ExecutableKind,
        debug: bool,
    ) -> Result<RunableKind, EvalError> {
        Ok(match executable {
            ExecutableKind::Native => {
                if debug {
                    RunableKind::Runner(self.native_debugger_runner()?)
                } else {
                    RunableKind::Native
                }
            }
            ExecutableKind::BareMetal => {
                if debug {
                    RunableKind::Runner(self.bare_metal_debugger()?)
                } else {
                    RunableKind::Runner(self.bare_metal_runner()?)
                }
            }
            ExecutableKind::Kernel => {
                if debug {
                    RunableKind::Runner(self.kernel_debugger()?)
                } else {
                    RunableKind::Runner(self.kernel_runner()?)
                }
            }
        })
    }

    pub fn get_runnable_in(
        mut self,
        module: &Module,
        debug: bool,
        binary: Option<Binary>,
    ) -> Result<Runnable, EvalError> {
        let target = match binary {
            Some(bin) => {
                let path = match bin.relative {
                    true => module.path.append(bin.path),
                    false => bin.path.into(),
                };

                let value = self.variables.get(&path).ok_or(EvalError::NoSuchBinary)?;
                let value = self.eval_lazy(value)?;

                match value {
                    Value::Target(t) => t,
                    _ => return Err(EvalError::NotABinary),
                }
            }
            None => {
                let main_targets = self.find_main_target(module)?;

                if main_targets.is_empty() {
                    return Err(EvalError::NoMainTarget);
                }

                if main_targets.len() > 1 {
                    return Err(EvalError::MultipleMainTargets(
                        main_targets.iter().map(|(p, _)| p.to_string()).collect(),
                    ));
                }

                main_targets.into_iter().next().unwrap().1
            }
        };

        let kind = self.runable_kind(target.executable_kind()?, debug)?;

        let path = target.get_runable(
            self.evaluated_targets,
            self.release,
            &self.project_root,
            &self.build_root,
        )?;

        Ok(Runnable {
            name: target.name().to_string(),
            binary: path,
            kind,
        })
    }

    pub fn generate_info(&mut self, root: &Module) -> Result<(), EvalError> {
        let mut targets = self.evaluated_targets.clone();
        // Collect all the targets
        self.collect_targets(root, &mut targets, |_| true)?;

        target::scheduling::generate_project_json(
            targets.iter(),
            &self.project_root,
            &self.build_root,
        )
    }
}
