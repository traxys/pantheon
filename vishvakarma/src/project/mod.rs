use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    ops::Deref,
    path::PathBuf,
    rc::Rc,
};

use crate::{
    Binary, RcCmp, RunableKind, Runnable,
    parser::{
        ast::{
            self, Arguments, Directive, Expression, ItemPath, Module, Statement, TargetExpr,
            TargetKind,
        },
        span::{Location, SpannedValue},
    },
    project::target::TargetArch,
};

mod target;

use target::{Target, TargetError};

#[derive(Debug)]
struct VariableTree {
    values: HashMap<Rc<str>, LazyValue>,
    children: HashMap<Rc<str>, VariableTree>,
}

impl VariableTree {
    pub fn insert(&mut self, path: ItemPath, value: LazyValue) {
        let (modules, var) = path.parts();
        let mut this = self;
        for module in modules {
            this = this
                .children
                .entry(module.deref().into())
                .or_insert_with(VariableTree::new);
        }

        this.values.insert(var.into(), value);
    }

    pub fn get(&self, path: &ItemPath) -> Option<LazyValue> {
        let (modules, var) = path.parts();
        let mut this = self;
        for module in modules {
            this = this.children.get(&**module)?;
        }

        this.values.get(var).cloned()
    }

    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            children: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub struct Project<'a> {
    project_root: PathBuf,
    build_root: PathBuf,
    config: Option<Arguments>,
    root: &'a Module,
    variables: VariableTree,
    release: bool,
}

#[derive(Debug, Clone)]
enum Value {
    String(Rc<str>),
    Array(Rc<[LazyValue]>),
    Target(Rc<Target>),
}

#[derive(Debug)]
enum ValueInner {
    Realized(Value),
    Lazy(Rc<SpannedValue<Expression>>),
}

#[derive(Debug, Clone)]
struct LazyValue(Rc<RefCell<ValueInner>>);

impl From<Rc<SpannedValue<Expression>>> for LazyValue {
    fn from(value: Rc<SpannedValue<Expression>>) -> Self {
        Self(Rc::new(RefCell::new(ValueInner::Lazy(value))))
    }
}

impl From<Value> for LazyValue {
    fn from(value: Value) -> Self {
        Self(Rc::new(RefCell::new(ValueInner::Realized(value))))
    }
}

#[derive(Debug)]
pub enum EvalError {
    UnexpectedType {
        expected: String,
        got: String,
        location: Location,
    },
    DuplicateVariable {
        name: String,
        redefinition: Location,
    },
    CreateBuildDir {
        path: PathBuf,
        err: std::io::Error,
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
    TestFailure,
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
            EvalError::DuplicateVariable { name, redefinition } => {
                write!(f, "Duplicate definition {name}")?;

                writeln!(f, "\n")?;
                redefinition.render_context(1, f)
            }
            EvalError::CreateBuildDir { path, .. } => {
                write!(
                    f,
                    "Failed to create build directory at {}",
                    path.to_string_lossy()
                )
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
            EvalError::TestFailure => write!(f, "Test failure"),
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
            EvalError::NoSuchField { source, field, location } => {
                writeln!(f, "No field `{field}` on this expression of type {source}")?;

                location.render_context(1, f)
            },
        }
    }
}

impl std::error::Error for EvalError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            EvalError::CreateBuildDir { err, .. } => Some(err),
            EvalError::Target { err, .. } => Some(err),
            EvalError::CreateProjectJson(err) => Some(err),
            _ => None,
        }
    }
}

impl Value {
    fn type_name(&self) -> &'static str {
        match self {
            Value::String(_) => "string",
            Value::Array(_) => "array",
            Value::Target(_) => "target",
        }
    }

    fn to_target(&self) -> Result<Rc<Target>, EvalError> {
        match self {
            Value::Target(target) => Ok(target.clone()),
            _ => todo!(),
        }
    }
}

impl<'a> Project<'a> {
    pub fn load(
        root: &'a Module,
        project_root: PathBuf,
        build_root: PathBuf,
        release: bool,
    ) -> Result<Self, EvalError> {
        let build_root = build_root.canonicalize().unwrap();

        let mut this = Self {
            root,
            build_root,
            variables: VariableTree::new(),
            release,
            project_root,
            config: None,
        };

        this.load_module(this.root)?;

        Ok(this)
    }

    fn load_module(&mut self, module: &'a Module) -> Result<(), EvalError> {
        for statement in &module.statements {
            match &statement.v {
                Statement::Module(_) | Statement::Expr(_) => (),
                Statement::Config(arguments) => {
                    self.config = Some(arguments.clone());
                }
                Statement::Assign { name, value } => {
                    let full_name = module.path.join(name.clone());

                    if self.variables.get(&full_name).is_some() {
                        return Err(EvalError::DuplicateVariable {
                            name: name.clone(),
                            redefinition: statement.span(),
                        });
                    }

                    self.variables.insert(full_name, value.clone().into());
                }
            }
        }

        for profile in ["release", "debug", "clippy"] {
            for arch in [TargetArch::Native, TargetArch::BareRV64] {
                let module_dir = self
                    .build_root
                    .join(profile)
                    .join(arch.as_str())
                    .join(&module.location);

                std::fs::create_dir_all(&module_dir).map_err(|err| EvalError::CreateBuildDir {
                    path: module_dir.clone(),
                    err,
                })?;
            }
        }

        for child in module.children.values() {
            self.load_module(child)?;
        }

        Ok(())
    }

    pub fn build(self, module: Option<PathBuf>, all: bool) -> Result<(), EvalError> {
        let eval_root = self.root.get_descendent(module);

        let mut interpreter = Interpreter::new(
            self.variables,
            self.config,
            self.release,
            self.project_root,
            self.build_root,
        );

        interpreter.build_module(eval_root, all)?;

        Ok(())
    }

    pub fn generate_info(self) -> Result<(), EvalError> {
        let mut interpreter = Interpreter::new(
            self.variables,
            self.config,
            self.release,
            self.project_root,
            self.build_root,
        );

        interpreter.generate_info(self.root)?;

        Ok(())
    }

    pub fn check(self, module: Option<PathBuf>, all: bool, json: bool) -> Result<(), EvalError> {
        let eval_root = self.root.get_descendent(module);

        let mut interpreter = Interpreter::new(
            self.variables,
            self.config,
            self.release,
            self.project_root,
            self.build_root,
        );

        interpreter.check_module(eval_root, all, json)?;

        Ok(())
    }

    pub fn test(self, module: Option<PathBuf>, recursive: bool) -> Result<(), EvalError> {
        let eval_root = self.root.get_descendent(module);

        let mut interpreter = Interpreter::new(
            self.variables,
            self.config,
            self.release,
            self.project_root,
            self.build_root,
        );

        let tests = interpreter.test_module(eval_root, recursive)?;

        let mut fail = false;

        for test in tests {
            let test = test?;

            eprintln!(
                "Running test for {} ({})",
                test.name,
                test.binary.to_string_lossy()
            );

            let status = test.run(vec![]).unwrap();
            if !status.success() {
                eprintln!("Failure of {}", test.name);
                fail = true;
            }
        }

        if fail {
            Err(EvalError::TestFailure)
        } else {
            Ok(())
        }
    }

    pub fn get_runnable(
        self,
        debug: bool,
        module: Option<PathBuf>,
        binary: Option<Binary>,
    ) -> Result<Runnable, EvalError> {
        let eval_root = self.root.get_descendent(module);

        let mut interpreter = Interpreter::new(
            self.variables,
            self.config,
            self.release,
            self.project_root,
            self.build_root,
        );

        interpreter.get_runnable_in(eval_root, debug, binary)
    }
}

struct Interpreter {
    variables: VariableTree,
    config: HashMap<Rc<str>, LazyValue>,
    targets: HashMap<Location, Rc<Target>>,
    build_root: PathBuf,
    project_root: PathBuf,
    release: bool,
}

impl Interpreter {
    fn new(
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

        if value.iter().find(|p| &***p == "{binary}").is_none() {
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

    fn evaluate_target(
        &mut self,
        loc: Location,
        module_path: &ItemPath,
        kind: TargetKind,
        args: &ast::Arguments,
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

    fn eval_expr(&mut self, value: &SpannedValue<Expression>) -> Result<LazyValue, EvalError> {
        match &value.v {
            Expression::String(s) => Ok(Value::String(s.clone()).into()),
            Expression::Array(expressions) => {
                Ok(Value::Array(expressions.iter().cloned().map(Into::into).collect()).into())
            }
            Expression::Identifier(path) => {
                self.variables
                    .get(path)
                    .ok_or_else(|| EvalError::UndefinedVariable {
                        name: path.clone(),
                        location: value.span(),
                    })
            }
            Expression::Sum { lhs, rhs } => {
                let lhse = self.eval_expr(lhs)?;
                let lhse = self.eval_lazy(lhse)?;
                let rhse = self.eval_expr(rhs)?;
                let rhse = self.eval_lazy(rhse)?;

                match lhse {
                    Value::Target(_) => Err(EvalError::UnsupportedOperation {
                        name: "+".into(),
                        got: "target".into(),
                        location: lhs.location.clone(),
                    }),
                    Value::String(l) => match rhse {
                        Value::String(r) => Ok(Value::String(format!("{l}{r}").into()).into()),
                        v => Err(EvalError::UnexpectedType {
                            expected: "string".into(),
                            got: v.type_name().to_string(),
                            location: rhs.location.clone(),
                        }),
                    },
                    Value::Array(l) => match rhse {
                        Value::Array(r) => {
                            Ok(Value::Array(l.iter().chain(r.iter()).cloned().collect()).into())
                        }
                        v => Err(EvalError::UnexpectedType {
                            expected: "array".into(),
                            got: v.type_name().to_string(),
                            location: rhs.location.clone(),
                        }),
                    },
                }
            }
            Expression::Field { source, field } => {
                let sourcee = self.eval_expr(source)?;
                let sourcee = self.eval_lazy(sourcee)?;

                Err(EvalError::NoSuchField {
                    field: field.clone(),
                    source: sourcee.type_name().to_string(),
                    location: value.location.clone(),
                })
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

    pub fn collect_target_expr(
        &mut self,
        expr: &SpannedValue<Expression>,
        targets: &mut HashSet<RcCmp<Target>>,
        mut matching: impl FnMut(&TargetExpr) -> bool,
    ) -> Result<(), EvalError> {
        match &expr.v {
            Expression::Target(target_expr) if matching(target_expr) => {
                targets.insert(RcCmp(self.evaluate_target(
                    expr.span(),
                    &target_expr.module_path,
                    target_expr.kind,
                    &target_expr.args,
                )?));
            }
            _ => (),
        }

        Ok(())
    }

    pub fn collect_targets(
        &mut self,
        module: &Module,
        targets: &mut HashSet<RcCmp<Target>>,
        matching: impl Fn(&TargetExpr) -> bool + Copy,
    ) -> Result<(), EvalError> {
        for statement in &module.statements {
            match &statement.v {
                ast::Statement::Module(m) => {
                    self.collect_targets(&module.children[m], targets, matching)?;
                }
                ast::Statement::Assign { value, .. } => {
                    self.collect_target_expr(value, targets, matching)?
                }
                ast::Statement::Config(_) => (),
                ast::Statement::Expr(expr) => match &expr.v {
                    Expression::Target { .. } => {
                        self.collect_target_expr(expr, targets, matching)?
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

    pub fn build_module(&mut self, module: &Module, all: bool) -> Result<(), EvalError> {
        let mut targets = HashSet::new();
        self.collect_targets(module, &mut targets, |t| {
            t.directives.contains(&Directive::Default) || all
        })?;

        target::build_list(targets, &self.project_root, &self.build_root, self.release)
    }

    pub fn check_module(
        &mut self,
        module: &Module,
        all: bool,
        json: bool,
    ) -> Result<(), EvalError> {
        let mut targets = HashSet::new();
        self.collect_targets(module, &mut targets, |t| {
            t.directives.contains(&Directive::Default) || all
        })?;

        target::check_list(
            targets,
            &self.project_root,
            &self.build_root,
            module.path.clone(),
            json,
        )
    }

    pub fn test_module(
        &mut self,
        module: &Module,
        recursive: bool,
    ) -> Result<impl Iterator<Item = Result<Runnable, EvalError>>, EvalError> {
        let mut targets = HashSet::new();
        self.collect_targets(module, &mut targets, |t| {
            t.directives.contains(&Directive::Default) || t.kind == TargetKind::StandaloneTest
        })?;

        Ok(target::test_list(
            targets,
            self.project_root.clone(),
            self.build_root.clone(),
            recursive,
            self.release,
        )?
        .map(|res| {
            let (arch, name, path) = res?;

            let kind = match arch {
                TargetArch::Native => RunableKind::Native,
                TargetArch::BareRV64 => RunableKind::Runner(self.bare_metal_runner()?),
            };

            Ok(Runnable {
                name: format!("{name} (test)"),
                binary: path,
                kind,
            })
        }))
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
                ast::Statement::Config(_) => (),
                ast::Statement::Module(_) => (),
                ast::Statement::Assign { value, name } => {
                    if let Some(target) = self.find_main_expr(value)? {
                        targets.push((ItemPath::from(vec![name.clone()]), target));
                    }
                }
                ast::Statement::Expr(expr) => match &expr.v {
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

    pub fn get_runnable_in(
        &mut self,
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

        let (arch, path) =
            target.get_runable(self.release, &self.project_root, &self.build_root)?;

        let kind = match arch {
            TargetArch::Native => {
                if debug {
                    RunableKind::Runner(self.native_debugger_runner()?)
                } else {
                    RunableKind::Native
                }
            }
            TargetArch::BareRV64 => {
                if debug {
                    RunableKind::Runner(dbg!(self.bare_metal_debugger()?))
                } else {
                    RunableKind::Runner(self.bare_metal_runner()?)
                }
            }
        };

        Ok(Runnable {
            name: target.name().to_string(),
            binary: path,
            kind,
        })
    }

    pub fn generate_info(&mut self, root: &Module) -> Result<(), EvalError> {
        let mut targets = HashSet::new();
        // Collect all the targets
        self.collect_targets(root, &mut targets, |_| true)?;

        target::generate_project_json(targets, &self.project_root, &self.build_root)
    }
}
