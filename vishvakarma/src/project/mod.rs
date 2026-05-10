use std::{cell::RefCell, collections::HashMap, ops::Deref, path::PathBuf, rc::Rc};

use crate::{
    Binary, Runnable,
    parser::{
        ast::{Arguments, Expression, ItemPath, Module, Statement},
        span::{Location, SpannedValue},
    },
    project::interpreter::{EvalError, Interpreter, Target, TargetArch},
};

mod interpreter;

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
    Bool(bool),
    String(Rc<str>),
    Array(Rc<[LazyValue]>),
    Target(Rc<Target>),
    ProjectInfo,
}

#[derive(Debug)]
enum ValueInner {
    Realized(Value),
    Lazy(Rc<SpannedValue<Expression>>),
    Target { target: Rc<Target>, value: Value },
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

impl Value {
    fn type_name(&self) -> &'static str {
        match self {
            Value::String(_) => "string",
            Value::Array(_) => "array",
            Value::Target(_) => "target",
            Value::ProjectInfo => "project-info",
            Value::Bool(_) => "bool",
        }
    }

    fn to_target(&self) -> Result<Rc<Target>, EvalError> {
        match self {
            Value::Target(target) => Ok(target.clone()),
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub enum ProjectError {
    Eval(EvalError),
    DuplicateVariable {
        name: String,
        redefinition: Location,
    },
    CreateBuildDir {
        path: PathBuf,
        err: std::io::Error,
    },
    TestFailure,
}

impl std::fmt::Display for ProjectError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eval(_) => write!(f, "Error while evaluating project"),
            Self::DuplicateVariable { name, redefinition } => {
                write!(f, "Duplicate definition {name}")?;

                writeln!(f, "\n")?;
                redefinition.render_context(1, f)
            }
            Self::CreateBuildDir { path, .. } => {
                write!(
                    f,
                    "Failed to create build directory at {}",
                    path.to_string_lossy()
                )
            }
            Self::TestFailure => write!(f, "Test failure"),
        }
    }
}

impl std::error::Error for ProjectError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Self::CreateBuildDir { err, .. } => Some(err),
            Self::Eval(e) => Some(e),
            _ => None,
        }
    }
}

impl From<EvalError> for ProjectError {
    fn from(value: EvalError) -> Self {
        Self::Eval(value)
    }
}

impl<'a> Project<'a> {
    pub fn load(
        root: &'a Module,
        project_root: PathBuf,
        build_root: PathBuf,
        release: bool,
    ) -> Result<Self, ProjectError> {
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

    fn load_module(&mut self, module: &'a Module) -> Result<(), ProjectError> {
        for statement in &module.statements {
            match &statement.v {
                Statement::Module(_) | Statement::Expr(_) => (),
                Statement::Config(arguments) => {
                    self.config = Some(arguments.clone());
                }
                Statement::Assign { name, value } => {
                    let full_name = module.path.join(name.clone());

                    if self.variables.get(&full_name).is_some() {
                        return Err(ProjectError::DuplicateVariable {
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

                std::fs::create_dir_all(&module_dir).map_err(|err| {
                    ProjectError::CreateBuildDir {
                        path: module_dir.clone(),
                        err,
                    }
                })?;
            }
        }

        for child in module.children.values() {
            self.load_module(child)?;
        }

        Ok(())
    }

    pub fn build(self, module: Option<PathBuf>, all: bool) -> Result<(), ProjectError> {
        let eval_root = self.root.get_descendent(module);

        let interpreter = Interpreter::new(
            self.variables,
            self.config,
            self.release,
            self.project_root,
            self.build_root,
        );

        interpreter.build_module(eval_root, all)?;

        Ok(())
    }

    pub fn generate_info(self) -> Result<(), ProjectError> {
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

    pub fn check(
        self,
        module: Option<PathBuf>,
        only_default: bool,
        json: bool,
    ) -> Result<(), ProjectError> {
        let eval_root = self.root.get_descendent(module);

        let interpreter = Interpreter::new(
            self.variables,
            self.config,
            self.release,
            self.project_root,
            self.build_root,
        );

        interpreter.check_module(eval_root, only_default, json)?;

        Ok(())
    }

    pub fn test(
        self,
        module: Option<PathBuf>,
        recursive: bool,
        list: bool,
        debug: bool,
    ) -> Result<(), ProjectError> {
        let eval_root = self.root.get_descendent(module);

        let interpreter = Interpreter::new(
            self.variables,
            self.config,
            self.release,
            self.project_root,
            self.build_root,
        );

        if list {
            interpreter.list_tests(eval_root, recursive)?;

            Ok(())
        } else {
            let tests = interpreter.test_module(eval_root, recursive, debug)?;

            let mut fail = false;

            for test in tests {
                eprintln!(
                    "Running test for {} ({})",
                    test.runnable.name,
                    test.runnable.binary.to_string_lossy()
                );

                let status = test.runnable.run(vec![]).unwrap();
                if !status.success() {
                    eprintln!("Failure of {}", test.runnable.name);
                    fail = true;
                }
            }

            if fail {
                Err(ProjectError::TestFailure)
            } else {
                Ok(())
            }
        }
    }

    pub fn get_runnable(
        self,
        debug: bool,
        module: Option<PathBuf>,
        binary: Option<Binary>,
    ) -> Result<Runnable, ProjectError> {
        let eval_root = self.root.get_descendent(module);

        let interpreter = Interpreter::new(
            self.variables,
            self.config,
            self.release,
            self.project_root,
            self.build_root,
        );

        Ok(interpreter.get_runnable_in(eval_root, debug, binary)?)
    }
}
