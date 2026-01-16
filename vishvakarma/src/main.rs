use std::{
    alloc::Layout,
    borrow::Borrow,
    hash::Hash,
    io,
    ops::Deref,
    os::unix::process::CommandExt,
    path::{Path, PathBuf},
    rc::Rc,
    str::FromStr,
};

use sheshat::{CommandName, Sheshat, SheshatMetadataHandler, SheshatSubCommand, Void};

use crate::{
    parser::ast::{Module, ParseError},
    project::EvalError,
};

mod parser;
mod project;

#[derive(Sheshat)]
struct Vvk {
    #[sheshat(short, long)]
    root: Option<PathBuf>,
    #[sheshat(long)]
    release: bool,
    #[sheshat(subcommand)]
    command: Commands,
}

#[derive(SheshatSubCommand)]
enum Commands {
    Run(Run),
    Test(Test),
    Build(Build),
}

struct Binary {
    relative: bool,
    path: Vec<String>,
}

impl FromStr for Binary {
    type Err = Void;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (p, relative) = match s.strip_prefix("::") {
            Some(s) => (s, false),
            None => (s, true),
        };

        Ok(Binary {
            relative,
            path: p.split("::").map(ToString::to_string).collect(),
        })
    }
}

#[derive(Sheshat)]
struct Run {
    #[sheshat(short, long)]
    binary: Option<Binary>,
    extra_args: Vec<String>,
}

#[derive(Sheshat)]
struct Test {
    #[sheshat(short, long)]
    recursive: bool,
    path: Option<PathBuf>,
}

#[derive(Sheshat)]
struct Build {
    #[sheshat(short, long)]
    all: bool,
    path: Option<PathBuf>,
}

#[derive(Debug)]
#[repr(transparent)]
struct RcCmp<T>(Rc<T>);

impl<T> Borrow<RcCmp<T>> for Rc<T> {
    fn borrow(&self) -> &RcCmp<T> {
        // SAFETY: RcCmp is a transparent structure so a pointer to a Rc is the same as a pointer
        // to a RcCmp
        unsafe { &*(self as *const _ as *const RcCmp<T>) }
    }
}

impl<T> Clone for RcCmp<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> PartialEq for RcCmp<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl<T> Eq for RcCmp<T> {}

impl<T> std::hash::Hash for RcCmp<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        std::ptr::hash(&*self.0, state)
    }
}

impl<T> Deref for RcCmp<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0.deref()
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct SourceInner {
    path: PathBuf,
    lines: Vec<usize>,
    end: (),
    data: str,
}

#[repr(C)]
struct SourceInnerSized {
    path: PathBuf,
    lines: Vec<usize>,
}

#[derive(Clone, Debug)]
pub struct Source(Rc<SourceInner>);

impl PartialEq for Source {
    fn eq(&self, other: &Self) -> bool {
        self.0.path == other.0.path
    }
}

impl Eq for Source {}

impl Hash for Source {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.path.hash(state);
    }
}

impl Deref for Source {
    type Target = SourceInner;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug)]
pub struct MalformedModule {
    location: PathBuf,
    error: std::io::Error,
}

impl std::fmt::Display for MalformedModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Could not read data at {:?}", self.location)
    }
}

impl std::error::Error for MalformedModule {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.error)
    }
}

impl Source {
    pub fn new(root: &Path, module: &Path) -> Result<Self, MalformedModule> {
        let complete_path = root.join(module);
        let data = match std::fs::read_to_string(&complete_path) {
            Ok(d) => d,
            Err(error) => {
                return Err(MalformedModule {
                    location: complete_path,
                    error,
                });
            }
        };

        let lines = std::iter::once(0)
            .chain(
                data.as_bytes()
                    .iter()
                    .enumerate()
                    .filter_map(|(i, &c)| if c == b'\n' { Some(i + 1) } else { None }),
            )
            .chain(std::iter::once(data.len()))
            .collect();

        let path = module.to_owned();

        let layout = Layout::for_value(&path)
            .extend(Layout::for_value(&lines))
            .unwrap()
            .0
            .extend(Layout::from_size_align(data.len(), 1).unwrap())
            .unwrap()
            .0
            .pad_to_align();

        // SAFETY: layout is non null as PathBuf is not a ZST
        let inner_alloc = unsafe { std::alloc::alloc(layout) as *mut SourceInnerSized };

        // SAFETY: path_ptr was just allocated with a proper layout
        unsafe {
            let path_ptr = &raw mut (*inner_alloc).path;
            path_ptr.write(path);
        }

        // SAFETY: lines_ptr was just allocated with a proper layout
        unsafe {
            let lines_ptr = &raw mut (*inner_alloc).lines;
            lines_ptr.write(lines);
        }

        // SAFETY: str_ptr was just allocated with a proper layout
        let str_dst_ptr = unsafe {
            let str_ptr = inner_alloc.byte_add(std::mem::offset_of!(SourceInner, end)) as *mut u8;
            std::ptr::copy_nonoverlapping(data.as_ptr(), str_ptr, data.len());
            std::ptr::slice_from_raw_parts_mut(str_ptr, data.len())
        };

        // SAFETY: the value was completly initialized
        let dst_box = unsafe {
            let p =
                (str_dst_ptr as *mut SourceInner).byte_sub(std::mem::offset_of!(SourceInner, end));
            Box::from_raw(p)
        };

        Ok(Source(dst_box.into()))
    }
}

#[derive(Debug)]
enum Error {
    Args(sheshat::Error<'static, VvkParseErr<'static>, VvkFields>),
    MissingBuildDir,
    CreateBuildDir(PathBuf, io::Error),
    GetCwd(io::Error),
    SearchRoot(io::Error),
    RootNotFound,
    Parse(ParseError),
    Eval(EvalError),
    SpawnTarget { path: PathBuf, err: std::io::Error },
}

impl From<EvalError> for Error {
    fn from(value: EvalError) -> Self {
        Error::Eval(value)
    }
}

impl From<ParseError> for Error {
    fn from(value: ParseError) -> Self {
        Error::Parse(value)
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            Error::Parse(err) => Some(err),
            Error::Eval(err) => Some(err),
            Error::SpawnTarget { err, .. } => Some(err),
            Error::Args(err) => Some(err),
            _ => None,
        }
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::MissingBuildDir => writeln!(f, "Missing build dir $VVK_BUILD"),
            Error::CreateBuildDir(dir, error) => {
                writeln!(f, "Could not create build dir {dir:?}: {error}")
            }
            Error::GetCwd(error) => {
                writeln!(f, "Failed to get current directory: {error}")
            }
            Error::SearchRoot(error) => {
                write!(f, "Failed to find project root: {error}")
            }
            Error::RootNotFound => {
                write!(f, "Project root not found")
            }
            Error::Args(_) => {
                write!(f, "Invalid arguments")
            }
            Error::Parse(_) => write!(f, "Could not parse input"),
            Error::Eval(_) => write!(f, "Evaluation error"),
            Error::SpawnTarget { path, .. } => {
                write!(f, "Failed to run target at {}", path.to_string_lossy())
            }
        }
    }
}

pub struct ToWriteFmt<T>(pub T);

impl<T> std::fmt::Write for ToWriteFmt<T>
where
    T: std::io::Write,
{
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.0.write_all(s.as_bytes()).map_err(|_| std::fmt::Error)
    }
}

struct PrintMetadata;

impl SheshatMetadataHandler for PrintMetadata {
    fn help<'a, T: Sheshat<'a>>(command_name: &CommandName) {
        let mut stdout = ToWriteFmt(std::io::stdout());
        let _ = T::write_usage(command_name, &mut stdout);
        std::process::exit(0)
    }
}

struct ErrWrapper<T>(T);

impl<T: std::error::Error> std::fmt::Debug for ErrWrapper<T>
where
    T: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)?;

        let mut err = self.0.source();
        while let Some(src) = err {
            write!(f, "\n  .. {src}")?;
            err = src.source();
        }

        Ok(())
    }
}

impl<T> From<T> for ErrWrapper<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}

fn main() -> Result<(), ErrWrapper<Error>> {
    let args = Vec::leak(std::env::args().skip(1).collect());
    let Some(args) =
        Vvk::parse_arguments_metadata::<_, PrintMetadata>(args).map_err(Error::Args)?
    else {
        return Ok(());
    };

    let build_dir = PathBuf::from(std::env::var_os("VVK_BUILD").ok_or(Error::MissingBuildDir)?);
    std::fs::create_dir_all(&build_dir).map_err(|e| Error::CreateBuildDir(build_dir.clone(), e))?;

    let current_dir = std::env::current_dir().map_err(Error::GetCwd)?;

    let root_path = match args.root {
        None => {
            let mut ancestor: &Path = &current_dir;
            'parent: loop {
                for entry in ancestor.read_dir().map_err(Error::SearchRoot)? {
                    let entry = entry.map_err(Error::SearchRoot)?;

                    if entry.file_name() == "root.vvk" {
                        break 'parent ancestor.to_path_buf();
                    }
                }

                match ancestor.parent() {
                    Some(p) => ancestor = p,
                    None => return Err(Error::RootNotFound.into()),
                }
            }
        }
        Some(r) => r,
    };

    let root = Module::parse_root(root_path.clone()).map_err(Error::Parse)?;

    let project = project::Project::load(&root, root_path.clone(), build_dir, args.release)
        .map_err(Error::from)?;

    match args.command {
        Commands::Run(run) => {
            let path = current_dir;
            let sub_dir = Some(
                path.canonicalize()
                    .map_err(Error::GetCwd)?
                    .strip_prefix(&root_path)
                    .unwrap()
                    .to_owned(),
            );
            let exe_path = project.run(sub_dir, run.binary).map_err(Error::from)?;

            return Err(Error::SpawnTarget {
                err: std::process::Command::new(&exe_path)
                    .args(run.extra_args)
                    .exec(),
                path: exe_path,
            }
            .into());
        }
        Commands::Test(test) => {
            let path = match test.path {
                Some(p) => p.canonicalize().map_err(Error::GetCwd)?,
                None => current_dir,
            };
            let sub_dir = Some(path.strip_prefix(&root_path).unwrap().to_owned());
            project.test(sub_dir, test.recursive).map_err(Error::from)?
        }
        Commands::Build(build) => {
            let path = match (build.path, build.all) {
                (Some(p), _) => p.canonicalize().map_err(Error::GetCwd)?,
                (None, false) => current_dir,
                (None, true) => root_path.clone(),
            };
            let sub_dir = Some(path.strip_prefix(&root_path).unwrap().to_owned());
            project.build(sub_dir, build.all).map_err(Error::from)?
        }
    }

    Ok(())
}
