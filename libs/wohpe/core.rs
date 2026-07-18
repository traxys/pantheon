#![no_std]

use core::str::FromStr;

use oshun::{NoIrqMode, SpinLock};

#[repr(u8)]
#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum LogLevel {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
}

impl FromStr for LogLevel {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.eq_ignore_ascii_case("trace") {
            Ok(Self::Trace)
        } else if s.eq_ignore_ascii_case("debug") {
            Ok(Self::Debug)
        } else if s.eq_ignore_ascii_case("info") {
            Ok(Self::Info)
        } else if s.eq_ignore_ascii_case("warn") {
            Ok(Self::Warn)
        } else if s.eq_ignore_ascii_case("error") {
            Ok(Self::Error)
        } else {
            Err(())
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct Location {
    pub file: &'static str,
    pub line: u32,
    pub column: u32,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub struct Metadata {
    pub module: &'static str,
    pub level: LogLevel,
    pub location: Location,
}

#[derive(Clone, Copy, Debug)]
enum FilterKind {
    None,
    File {
        filename: &'static str,
        line: Option<u32>,
    },
    Module(&'static str),
}

#[derive(Clone, Copy, Debug)]
pub struct Filter {
    kind: FilterKind,
    level: LogLevel,
}

pub struct FilterParseError;

impl Filter {
    pub const fn level(level: LogLevel) -> Self {
        Self {
            level,
            kind: FilterKind::None,
        }
    }

    pub const fn file(level: LogLevel, filename: &'static str, line: Option<u32>) -> Self {
        Self {
            level,
            kind: FilterKind::File { filename, line },
        }
    }

    pub const fn module(level: LogLevel, module: &'static str) -> Self {
        Self {
            kind: FilterKind::Module(module),
            level,
        }
    }

    pub fn parse(s: &'static str) -> Result<Self, FilterParseError> {
        match s.split_once('=') {
            None => {
                let Ok(level) = s.parse() else {
                    warn!("Invalid log level '{s}'");
                    return Err(FilterParseError);
                };
                Ok(Self::level(level))
            }
            Some((kind, level)) => {
                let Some((kind, payload)) = kind.split_once(':') else {
                    warn!("Malformed kind: '{kind}'");
                    return Err(FilterParseError);
                };

                let kind = if kind.eq_ignore_ascii_case("file") || kind.eq_ignore_ascii_case("f") {
                    let (filename, line) = match payload.split_once(':') {
                        None => (payload, None),
                        Some((filename, line)) => {
                            let Ok(line) = line.parse() else {
                                warn!("Invalid line in file: '{line}'");
                                return Err(FilterParseError);
                            };
                            (filename, Some(line))
                        }
                    };

                    FilterKind::File { filename, line }
                } else if kind.eq_ignore_ascii_case("module") || kind.eq_ignore_ascii_case("m") {
                    FilterKind::Module(payload)
                } else {
                    warn!("Unknown kind '{kind}'");
                    return Err(FilterParseError);
                };

                let Ok(level) = level.parse() else {
                    warn!("Invalid log level '{level}'");
                    return Err(FilterParseError);
                };

                Ok(Filter { kind, level })
            }
        }
    }

    pub fn matches(&self, metadata: &Metadata) -> bool {
        match self.kind {
            FilterKind::None => true,
            FilterKind::File { filename, line } => {
                if metadata.location.file.ends_with(filename) {
                    match line {
                        Some(l) => l == metadata.location.line,
                        None => true,
                    }
                } else {
                    false
                }
            }
            FilterKind::Module(module) => {
                let Some(tail) = metadata.module.strip_prefix(module) else {
                    return false;
                };

                tail.is_empty() || tail.starts_with("::")
            }
        }
    }
}

pub trait Logger {
    /// Perform the filtering in the logger instead of the core
    fn local_filtering(&self) -> bool;
    /// Called each time a filter is applied
    fn apply_filter(&self, filter: Filter);
    /// Reset the filter stack
    fn reset_filters(&self);
    fn enabled(&self, metadata: Metadata) -> bool;
    fn record(&self, metadata: Metadata, args: core::fmt::Arguments<'_>);
}

struct NoLogger;
impl Logger for NoLogger {
    fn enabled(&self, _: Metadata) -> bool {
        false
    }

    fn record(&self, _: Metadata, _: core::fmt::Arguments<'_>) {}

    fn local_filtering(&self) -> bool {
        false
    }

    fn apply_filter(&self, _: Filter) {}

    fn reset_filters(&self) {}
}

struct FilterVec<const N: usize> {
    backing: [Filter; N],
    len: usize,
}

pub enum Verbosity {
    None,
    Module,
    All,
}

struct LoggerState {
    logger: &'static (dyn Logger + Send + Sync),
    verbosity: Verbosity,
    filters: FilterVec<4096>,
}

static NO_LOGGER: NoLogger = NoLogger;
static STATE: SpinLock<NoIrqMode, LoggerState> = SpinLock::new(LoggerState {
    logger: &NO_LOGGER,
    verbosity: Verbosity::None,
    filters: FilterVec {
        backing: [Filter::level(LogLevel::Trace); _],
        len: 0,
    },
});

pub fn set_verbosity(verbosity: Verbosity) {
    STATE.lock().verbosity = verbosity;
}

pub fn set_logger(logger: &'static (dyn Logger + Send + Sync)) {
    STATE.lock().logger = logger;
}

/// Returns `true` if all filters have been globally applied. All filters are always forwarded
/// to the underlying logger in addition of being globally applied
pub fn append_filters<I>(filters: I) -> bool
where
    I: IntoIterator<Item = Filter>,
{
    let mut state = STATE.lock();
    let mut ok = true;

    for filter in filters {
        state.logger.apply_filter(filter);

        let len = state.filters.len;
        if len >= state.filters.backing.len() {
            ok = false;
            continue;
        }

        state.filters.backing[len] = filter;
        state.filters.len += 1;
    }

    ok
}

pub fn append_filter(filter: Filter) -> bool {
    append_filters([filter])
}

pub fn reset_filters() {
    STATE.lock().filters.len = 0;
}

pub fn parse_directives(directives: &'static str) -> Result<(), FilterParseError> {
    for directive in directives.split(',') {
        if directive.eq_ignore_ascii_case("verbose") {
            set_verbosity(Verbosity::Module);
        } else if directive.eq_ignore_ascii_case("quiet") {
            set_verbosity(Verbosity::None);
        } else if directive.eq_ignore_ascii_case("verbose-all") {
            set_verbosity(Verbosity::All);
        } else {
            let filter = Filter::parse(directive)?;
            append_filter(filter);
        }
    }

    Ok(())
}

pub fn do_log(metadata: Metadata, args: core::fmt::Arguments<'_>) {
    let state = STATE.lock();

    if !state.logger.local_filtering() && state.filters.len > 0 {
        let mut target_level = None;
        for filter in &state.filters.backing[0..state.filters.len] {
            if filter.matches(&metadata) {
                target_level = Some(filter.level);
            }
        }

        match target_level {
            Some(target) if metadata.level as u8 >= target as u8 => (),
            _ => return,
        }
    }

    let log = match state.verbosity {
        Verbosity::None => args,
        Verbosity::Module => format_args!("{}: {}", metadata.module, args),
        Verbosity::All => format_args!(
            "{}@{}:{} {}",
            metadata.module, metadata.location.file, metadata.location.line, args
        ),
    };

    state.logger.record(metadata, log);
}

#[macro_export]
macro_rules! metadata {
    ($level:expr) => {
        $crate::Metadata {
            module: module_path!(),
            level: $level,
            location: $crate::Location {
                file: file!(),
                line: line!(),
                column: column!(),
            },
        }
    };
}

#[macro_export]
macro_rules! log {
    ($level:expr, $($fmt:tt)*) => {
        $crate::do_log($crate::metadata!($level), format_args!($($fmt)*))
    };
}

macro_rules! define_level_macro {
    ($dollar:tt, $name:ident, $level:ident) => {
        #[macro_export]
        macro_rules! $name {
                    ($dollar($dollar fmt:tt)*) => {
                        $dollar crate::log!($dollar crate::LogLevel::$level, $dollar ($dollar fmt)*)
                    };
                }
    };
}

define_level_macro!($, trace, Trace);
define_level_macro!($, debug, Debug);
define_level_macro!($, info, Info);
define_level_macro!($, warn, Warn);
define_level_macro!($, error, Error);
