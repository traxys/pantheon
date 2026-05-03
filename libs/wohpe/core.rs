#![no_std]

use oshun::{NoIrqMode, SpinLock};

#[derive(PartialEq, Eq, Hash, Debug, Clone, Copy)]
pub enum LogLevel {
    Trace,
    Debug,
    Info,
    Warn,
    Error,
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

pub trait Logger {
    fn enabled(&self, metadata: Metadata) -> bool;
    fn record(&self, metadata: Metadata, args: core::fmt::Arguments<'_>);
}

struct NoLogger;
impl Logger for NoLogger {
    fn enabled(&self, _: Metadata) -> bool {
        false
    }

    fn record(&self, _: Metadata, _: core::fmt::Arguments<'_>) {}
}

struct LoggerState {
    logger: &'static (dyn Logger + Send + Sync),
    verbose: bool,
}

static NO_LOGGER: NoLogger = NoLogger;
static STATE: SpinLock<NoIrqMode, LoggerState> = SpinLock::new(LoggerState {
    logger: &NO_LOGGER,
    verbose: false,
});

pub fn set_verbose(verbose: bool) {
    STATE.lock().verbose = verbose;
}

pub fn set_logger(logger: &'static (dyn Logger + Send + Sync)) {
    STATE.lock().logger = logger;
}

pub fn do_log(metadata: Metadata, args: core::fmt::Arguments<'_>) {
    let state = STATE.lock();

    let log = if state.verbose {
        format_args!(
            "{}@{}:{} {}",
            metadata.module, metadata.location.file, metadata.location.line, args
        )
    } else {
        args
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
