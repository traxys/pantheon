use std::env::VarError;

pub use wohpe;
use wohpe::{Filter, LogLevel};

struct EnvLogger;

impl wohpe::Logger for EnvLogger {
    fn local_filtering(&self) -> bool {
        false
    }

    fn apply_filter(&self, _: wohpe::Filter) {}
    fn reset_filters(&self) {}

    fn enabled(&self, _: wohpe::Metadata) -> bool {
        true
    }

    fn record(&self, metadata: wohpe::Metadata, args: core::fmt::Arguments<'_>) {
        let prepend = match metadata.level {
            wohpe::LogLevel::Trace => "\x1b[90;2m[TRACE]\x1b[0m",
            wohpe::LogLevel::Debug => "\x1b[90m[DEBUG]\x1b[0m",
            wohpe::LogLevel::Info => "\x1b[97;1m[INFO ]\x1b[0m",
            wohpe::LogLevel::Warn => "\x1b[93;1m[WARN ]\x1b[0m",
            wohpe::LogLevel::Error => "\x1b[91;1m[ERROR]\x1b[0m",
        };

        eprintln!("{prepend} {args}");
    }
}

pub fn init(default_level: LogLevel) -> Result<(), std::io::Error> {
    static LOGGER: EnvLogger = EnvLogger;
    wohpe::set_logger(&LOGGER);
    wohpe::append_filter(Filter::level(default_level));

    let env = match std::env::var("PT_LOG") {
        Ok(e) => Some(e.leak()),
        Err(VarError::NotPresent) => None,
        Err(VarError::NotUnicode(_)) => {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "non unicode env var",
            ));
        }
    };

    if let Some(env) = env {
        wohpe::parse_directives(env).map_err(|_| std::io::Error::other("Failed to parse filter"))?;
    }

    Ok(())
}
