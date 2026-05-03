use oshun::{NoIrqMode, SpinLock, SpinLockGuard};
use wohpe::{LogLevel, Metadata, debug, error, info, trace, warn};

struct TestLogger;
struct TestLoggerState {
    recorded: Vec<(Metadata, String)>,
    global: bool,
    filters: Vec<Filter>,
}

static STATE: SpinLock<NoIrqMode, TestLoggerState> = SpinLock::new(TestLoggerState {
    recorded: Vec::new(),
    filters: Vec::new(),
});

impl wohpe::Logger for TestLogger {
    fn enabled(&self, _: wohpe::Metadata) -> bool {
        true
    }

    fn record(&self, metadata: wohpe::Metadata, args: core::fmt::Arguments<'_>) {
        let mut state = STATE.lock();
        println!("{args}");
        state.recorded.push((metadata, args.to_string()));
    }
}

fn drain_logs() -> Vec<(Metadata, String)> {
    let mut state = STATE.lock();
    std::mem::take(&mut state.recorded)
}

macro_rules! assert_log_metadata {
    ($logs:expr, $level:expr, $module:expr, $filename:expr, $line_offset:expr, $message:expr) => {{
        let _log = $logs.next().unwrap();
        assert_eq!(_log.0.level, $level);
        assert_eq!(_log.0.module, $module);
        assert_eq!(_log.0.location.file, $filename);
        assert_eq!(_log.0.location.line, line!() - $line_offset);
        assert_eq!(_log.1, $message);
    }};
}

struct TestHandle<'a> {
    _guard: SpinLockGuard<'a, NoIrqMode, ()>,
}

impl<'a> TestHandle<'a> {
    pub fn enter(verbose: bool) -> Self {
        static TEST_LOCK: SpinLock<NoIrqMode, ()> = SpinLock::new(());

        let _guard = TEST_LOCK.lock();
        let mut state = STATE.lock();

        state.recorded.truncate(0);

        wohpe::set_logger(&TestLogger);
        wohpe::set_verbose(verbose);

        Self { _guard }
    }
}

#[rustfmt::skip]
#[test]
fn basic() {
    let _guard = TestHandle::enter(false);

    trace!("Trace log");
    debug!("Debug log");
    info!("Info log");
    warn!("Warn log");
    error!("Error log");

    let mut logs = drain_logs().into_iter();
    assert_log_metadata!(logs, LogLevel::Trace, module_path!(), file!(), 7, "Trace log");
    assert_log_metadata!(logs, LogLevel::Debug, module_path!(), file!(), 7, "Debug log");
    assert_log_metadata!(logs, LogLevel::Info, module_path!(), file!(), 7, "Info log");
    assert_log_metadata!(logs, LogLevel::Warn, module_path!(), file!(), 7, "Warn log");
    assert_log_metadata!(logs, LogLevel::Error, module_path!(), file!(), 7, "Error log");
    assert_eq!(logs.next(), None);
}

#[rustfmt::skip]
#[test]
fn verbose() {
    let _guard = TestHandle::enter(true);

    info!("Info log");

    mod child {
        use wohpe::info;

        pub fn child_log() {
            info!("Info log in child");
        }
    }

    child::child_log();

    let mut logs = drain_logs().into_iter();
    assert_log_metadata!(
        logs, LogLevel::Info, module_path!(), file!(), 13,
        format!("{}@{}:{} Info log", module_path!(), file!(), line!() - 15)
    );
    assert_log_metadata!(
        logs, LogLevel::Info, format!("{}::child", module_path!()), file!(), 11,
        format!("{}::child@{}:{} Info log in child", module_path!(), file!(), line!() - 13)
    );
    assert_eq!(logs.next(), None);
}
