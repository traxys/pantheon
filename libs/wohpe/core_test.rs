use oshun::{NoIrqMode, SpinLock, SpinLockGuard};
use wohpe::{Filter, LogLevel, Metadata, debug, error, info, trace, warn};

struct TestLogger;
struct TestLoggerState {
    recorded: Vec<(Metadata, String)>,
    local: bool,
    filters: Vec<Filter>,
}

static STATE: SpinLock<NoIrqMode, TestLoggerState> = SpinLock::new(TestLoggerState {
    recorded: Vec::new(),
    local: false,
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

    fn local_filtering(&self) -> bool {
        STATE.lock().local
    }

    fn apply_filter(&self, filter: wohpe::Filter) {
        STATE.lock().filters.push(filter);
    }

    fn reset_filters(&self) {
        STATE.lock().filters.truncate(0);
    }
}

fn drain_logs() -> Vec<(Metadata, String)> {
    let mut state = STATE.lock();
    std::mem::take(&mut state.recorded)
}

macro_rules! assert_log_metadata {
    ($logs:expr, $level:expr, $module:expr, $filename:expr, $line:expr, $message:expr) => {{
        let _log = $logs.next().unwrap();
        assert_eq!(_log.0.level, $level);
        assert_eq!(_log.0.module, $module);
        assert_eq!(_log.0.location.file, $filename);
        assert_eq!(_log.0.location.line, $line);
        assert_eq!(_log.1, $message);
    }};
}

macro_rules! assert_log {
    ($logs:expr, $message:expr) => {{
        let _log = $logs.next().unwrap();
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
        state.local = false;

        wohpe::set_logger(&TestLogger);
        wohpe::set_verbose(verbose);
        wohpe::reset_filters();
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
    assert_log_metadata!(logs, LogLevel::Trace, module_path!(), file!(), line!() - 7, "Trace log");
    assert_log_metadata!(logs, LogLevel::Debug, module_path!(), file!(), line!() - 7, "Debug log");
    assert_log_metadata!(logs, LogLevel::Info, module_path!(), file!(), line!() - 7, "Info log");
    assert_log_metadata!(logs, LogLevel::Warn, module_path!(), file!(), line!() - 7, "Warn log");
    assert_log_metadata!(logs, LogLevel::Error, module_path!(), file!(), line!() - 7, "Error log");
    assert_eq!(logs.next(), None);
}

mod test_child_file;

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
    test_child_file::info();

    let mut logs = drain_logs().into_iter();
    assert_log_metadata!(
        logs, LogLevel::Info, module_path!(), file!(), line!() - 15,
        format!("{}@{}:{} Info log", module_path!(), file!(), line!() - 16)
    );
    assert_log_metadata!(
        logs, LogLevel::Info, format!("{}::child", module_path!()), file!(), line!() - 13,
        format!("{}::child@{}:{} Info log in child", module_path!(), file!(), line!() - 14)
    );
    assert_log_metadata!(
        logs, LogLevel::Info, format!("{}::test_child_file", module_path!()), test_child_file::FILE, 4,
        format!("{}::test_child_file@{}:{} log in another file", module_path!(), test_child_file::FILE, 4)
    );
    assert_eq!(logs.next(), None);
}

#[test]
fn level_filter_ignore() {
    let _guard = TestHandle::enter(false);
    wohpe::append_filter(Filter::level(LogLevel::Info));

    debug!("Debg not shown");
    info!("Info shown");
    error!("Error shown");

    let mut logs = drain_logs().into_iter();
    assert_log!(logs, "Info shown");
    assert_log!(logs, "Error shown");
    assert_eq!(logs.next(), None);
}

#[test]
fn filename_filter_ignore() {
    let _guard = TestHandle::enter(false);
    wohpe::append_filters([
        Filter::level(LogLevel::Error),
        Filter::file(LogLevel::Warn, file!(), None),
        Filter::file(LogLevel::Info, test_child_file::FILE, None),
    ]);

    debug!("Debg not shown");
    warn!("Warn shown");
    error!("Error shown");
    test_child_file::info();

    let mut logs = drain_logs().into_iter();
    assert_log!(logs, "Warn shown");
    assert_log!(logs, "Error shown");
    assert_log!(logs, "log in another file");
    assert_eq!(logs.next(), None);
}

#[test]
fn line_filter_ignore() {
    let _guard = TestHandle::enter(false);
    wohpe::append_filters([
        Filter::level(LogLevel::Error),
        Filter::file(LogLevel::Trace, file!(), Some(line!() + 3)),
    ]);

    trace!("Shown trace");
    trace!("Not shown trace");
    info!("Not shown info");

    let mut logs = drain_logs().into_iter();
    assert_log!(logs, "Shown trace");
    assert_eq!(logs.next(), None);
}

#[test]
fn module_filter_ignore() {
    let _guard = TestHandle::enter(false);
    wohpe::append_filters([
        Filter::level(LogLevel::Error),
        Filter::module(LogLevel::Warn, module_path!()),
        Filter::module(LogLevel::Info, concat!(module_path!(), "::test_child_file")),
    ]);

    mod other_child {
        pub fn other_info() {
            wohpe::info!("Other info log")
        }
    }

    warn!("Shown warn");
    debug!("Not shown debug");
    test_child_file::info();
    other_child::other_info();

    let mut logs = drain_logs().into_iter();
    assert_log!(logs, "Shown warn");
    assert_log!(logs, "log in another file");
    assert_eq!(logs.next(), None);
}
