use core::{arch::asm, cell::UnsafeCell};

use crate::{STATE, SbiExtensionId, SbiRet, uart_print, uart_println};

const TEST_FID_RETURN: usize = 0;

pub struct TestContext {
    pub verbose: bool,
}

pub struct TestStatus {
    pub path: &'static str,
    pub res: i32,
}

#[unsafe(no_mangle)]
static TEST_SAVE: Save = Save(UnsafeCell::new([0, 0]));

#[repr(transparent)]
struct Save(UnsafeCell<[usize; 2]>);

unsafe impl Sync for Save {}

#[unsafe(no_mangle)]
extern "C" fn supervisor_trampoline(func: extern "C" fn() -> i32) -> ! {
    let status = func();

    unsafe {
        asm!("ecall",
            in("a6") SbiExtensionId::YmirTest as usize,
            in("a7") TEST_FID_RETURN,
            in("a0") status,
            options(noreturn)
        )
    }
}

const STACK_SIZE: usize = 1024 * 1024;
static mut SUPERVISOR_STACK: [u64; STACK_SIZE / core::mem::size_of::<u64>()] = [0; _];

pub(crate) fn run_supervisor(func: extern "C" fn() -> i32) -> i32 {
    if unsafe { *TEST_SAVE.0.get() } != [0, 0] {
        panic!("run_supervisor is not re-entrant");
    }

    let mut mstatus: u64;
    unsafe { asm!("csrr {0}, mepc", out(reg) mstatus) };
    // Set MPP to supervisor
    mstatus &= !(1 << 11 | 1 << 12);
    mstatus |= 1 << 11;

    let mut ret = !0;

    unsafe {
        asm!(
            "
            # Save the current execution context
            la t0, TEST_SAVE
            sd sp, 0(t0)
            la t1, {restore}
            sd t1, 8(t0)

            mv sp, {stack}
            csrw mepc, {trampoline}
            csrw mstatus, {mstatus}
            mret
            ",
            stack=in(reg) (&raw mut SUPERVISOR_STACK).byte_add(STACK_SIZE),
            mstatus=in(reg) mstatus,
            trampoline=in(reg) supervisor_trampoline,
            in("a0") func,
            out("t0") _,
            out("t1") _,
            restore=label {
                // Restore the execution context
                unsafe { asm!(
                    "
                        la t0, TEST_SAVE
                        ld sp, 0(t0)
                    ", lateout("a0") ret, out("t0") _) };
            },
        );
    }

    unsafe { *TEST_SAVE.0.get() = [0, 0] }

    ret
}

pub fn sbi_handler(function_id: usize, a0: usize) -> Option<SbiRet> {
    match function_id {
        TEST_FID_RETURN => unsafe {
            let [_, restore] = *TEST_SAVE.0.get();
            asm!(
                "jr {restore}",
                in("a0") a0,
                restore=in(reg) restore,
                options(noreturn)
            );
        },
        _ => None,
    }
}

pub fn test_runner(tests: &[&dyn Fn(&mut TestContext) -> TestStatus]) -> ! {
    uart_println!("=== Running {} tests ===", tests.len());

    let mut context = TestContext { verbose: false };

    for test in tests {
        let status = test(&mut context);
        uart_print!(
            "{} ... {}",
            status.path,
            if status.res == 0 { "ok" } else { "not ok" }
        );
        if status.res != 0 {
            uart_print!(" ({})", status.res);
        }
        uart_println!("");
    }

    STATE.lock().test.as_mut().unwrap().shutdown(0)
}

pub struct SbiConsole;

impl core::fmt::Write for SbiConsole {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        let error: usize;
        let value: usize;
        unsafe {
            asm!("ecall",
                 in("a6") SbiExtensionId::DebugConsole as usize,
                 in("a7") 0,
                 in("a0") s.len(),
                 in("a1") s.as_ptr(),
                 in("a2") 0,
                 lateout("a0") error,
                 lateout("a1") value);
        }

        if error != 0 {
            Err(core::fmt::Error)
        } else {
            assert_eq!(value, s.len(), "Did not print entire string");
            Ok(())
        }
    }
}

macro_rules! sbi_print {
    ($fmt:expr $(, $($args:tt)*)?) => {{
        use core::fmt::Write;

        write!(crate::test::SbiConsole, $fmt $(, $($args)*)?).unwrap();
    }};
}

macro_rules! sbi_println {
    ($fmt:expr $(, $($args:tt)*)?) => {{
        let args = format_args!($fmt $(, $($args)*)?);
        crate::test::sbi_print!("{}\n", args);
    }};
}

macro_rules! sbi_assert_eq {
    ($left:expr, $right:expr, $($msg:tt)*) => {{
        let left = $left;
        let right = $right;
        if left != right {
            crate::test::sbi_println!("{left:?} != {right:?}: {}", format_args!($($msg)*));
            return Err(-10);
        }
    }};
}

macro_rules! ymir_supervisor_test {
    (fn $name:ident() -> Result<(), i32> { $($body:tt)* }) => {
        #[test_case]
        fn $name(context: &mut crate::test::TestContext) -> crate::test::TestStatus {
            if context.verbose {
                crate::uart_println!("> Running {}", stringify!($name));
            }

            extern "C" fn test_body() -> i32 {
                match (|| {$($body)*})() {
                    Ok(()) => 0,
                    Err(e) => e,
                }
            }

            let res: i32 = crate::test::run_supervisor(test_body);

            crate::test::TestStatus {
                path: concat!(module_path!(), "::", stringify!($name)),
                res,
            }
        }
    };
}

pub(crate) use {sbi_assert_eq, sbi_print, sbi_println, ymir_supervisor_test};
