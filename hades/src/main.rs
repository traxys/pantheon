#![no_main]
#![no_std]
#![feature(custom_test_frameworks)]
#![test_runner(crate::test::test_runner)]
#![reexport_test_harness_main = "test_main"]

use core::{
    arch::global_asm,
    fmt::{self, Write},
    mem::MaybeUninit,
    ptr::addr_of_mut,
};

use sbi::DebugConsole;

use crate::dtb::DeviceTree;

mod arch;
mod dtb;
mod early_alloc;
mod lock;
mod page_table;
mod sbi;

// TODO: load these from symbols
const RAM_START: usize = 0x80000000;
const PHYSICAL_STACK_START: usize = 0x80000000 + 0x2000000 + 16 * 1024 * 1024;
const RAM_VIRTUAL_START: u64 = !((1 << 47) - 1);

extern "C" {
    #[link_name = "_KERNEL_CODE_VIRTUAL"]
    static KERNEL_CODE_VIRTUAL: u8;

    #[link_name = "_VIRTUAL_STACK"]
    static KERNEL_STACK_VIRTUAL: u8;
}

fn virt_to_phys<T: ?Sized>(v: &T) -> usize {
    let addr = v as *const T as *const () as usize;
    let virtual_code_start = unsafe { &KERNEL_CODE_VIRTUAL as *const _ as usize };
    let virtual_stack_start = unsafe { &KERNEL_STACK_VIRTUAL as *const _ as usize };

    if addr >= virtual_code_start {
        let offset = addr - virtual_code_start;
        RAM_START + offset
    } else if addr >= virtual_stack_start {
        let offset = addr - virtual_stack_start;
        PHYSICAL_STACK_START + offset
    } else {
        panic!("Unhandled virtual address: 0x{addr:x}")
    }
}

/// Prints and returns the value of a given expression for quick and dirty
/// debugging.
///
/// An example:
///
/// ```rust
/// let a = 2;
/// let b = dbg!(a * 2) + 1;
/// //      ^-- prints: [src/main.rs:2] a * 2 = 4
/// assert_eq!(b, 5);
/// ```
///
/// The macro works by using the `Debug` implementation of the type of
/// the given expression to print the value to [stderr] along with the
/// source location of the macro invocation as well as the source code
/// of the expression.
///
/// Invoking the macro on an expression moves and takes ownership of it
/// before returning the evaluated expression unchanged. If the type
/// of the expression does not implement `Copy` and you don't want
/// to give up ownership, you can instead borrow with `dbg!(&expr)`
/// for some expression `expr`.
///
/// The `dbg!` macro works exactly the same in release builds.
/// This is useful when debugging issues that only occur in release
/// builds or when debugging in release mode is significantly faster.
///
/// Note that the macro is intended as a debugging tool and therefore you
/// should avoid having uses of it in version control for long periods
/// (other than in tests and similar).
/// Debug output from production code is better done with other facilities
/// such as the [`debug!`] macro from the [`log`] crate.
///
/// # Stability
///
/// The exact output printed by this macro should not be relied upon
/// and is subject to future changes.
///
/// # Panics
///
/// Panics if writing to `io::stderr` fails.
///
/// # Further examples
///
/// With a method call:
///
/// ```rust
/// fn foo(n: usize) {
///     if let Some(_) = dbg!(n.checked_sub(4)) {
///         // ...
///     }
/// }
///
/// foo(3)
/// ```
///
/// This prints to [stderr]:
///
/// ```text,ignore
/// [src/main.rs:4] n.checked_sub(4) = None
/// ```
///
/// Naive factorial implementation:
///
/// ```rust
/// fn factorial(n: u32) -> u32 {
///     if dbg!(n <= 1) {
///         dbg!(1)
///     } else {
///         dbg!(n * factorial(n - 1))
///     }
/// }
///
/// dbg!(factorial(4));
/// ```
///
/// This prints to [stderr]:
///
/// ```text,ignore
/// [src/main.rs:3] n <= 1 = false
/// [src/main.rs:3] n <= 1 = false
/// [src/main.rs:3] n <= 1 = false
/// [src/main.rs:3] n <= 1 = true
/// [src/main.rs:4] 1 = 1
/// [src/main.rs:5] n * factorial(n - 1) = 2
/// [src/main.rs:5] n * factorial(n - 1) = 6
/// [src/main.rs:5] n * factorial(n - 1) = 24
/// [src/main.rs:11] factorial(4) = 24
/// ```
///
/// The `dbg!(..)` macro moves the input:
///
/// ```compile_fail
/// /// A wrapper around `usize` which importantly is not Copyable.
/// #[derive(Debug)]
/// struct NoCopy(usize);
///
/// let a = NoCopy(42);
/// let _ = dbg!(a); // <-- `a` is moved here.
/// let _ = dbg!(a); // <-- `a` is moved again; error!
/// ```
///
/// You can also use `dbg!()` without a value to just print the
/// file and line whenever it's reached.
///
/// Finally, if you want to `dbg!(..)` multiple values, it will treat them as
/// a tuple (and return it, too):
///
/// ```
/// assert_eq!(dbg!(1usize, 2u32), (1, 2));
/// ```
///
/// However, a single argument with a trailing comma will still not be treated
/// as a tuple, following the convention of ignoring trailing commas in macro
/// invocations. You can use a 1-tuple directly if you need one:
///
/// ```
/// assert_eq!(1, dbg!(1u32,)); // trailing comma ignored
/// assert_eq!((1,), dbg!((1u32,))); // 1-tuple
/// ```
///
/// [stderr]: https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)
/// [`debug!`]: https://docs.rs/log/*/log/macro.debug.html
/// [`log`]: https://crates.io/crates/log
#[macro_export]
macro_rules! dbg {
    // NOTE: We cannot use `concat!` to make a static string as a format argument
    // of `eprintln!` because `file!` could contain a `{` or
    // `$val` expression could be a block (`{ .. }`), in which case the `eprintln!`
    // will be malformed.
    () => {
        $crate::debug_println!("[{}:{}]", core::file!(), core::line!())
    };
    ($val:expr $(,)?) => {
        // Use of `match` here is intentional because it affects the lifetimes
        // of temporaries - https://stackoverflow.com/a/48732525/1063961
        match $val {
            tmp => {
                $crate::debug_println!("[{}:{}] {} = {:#?}",
                    core::file!(), core::line!(), core::stringify!($val), &tmp);
                tmp
            }
        }
    };
    ($($val:expr),+ $(,)?) => {
        ($($crate::dbg!($val)),+,)
    };
}

#[macro_export]
macro_rules! debug_print {
    ($($arg:tt)*) => ($crate::_debug_print_args(format_args!($($arg)*)));
}

#[macro_export]
macro_rules! debug_println {
    ($($arg:tt)*) => {{
        $crate::_debug_println_args(core::format_args!($($arg)*));
    }};
}

pub fn _debug_print_args(args: fmt::Arguments) {
    let _ = write!(DebugConsole, "{args}");
}

pub fn _debug_println_args(args: fmt::Arguments) {
    let _ = writeln!(DebugConsole, "{args}");
}

#[panic_handler]
fn panic(info: &core::panic::PanicInfo) -> ! {
    debug_println!("\n==== PANIC ====\n{info}");

    sbi::sbi_panic()
}

global_asm!(include_str!("boot.S"));

const BANNER: &str = r#"

==============================

 ,,           |\               
 ||      _     \\              
 ||/\\  < \,  / \\  _-_   _-_, 
 || ||  /-|| || || || \\ ||_.  
 || || (( || || || ||/    ~ || 
 \\ |/  \/\\  \\/  \\,/  ,-_-  
   _/                          
"#;

// 1MB of early heap
const EARLY_HEAP_LEN: usize = 1024 * 1024;

global_asm!(
    ".section .data",
    ".global _early_heap",
    ".align 12",
    "_early_heap:",
    ".rep 1024 * 1024",
    "    .byte 0",
    ".endr"
);

extern "C" {
    #[link_name = "_early_heap"]
    static mut EARLY_HEAP: u8;
}

#[export_name = "_kmain"]
pub unsafe extern "C" fn kmain(hart_id: usize, phys_dtb: usize) -> ! {
    debug_println!("{BANNER}\n");
    debug_println!("  hart: {hart_id}");
    debug_println!("  dtb (physical): 0x{phys_dtb:x}");

    page_table::init_root_pt();

    let early_heap = unsafe {
        core::slice::from_raw_parts_mut(
            addr_of_mut!(EARLY_HEAP) as *mut MaybeUninit<u8>,
            EARLY_HEAP_LEN,
        )
    };
    let early_allocator = early_alloc::EarlyAllocator::new(early_heap);

    let dtb_virtual_address = unsafe {
        let offset = phys_dtb - RAM_START;
        &KERNEL_CODE_VIRTUAL as *const _ as usize + offset
    };

    let dtb = unsafe {
        dtb::load_dtb(dtb_virtual_address as *const u8, &early_allocator)
            .expect("Could not load dtb")
    };

    let dtb = DeviceTree::load(&dtb, &early_allocator).expect("Could not load the dtb");

    let general_ram_phys_start = PHYSICAL_STACK_START + 2 * 1024 * 1024;

    if let Some(reserved) = dtb.root.child("reserved-memory") {
        for segment in &*reserved.children {
            let regs = segment.reg().expect("Reserved segment has no registers");
            for reg in regs {
                if reg.address + reg.size >= general_ram_phys_start as u64 {
                    panic!("Reserved memory in general ram is not yet handled");
                }
            }
        }
    }

    let memory = dtb.root.child("memory").expect("no memory found");
    let mem_info = memory.reg().expect("no memory reg found");

    assert_eq!(mem_info.len(), 1, "Fragmented memory is not handled");

    let ram_len =
        mem_info[0].address as usize - (general_ram_phys_start - mem_info[0].address as usize);

    #[cfg(test)]
    test_main();

    todo!("Main loop");
}

#[cfg(test)]
mod test {
    use crate::sbi;

    pub fn test_runner(tests: &[&dyn Fn()]) -> ! {
        debug_println!("== Running {} tests", tests.len());

        for test in tests {
            test();
        }

        sbi::sbi_shutdown()
    }

    #[macro_export]
    macro_rules! hades_test {
        (fn $name:ident() { $($tt:tt)* }) => {
            #[test_case]
            fn $name() {
                $crate::debug_print!("{}...", stringify!($name));
                {
                    $($tt)*
                };
                $crate::debug_println!("[ok]");
            }
        };
    }

    #[cfg(test)]
    mod test {
        #[macro_rules_attribute::apply(hades_test)]
        fn trivial() {
            assert_eq!(1, 1);
        }
    }
}
