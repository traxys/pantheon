#![no_std]
#![no_main]
#![feature(custom_test_frameworks)]
#![test_runner(crate::test_runner)]
#![reexport_test_harness_main = "test_main"]

use core::fmt::{self, Write};
use core::panic::PanicInfo;
use riscv_rt::entry;
use sbi::DebugConsole;

mod sbi;

#[panic_handler]
fn panic(info: &PanicInfo) -> ! {
    debug_println!("==== PANIC ====\n{info}");

    sbi::sbi_panic()
}

#[macro_export]
macro_rules! debug_print {
    ($($arg:tt)*) => ($crate::_print_args(format_args!($($arg)*)));
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

#[cfg(test)]
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
            crate::debug_print!("{}...", stringify!($name));
            $($tt)*
            crate::debug_println!("[ok]");
        }
    };
}

#[macro_rules_attribute::apply(hades_test)]
fn trivial() {
    assert_eq!(1, 1);
}

pub fn wfi() {
    unsafe { core::arch::asm!("wfi") }
}

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

#[entry]
fn main(a0: usize, a1: usize) -> ! {
    debug_println!("\n{BANNER}\n");
    debug_println!("  hart: {a0}");
    debug_println!("  dtb:  0x{a1:x}");

    #[cfg(test)]
    test_main();

    loop {
        wfi();
    }
}
