#![no_std]
#![no_main]

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
fn main() -> ! {
    debug_println!("\n{BANNER}\n");

    loop {
        wfi();
    }
}
