#![no_main]
#![no_std]

use core::arch::{asm, global_asm};

#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {}
}

global_asm!(include_str!("boot.S"));

#[export_name = "_kmain"]
pub unsafe extern "C" fn kmain(_hart_id: usize, _dtb: usize) -> ! {
    loop {
        asm!("wfi")
    }
}
