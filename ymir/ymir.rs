#![no_std]
#![no_main]

use core::{
    arch::{asm, global_asm},
    fmt::Write,
    mem::MaybeUninit,
};

mod uart;

global_asm!(
    "
.section .text.init
.global _start

_start:
    .cfi_startproc
    .cfi_undefined ra
    la sp, _sstack
    call ymir_entry
    .cfi_endproc
"
);

static BANNER: &str = r#"
'\\  //` '||\   /||` |''||''| '||'''|, 
  \\//    ||\\.//||     ||     ||   || 
   ||     ||     ||     ||     ||...|' 
   ||     ||     ||     ||     || \\   
  .||.   .||     ||. |..||..| .||  \\. 
"#;

#[unsafe(no_mangle)]
/// # SAFETY
///
/// This must be called with QEMU’s starting arguments
pub unsafe extern "C" fn ymir_entry(hart_id: usize, phys_dtb: *const u8) -> ! {
    let memory = &mut [MaybeUninit::uninit(); 4096 * 8];

    let allocator = apis::Allocator::new(memory);
    // If the parsing fails then no logging is possible
    let device_tree = unsafe { ogma::load_dtb(phys_dtb, &allocator).unwrap() };
    let device_tree = ogma::DeviceTree::load(&device_tree, &allocator).unwrap();
    let soc = device_tree.root.child("soc").unwrap();

    // SAFETY: The device tree comes from QEMU
    let mut uart = unsafe { uart::Uart::new(soc.child("serial").unwrap()).unwrap() };

    writeln!(uart, "{BANNER}").unwrap();
    writeln!(
        uart,
        "Platform Model\t\t: {}",
        device_tree.root.model().unwrap_or_default()
    )
    .unwrap();
    writeln!(uart, "Hart ID:\t\t: {hart_id}").unwrap();

    loop {
        unsafe { asm!("wfi", options(nostack)) }
    }
}

#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    loop {
        unsafe { asm!("wfi", options(nostack)) }
    }
}
