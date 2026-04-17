#![no_std]
#![no_main]

use core::{
    arch::{asm, global_asm},
    mem::MaybeUninit,
};

use oshun::SpinLock;

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

static UART: SpinLock<Option<uart::Uart>> = SpinLock::new(None);

macro_rules! uart_print {
    ($fmt:expr $(, $($args:tt)*)?) => {
        let mut uart = crate::UART.lock();
        if let Some(uart) = &mut *uart {
            use core::fmt::Write;

            write!(uart, $fmt $(, $($args)*)?).unwrap();
        }
        drop(uart);
    };
}

macro_rules! uart_println {
    ($fmt:expr $(, $($args:tt)*)?) => {
        let args = format_args!($fmt $(, $($args)*)?);
        uart_print!("{}\n", args);
    };
}

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
    let uart = unsafe { uart::Uart::new(soc.child("serial").unwrap()).unwrap() };
    *UART.lock() = Some(uart);

    uart_println!("{BANNER}");
    uart_println!(
        "Platform Model\t\t: {}",
        device_tree.root.model().unwrap_or_default()
    );
    uart_println!("Hart ID:\t\t: {hart_id}");

    loop {
        unsafe { asm!("wfi", options(nostack)) }
    }
}

#[panic_handler]
fn panic(info: &core::panic::PanicInfo) -> ! {
    uart_println!("{info}");

    loop {
        unsafe { asm!("wfi", options(nostack)) }
    }
}
