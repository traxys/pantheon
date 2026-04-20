#![no_std]
#![no_main]
#![feature(custom_test_frameworks)]
#![test_runner(crate::test::test_runner)]
#![reexport_test_harness_main = "test_main"]

use core::{
    arch::{asm, global_asm},
    mem::MaybeUninit,
};

use oshun::{MachineMode, SpinLock};

mod sifive_test;
#[cfg(test)]
mod test;
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

struct YmirState {
    uart: Option<uart::Uart>,
    test: Option<sifive_test::SifiveTest>,
}

static STATE: SpinLock<MachineMode, YmirState> = SpinLock::new(YmirState {
    uart: None,
    test: None,
});

macro_rules! uart_print {
    ($fmt:expr $(, $($args:tt)*)?) => {{
        let mut state = crate::STATE.lock();
        if let Some(uart) = &mut state.uart {
            use core::fmt::Write;

            write!(uart, $fmt $(, $($args)*)?).unwrap();
        }
        drop(state);
    }};
}

macro_rules! uart_println {
    ($fmt:expr $(, $($args:tt)*)?) => {{
        let args = format_args!($fmt $(, $($args)*)?);
        crate::uart_print!("{}\n", args);
    }};
}

pub(crate) use uart_print;
#[cfg(test)]
pub(crate) use uart_println;

pub fn setup_pmp() {
    #[allow(unused)]
    #[repr(u8)]
    enum AddressMode {
        Null = 0,
        Top = 1,
        Nat4 = 2,
        Napot = 3,
    }

    fn pmp_cfg(locked: bool, mode: AddressMode, read: bool, write: bool, execute: bool) -> u8 {
        (read as u8)
            | ((write as u8) << 1)
            | ((execute as u8) << 2)
            | ((mode as u8) << 3)
            | ((locked as u8) << 7)
    }

    let pmpcfg0 = pmp_cfg(false, AddressMode::Top, true, true, true) as u64;

    unsafe {
        asm!(
        "
            csrw pmpaddr0, {end}

            csrw pmpcfg0, {cfg}
        ",
            end = in(reg) !0usize,
            cfg = in(reg) pmpcfg0,
        );
    }
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
    STATE.lock().uart = Some(uart);

    uart_println!("{BANNER}");
    uart_println!(
        "Platform Model\t\t: {}",
        device_tree.root.model().unwrap_or_default()
    );
    uart_println!("Hart ID\t\t\t: {hart_id}");

    let test_dev = unsafe { sifive_test::SifiveTest::new(soc.child("test").unwrap()).unwrap() };
    STATE.lock().test = Some(test_dev);

    setup_pmp();

    #[cfg(test)]
    {
        test_main();
    }

    STATE.lock().test.as_mut().unwrap().shutdown(0)
}

#[panic_handler]
fn panic(info: &core::panic::PanicInfo) -> ! {
    uart_println!("{info}");

    loop {
        unsafe { asm!("wfi", options(nostack)) }
    }
}
