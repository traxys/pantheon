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

#[repr(C)]
struct InterruptFrame {
    ra: usize,
    sp: usize,
    gp: usize,
    tp: usize,
    t0: usize,
    t1: usize,
    t2: usize,
    fp: usize,
    s1: usize,
    a0: usize,
    a1: usize,
    a2: usize,
    a3: usize,
    a4: usize,
    a5: usize,
    a6: usize,
    a7: usize,
    s2: usize,
    s3: usize,
    s4: usize,
    s5: usize,
    s6: usize,
    s7: usize,
    s8: usize,
    s9: usize,
    s10: usize,
    s11: usize,
    t3: usize,
    t4: usize,
    t5: usize,
    t6: usize,
}

global_asm!(
    "
.global ymir_trap_entry
.align 4
ymir_trap_entry:
    # Use the trap stack
    csrw mscratch, sp
    la sp, _strapstack

    addi sp, sp, -31*8
    sd x1,   0*8(sp)
    sd x2,   1*8(sp)
    sd x3,   2*8(sp)
    sd x4,   3*8(sp)
    sd x5,   4*8(sp)
    sd x6,   5*8(sp)
    sd x7,   6*8(sp)
    sd x8,   7*8(sp)
    sd x9,   8*8(sp)
    sd x10,  9*8(sp)
    sd x11, 10*8(sp)
    sd x12, 11*8(sp)
    sd x13, 12*8(sp)
    sd x14, 13*8(sp)
    sd x15, 14*8(sp)
    sd x16, 15*8(sp)
    sd x17, 16*8(sp)
    sd x18, 17*8(sp)
    sd x19, 18*8(sp)
    sd x20, 19*8(sp)
    sd x21, 20*8(sp)
    sd x22, 21*8(sp)
    sd x23, 22*8(sp)
    sd x24, 23*8(sp)
    sd x25, 24*8(sp)
    sd x26, 25*8(sp)
    sd x27, 26*8(sp)
    sd x28, 27*8(sp)
    sd x29, 28*8(sp)
    sd x30, 29*8(sp)
    sd x31, 30*8(sp)

    mv t0, sp
    addi sp, sp, -8
    sd t0, 0(sp)

    call ymir_trap_handler

    addi sp, sp, 8

    ld x1,   0*8(sp)
    ld x2,   1*8(sp)
    ld x3,   2*8(sp)
    ld x4,   3*8(sp)
    ld x5,   4*8(sp)
    ld x6,   5*8(sp)
    ld x7,   6*8(sp)
    ld x8,   7*8(sp)
    ld x9,   8*8(sp)
    ld x10,  9*8(sp)
    ld x11, 10*8(sp)
    ld x12, 11*8(sp)
    ld x13, 12*8(sp)
    ld x14, 13*8(sp)
    ld x15, 14*8(sp)
    ld x16, 15*8(sp)
    ld x17, 16*8(sp)
    ld x18, 17*8(sp)
    ld x19, 18*8(sp)
    ld x20, 19*8(sp)
    ld x21, 20*8(sp)
    ld x22, 21*8(sp)
    ld x23, 22*8(sp)
    ld x24, 23*8(sp)
    ld x25, 24*8(sp)
    ld x26, 25*8(sp)
    ld x27, 26*8(sp)
    ld x28, 27*8(sp)
    ld x29, 28*8(sp)
    ld x30, 29*8(sp)
    ld x31, 30*8(sp)

    # Restore the original stack
    csrr sp, mscratch

    mret
"
);

unsafe extern "C" {
    fn ymir_trap_entry();
}

#[unsafe(no_mangle)]
extern "C" fn ymir_trap_handler(
    _a0: usize,
    _a1: usize,
    _a2: usize,
    _a3: usize,
    _a4: usize,
    _a5: usize,
    _a6: usize,
    _a7: usize,
    _interrupt_frame: &mut InterruptFrame,
) {
    let mcause: usize;
    unsafe {
        asm!("csrr {0}, mcause", out(reg) mcause);
    }

    if mcause & 1 << 63 == 0 {
        // Exception
        match mcause {
            _ => {
                uart_println!("Unhandled exception: {mcause}");
                STATE.lock().test.as_mut().unwrap().panic(1)
            }
        }
    } else {
        // Interrupt
    }
}

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

    unsafe {
        asm!("csrw mtvec, {0}", in(reg) ymir_trap_entry);
    }

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
