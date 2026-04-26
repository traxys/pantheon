#![no_std]
#![no_main]

use core::arch::{asm, naked_asm};

const STACK_LEN: usize = 4096;
static mut STACK: [u8; STACK_LEN] = [0; _];

unsafe extern "C" {
    #[link_name = "__global_pointer$"]
    static GLOBAL_POINTER: usize;
}

#[unsafe(no_mangle)]
#[unsafe(naked)]
pub extern "C" fn _start() -> ! {
    naked_asm!(
        r#"
        .cfi_startproc
        .cfi_undefined ra
        la sp, {stack}
        li t0, {stack_len}
        add sp, t0, sp
        la gp, {gp}
        call {start}
        .cfi_endproc
    "#,
        start=sym _kernel_start,
        stack=sym STACK,
        stack_len=const STACK_LEN,
        gp=sym GLOBAL_POINTER,
    )
}

#[unsafe(no_mangle)]
extern "C" fn _kernel_start() -> ! {
    unsafe {
        asm!("ecall", options(noreturn), in("a6") 0x53525354, in("a7") 0, in("a0") 0, in("a1") 0);
    }
}

#[panic_handler]
fn panic(_: &core::panic::PanicInfo) -> ! {
    unsafe {
        asm!("ecall", options(noreturn), in("a6") 0x53525354, in("a7") 0, in("a0") 0, in("a1") 1);
    }
}
