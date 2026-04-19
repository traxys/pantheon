#![no_std]

pub const SIE: usize = 1 << 1;
pub const MIE: usize = 1 << 3;
pub const PAGE_SHIFT: usize = 12;
pub const PAGE_SIZE: usize = 1 << PAGE_SHIFT;

macro_rules! csr_read_clear {
    ($name:ident, $mask:expr) => {{
        let v;
        core::arch::asm!(concat!("csrrc {0}, ", stringify!($name), ", {1}"), in(reg) $mask, out(reg) v);
        v
    }};
}

macro_rules! csr_set {
    ($name:ident, $value:expr) => {
        core::arch::asm!(concat!("csrs ", stringify!($name), ", {0}"), in(reg) $value)
    };
}

pub trait PrivilegeMode {
    const IRQ_BIT: usize;

    fn set_status(val: usize);
    fn read_clear_status(val: usize) -> usize;
}

pub struct MachineMode;
impl PrivilegeMode for MachineMode {
    const IRQ_BIT: usize = MIE;

    fn set_status(val: usize) {
        unsafe { csr_set!(mstatus, val) }
    }

    fn read_clear_status(val: usize) -> usize {
        unsafe { csr_read_clear!(mstatus, val) }
    }
}

pub struct SupervisorMode;
impl PrivilegeMode for SupervisorMode {
    const IRQ_BIT: usize = SIE;

    fn set_status(val: usize) {
        unsafe { csr_set!(sstatus, val) }
    }

    fn read_clear_status(val: usize) -> usize {
        unsafe { csr_read_clear!(sstatus, val) }
    }
}

struct InterruptGuard<M>
where
    M: PrivilegeMode,
{
    saved: usize,
    mode: PhantomData<M>,
}

impl<M: PrivilegeMode> InterruptGuard<M> {
    pub fn new() -> Self {
        Self {
            saved: M::read_clear_status(M::IRQ_BIT),
            mode: PhantomData,
        }
    }
}

impl<M: PrivilegeMode> Drop for InterruptGuard<M> {
    fn drop(&mut self) {
        M::set_status(M::IRQ_BIT & self.saved)
    }
}

mod spin;
mod spin_lazy;
mod spin_once;
mod spin_once_lock;

use core::marker::PhantomData;

pub use spin::{SpinLock, SpinLockGuard};
pub use spin_lazy::SpinLazy;
pub use spin_once::SpinOnce;
pub use spin_once_lock::SpinOnceLock;
