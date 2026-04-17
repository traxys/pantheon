#![no_std]

pub const SIE: usize = 1 << 1;
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

struct SieGuard(usize);

impl SieGuard {
    pub fn new() -> Self {
        Self(unsafe { csr_read_clear!(sstatus, SIE) })
    }
}

impl Drop for SieGuard {
    fn drop(&mut self) {
        unsafe { csr_set!(sstatus, SIE & self.0) }
    }
}

mod spin;
mod spin_lazy;
mod spin_once;
mod spin_once_lock;

pub use spin::{SpinLock, SpinLockGuard};
pub use spin_lazy::SpinLazy;
pub use spin_once::SpinOnce;
pub use spin_once_lock::SpinOnceLock;
