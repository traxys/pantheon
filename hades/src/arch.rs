pub const SIE: usize = 1 << 1;
pub const PAGE_SHIFT: usize = 12;
pub const PAGE_SIZE: usize = 1 << PAGE_SHIFT;

macro_rules! csr_read_clear {
    ($name:ident, $mask:expr) => {{
        let v;
        asm!(concat!("csrrc {0}, ", stringify!($name), ", {1}"), in(reg) $mask, out(reg) v);
        v
    }};
}

macro_rules! csr_set {
    ($name:ident, $value:expr) => {
        asm!(concat!("csrs ", stringify!($name), ", {0}"), in(reg) $value)
    };
}

pub(crate) use csr_read_clear;
pub(crate) use csr_set;
