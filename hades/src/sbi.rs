#![allow(unused)]

use core::arch::asm;

use crate::virt_to_phys;

#[repr(isize)]
pub enum SbiError {
    Failed = -1,
    NotSupported = -2,
    InvalidParam = -3,
    Denied = -4,
    InvalidAddress = -5,
    AlreadyAvailable = -6,
    AlreadyStarted = -7,
    AlreadyStopped = -8,
    NoShmem = -9,
    Unknown(isize) = -10,
}

impl SbiError {
    fn parse(error: isize) -> Option<Self> {
        match error {
            0 => None,
            -1 => Some(SbiError::Failed),
            -2 => Some(SbiError::NotSupported),
            -3 => Some(SbiError::InvalidParam),
            -4 => Some(SbiError::Denied),
            -5 => Some(SbiError::InvalidAddress),
            -6 => Some(SbiError::AlreadyAvailable),
            -7 => Some(SbiError::AlreadyStarted),
            -8 => Some(SbiError::AlreadyStopped),
            -9 => Some(SbiError::NoShmem),
            _ => Some(SbiError::Unknown(error)),
        }
    }
}

type SbiResult<T> = Result<T, SbiError>;

fn sbi_ret<T>(error: isize, value: T) -> Result<T, SbiError> {
    match SbiError::parse(error) {
        None => Ok(value),
        Some(e) => Err(e),
    }
}

const DBCN_EID: u64 = 0x4442434E;

pub struct DebugConsole;

impl core::fmt::Write for DebugConsole {
    fn write_str(&mut self, mut s: &str) -> core::fmt::Result {
        while !s.is_empty() {
            let addr = virt_to_phys(s);
            match sbi_debug_console_write(addr as u64, s.len()) {
                Ok(n) => s = &s[n..],
                Err(_) => return Err(core::fmt::Error),
            }
        }

        Ok(())
    }
}

pub fn sbi_debug_console_write(start: u64, len: usize) -> SbiResult<usize> {
    let written_len;
    let status;

    unsafe {
        asm!(
            "ecall",
            in("a7") DBCN_EID,
            in("a6") 0,
            in("a0") len,
            in("a1") start,
            in("a2") 0,
            lateout("a0") status,
            lateout("a1") written_len,
        );
    }

    sbi_ret(status, written_len)
}

const SRST: usize = 0x53525354;

fn sbi_system_reset(reset_type: u32, reason: u32) -> ! {
    let status: isize;

    unsafe {
        asm!(
            "ecall",
            in("a7") SRST,
            in("a6") 0,
            in("a0") reset_type,
            in("a1") reason,
            lateout("a0") status,
            lateout("a1") _,
        )
    };

    panic!("Reset failed: {status}")
}

pub fn sbi_shutdown() -> ! {
    sbi_system_reset(0x00000000, 0x00000000)
}

pub fn sbi_panic() -> ! {
    sbi_system_reset(0x00000000, 0x00000001)
}

// pub unsafe fn sbi_call(eid: i32, fid: i32) -> Result<i64, SbiError> {
//     let error: i64;
//     let value: i64;
//
//     asm!(
//         "ecall",
//         in("a7") eid,
//         in("a6") fid,
//         out("a0") error,
//         out("a1") value,
//     );
// }
