use core::arch::asm;

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

type SbiResult<T> = Result<T, SbiError>;

fn sbi_ret<T>(error: isize, value: T) -> Result<T, SbiError> {
    match error {
        0 => Ok(value),
        -1 => Err(SbiError::Failed),
        -2 => Err(SbiError::NotSupported),
        -3 => Err(SbiError::InvalidParam),
        -4 => Err(SbiError::Denied),
        -5 => Err(SbiError::InvalidAddress),
        -6 => Err(SbiError::AlreadyAvailable),
        -7 => Err(SbiError::AlreadyStarted),
        -8 => Err(SbiError::AlreadyStopped),
        -9 => Err(SbiError::NoShmem),
        _ => Err(SbiError::Unknown(error)),
    }
}

const DBCN_EID: u64 = 0x4442434E;

pub struct DebugConsole;

impl core::fmt::Write for DebugConsole {
    fn write_str(&mut self, mut s: &str) -> core::fmt::Result {
        while !s.is_empty() {
            match sbi_debug_console_write(s.as_bytes()) {
                Ok(n) => s = &s[n..],
                Err(_) => return Err(core::fmt::Error),
            }
        }

        Ok(())
    }
}

pub fn sbi_debug_console_write(data: &[u8]) -> SbiResult<usize> {
    let written_len;
    let status;

    unsafe {
        asm!(
            "ecall",
            in("a7") DBCN_EID,
            in("a6") 0,
            in("a0") data.len(),
            in("a1") data.as_ptr(),
            in("a2") 0,
            lateout("a0") status,
            lateout("a1") written_len,
        );
    }

    sbi_ret(status, written_len)
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
