#![no_std]

use oshun::{NoIrqMode, SpinOnceLock};
use wohpe::{Filter, FilterParseError, Logger};

pub use wohpe;

#[unsafe(no_mangle)]
#[used]
pub static WOHPE_DIRECTIVE_INITIAL: usize = 0xfff00000;

#[unsafe(no_mangle)]
#[used]
pub static WOHPE_DIRECTIVE_SIZE: usize = 4096;

///
/// This function will only have an effect the first time it is called
///
/// # SAFETY
///
/// WOHPE_DIRECTIVE_INITIAL must point to a null terminated string
pub unsafe fn init(logger: &'static (dyn Logger + Send + Sync)) -> Result<(), FilterParseError> {
    static DIRECTIVES: SpinOnceLock<NoIrqMode, [u8; WOHPE_DIRECTIVE_SIZE]> = SpinOnceLock::new();

    // Already initialized
    if DIRECTIVES.get().is_some() {
        return Ok(());
    }

    let directives = DIRECTIVES.get_or_init(|| {
        wohpe::set_logger(logger);

        let mut directives = [0; WOHPE_DIRECTIVE_SIZE];

        unsafe {
            core::ptr::copy_nonoverlapping(
                WOHPE_DIRECTIVE_INITIAL as *const u8,
                directives.as_mut_ptr(),
                WOHPE_DIRECTIVE_SIZE,
            );
        }

        directives
    });

    let Ok(directives) = core::ffi::CStr::from_bytes_until_nul(directives) else {
        return Err(FilterParseError);
    };

    let Ok(directives) = directives.to_str() else {
        return Err(FilterParseError);
    };

    if directives.is_empty() {
        wohpe::append_filter(Filter::level(wohpe::LogLevel::Warn));
    } else {
        wohpe::parse_directives(directives)?;
    }

    Ok(())
}
