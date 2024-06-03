/// An error returned when trying to allocate some memory
#[derive(Debug)]
pub enum ApisError {
    /// No enough memory in the [Allocator](crate::Allocator) for the operation
    OutOfMemory,
    /// Could not format the [String]. Probably due to a [ApisError::OutOfMemory]
    Fmt,
}

mod vec;
mod string;
pub use vec::Vec;
pub use string::String;
