/// An error returned when trying to allocate some memory
#[derive(Debug)]
pub enum ApisError {
    /// No enough memory in the [Allocator](crate::Allocator) for the operation
    OutOfMemory,
}

mod vec;
pub use vec::Vec;
