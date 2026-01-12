use core::{
    fmt::Debug,
    ops::{Deref, DerefMut},
};

use super::Allocator;

/// A owned value in an [Allocator].
///
/// This value will be deallocated on [drop](Self::drop).
#[repr(transparent)]
pub struct Box<'a, T: ?Sized>(&'a mut T);

impl<'a, T: ?Sized> Deref for Box<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'a, T: ?Sized> DerefMut for Box<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

impl<'a, T: Debug + ?Sized> Debug for Box<'a, T> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        Debug::fmt(&**self, f)
    }
}

impl<'a, T: ?Sized> Box<'a, T> {
    /// Convert a raw pointer to a [Box].
    ///
    /// # SAFETY
    /// The pointer must come from a value allocated with [Box::new].
    pub unsafe fn from_raw(raw: *mut T) -> Self {
        Self(unsafe { &mut *raw })
    }
}

impl<'a, T> Box<'a, T> {
    /// Create a [Box] from a value
    pub fn new(v: T, a: &'a Allocator<'a>) -> Option<Self> {
        a.alloc_val(v).map(Self)
    }
}

impl<'a, T: ?Sized> Drop for Box<'a, T> {
    fn drop(&mut self) {
        unsafe { core::ptr::drop_in_place(self.0) }
    }
}

#[cfg(test)]
mod test {
    use crate::mem_test;
    use crate::{boxed::Box, Allocator};

    #[macro_rules_attribute::apply(mem_test)]
    fn drop_box(a: &mut Allocator) {
        struct OnDrop<F: FnMut()>(F);

        impl<F: FnMut()> Drop for OnDrop<F> {
            fn drop(&mut self) {
                (self.0)()
            }
        }

        let mut dropped = false;
        let v = Box::new(
            OnDrop(|| {
                dropped = true;
            }),
            a,
        )
        .unwrap();
        drop(v);

        assert!(dropped);
    }
}
