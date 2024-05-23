use core::{
    fmt::Debug,
    ops::{Deref, DerefMut},
};

use super::EarlyAllocator;

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
    pub unsafe fn from_raw(raw: *mut T) -> Self {
        Self(&mut *raw)
    }
}

impl<'a, T> Box<'a, T> {
    #[allow(unused)]
    pub fn new(v: T, a: &'a EarlyAllocator<'a>) -> Option<Self> {
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
    use crate::{
        early_alloc::{boxed::Box, EarlyAllocator},
        mem_test,
    };

    #[macro_rules_attribute::apply(mem_test)]
    fn drop_box(a: &mut EarlyAllocator) {
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
