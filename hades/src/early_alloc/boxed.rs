use core::{
    fmt::Debug,
    ops::{Deref, DerefMut},
};

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
