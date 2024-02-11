use core::{
    alloc::Layout,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use super::EarlyAllocator;

#[derive(Debug)]
pub enum EarlyAllocError {
    OutOfMemory,
}

pub struct Vec<'a, T: 'a> {
    data: NonNull<T>,
    len: usize,
    cap: usize,
    a: &'a EarlyAllocator<'a>,
}

impl<'a, T: 'a> core::fmt::Debug for Vec<'a, T>
where
    T: core::fmt::Debug,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_list().entries(self.deref()).finish()
    }
}

impl<'a, T: 'a> Deref for Vec<'a, T> {
    type Target = [T];

    fn deref(&self) -> &Self::Target {
        unsafe { core::slice::from_raw_parts(self.data.as_ptr(), self.len) }
    }
}

impl<'a, T: 'a> DerefMut for Vec<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { core::slice::from_raw_parts_mut(self.data.as_ptr(), self.len) }
    }
}

impl<'a, T: 'a> Vec<'a, T> {
    pub fn new(a: &'a EarlyAllocator<'a>) -> Self {
        Self {
            a,
            data: NonNull::dangling(),
            cap: 0,
            len: 0,
        }
    }

    fn ensure_capacity(&mut self, cap: usize) -> Result<(), EarlyAllocError> {
        if cap == 0 {
            return Ok(());
        }

        if self.cap == 0 {
            let data = self.a.alloc(
                Layout::from_size_align(
                    core::mem::size_of::<T>() * cap,
                    core::mem::align_of::<T>(),
                )
                .unwrap(),
            );

            if data.is_null() {
                return Err(EarlyAllocError::OutOfMemory);
            }

            self.data = NonNull::new(data as *mut _).unwrap();
            self.cap = cap;
        } else if self.cap < cap {
            let new_cap = if self.cap * 2 < cap {
                cap
            } else {
                self.cap * 2
            };

            let new_data = unsafe {
                self.a.realloc(
                    self.data.as_ptr() as *mut _,
                    Layout::from_size_align(
                        core::mem::size_of::<T>() * self.cap,
                        core::mem::align_of::<T>(),
                    )
                    .unwrap(),
                    core::mem::size_of::<T>() * new_cap,
                )
            };

            if new_data.is_null() {
                return Err(EarlyAllocError::OutOfMemory);
            }

            self.data = NonNull::new(new_data as *mut _).unwrap();
            self.cap = new_cap
        }

        Ok(())
    }

    pub fn push(&mut self, value: T) -> Result<(), EarlyAllocError> {
        self.ensure_capacity(self.len + 1)?;
        unsafe { self.data.as_ptr().add(self.len).write(value) }
        self.len += 1;
        Ok(())
    }
}

impl<'a, T: 'a> Drop for Vec<'a, T> {
    fn drop(&mut self) {
        for i in 0..self.len() {
            unsafe {
                self.data.as_ptr().add(i).drop_in_place();
            }
        }

        if self.cap != 0 {
            unsafe {
                self.a.dealloc(
                    self.data.as_ptr() as *mut _,
                    Layout::from_size_align(
                        core::mem::size_of::<T>() * self.cap,
                        core::mem::align_of::<T>(),
                    )
                    .unwrap(),
                )
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::early_alloc::collections::EarlyAllocError;
    use crate::early_alloc::EarlyAllocator;
    use crate::mem_test;

    use super::Vec;

    #[macro_rules_attribute::apply(mem_test)]
    fn push(a: &mut EarlyAllocator) {
        let mut v = Vec::new(a);
        v.push(42).unwrap();
        v.push(43).unwrap();
        v.push(44).unwrap();
        v.push(45).unwrap();
        assert_eq!(&*v, &[42, 43, 44, 45]);
    }

    #[macro_rules_attribute::apply(mem_test)]
    fn push_first_too_large(a: &mut EarlyAllocator) {
        let mut v = Vec::new(a);

        assert!(matches!(
            v.push([0u8; 130]),
            Err(EarlyAllocError::OutOfMemory)
        ));
    }

    #[macro_rules_attribute::apply(mem_test)]
    fn push_second_too_large(a: &mut EarlyAllocator) {
        let mut v = Vec::new(a);

        v.push([0u8; 100]).unwrap();

        assert!(matches!(
            v.push([0u8; 100]),
            Err(EarlyAllocError::OutOfMemory)
        ));
    }

    #[macro_rules_attribute::apply(mem_test)]
    fn drop_vec(a: &mut EarlyAllocator) {
        struct OnDrop<F: FnMut()>(F);

        impl<F: FnMut()> Drop for OnDrop<F> {
            fn drop(&mut self) {
                (self.0)()
            }
        }

        let mut dropped = false;
        let mut v = Vec::new(a);
        v.push(OnDrop(|| {
            dropped = true;
        }))
        .unwrap();
        drop(v);

        assert!(dropped);
    }
}
