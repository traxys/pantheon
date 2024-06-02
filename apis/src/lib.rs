//! Apis is a bump allocator, heavily inspired in it's API by the bumpallo crate
//!
//! Apis is an egyptian god taking the form of a bull, in order to stay in the bumpallo theme
#![no_std]

use core::{
    alloc::Layout, cell::RefCell, cmp::Ordering, marker::PhantomData, mem::MaybeUninit,
    ptr::NonNull,
};

/// Owned pointers in [Allocator]
pub mod boxed;
/// Collections using [Allocator]
pub mod collections;

/// Bump allocator, stores a region of memory to give out pointers.
///
/// Only the last allocation may be de-allocated or re-allocated in place.
pub struct Allocator<'a> {
    inner: RefCell<AllocatorInner>,
    _life: PhantomData<&'a mut [MaybeUninit<u8>]>,
}

struct AllocatorInner {
    backing: *mut u8,
    len: usize,
    current: usize,
    last: Option<NonNull<u8>>,
}

impl<'a> Allocator<'a> {
    /// Initialize an allocator from a memory region
    pub fn new(backing: &'a mut [MaybeUninit<u8>]) -> Self {
        Self {
            inner: RefCell::new(AllocatorInner {
                backing: backing.as_mut_ptr() as *mut _,
                len: backing.len(),
                current: 0,
                last: None,
            }),
            _life: PhantomData,
        }
    }

    /// Returns the amount of space still available in the [Allocator](Self)
    pub fn available(&self) -> usize {
        let s = self.inner.borrow();

        s.len - s.current
    }

    /// Allocate a new pointer from the bump allocator, given the layout.
    ///
    /// This pointer may be freed with [dealloc](Self::dealloc)
    pub fn alloc(&self, layout: Layout) -> *mut u8 {
        let mut s = self.inner.borrow_mut();

        // SAFETY: all memory from &s.backing[s.current] is not yet allocated
        unsafe {
            let start = s.backing.add(s.current);

            let offset = start.align_offset(layout.align());
            if s.current + offset + layout.size() > s.len {
                return core::ptr::null_mut();
            }

            let aligned_start = start.add(offset);

            s.last = Some(NonNull::new_unchecked(aligned_start));
            s.current += layout.size();
            aligned_start
        }
    }

    /// Allocate a slot in the [Allocator](Self) for the value T, and return a pointer to it.
    pub fn alloc_val<T>(&self, v: T) -> Option<&mut T> {
        let layout = Layout::new::<T>();

        let p = self.alloc(layout) as *mut T;
        if p.is_null() {
            return None;
        } else {
            unsafe {
                p.write(v);
                Some(&mut *p)
            }
        }
    }

    /// Free a pointer previously allocated with [alloc](Self::alloc)
    ///
    /// The memory allocated will only be made available again if no other memory was allocated
    /// since the allocation of _ptr_.
    ///
    /// # SAFETY
    /// The pointer **must** have been allocated with [alloc](Self::alloc)
    pub unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        let mut s = self.inner.borrow_mut();

        if Some(ptr) == s.last.map(|p| p.as_ptr()) {
            s.last = None;
            s.current -= layout.size();
        }
    }

    /// Re-allocate a pointer previously allocated with [alloc](Self::alloc)
    ///
    /// This will copy the data from the old allocation to the new allocation.
    /// If no other allocation was performed since _old_ was allocated the allocation is increased
    /// in place.
    ///
    /// The returned pointer has the same characteristics as a pointer allocated with
    /// [alloc](Self::alloc).
    ///
    /// # SAFETY
    /// The pointer **must** have been allocated with [alloc](Self::alloc)
    pub unsafe fn realloc(&self, old: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        let mut s = self.inner.borrow_mut();

        match s.last {
            Some(p) if p.as_ptr() == old => {
                match new_size.cmp(&layout.size()) {
                    Ordering::Less => s.current -= layout.size() - new_size,
                    Ordering::Equal => (),
                    Ordering::Greater => {
                        let added = new_size - layout.size();
                        if s.current + added >= s.len {
                            return core::ptr::null_mut();
                        } else {
                            s.current += new_size - layout.size();
                        }
                    }
                };
                old
            }
            _ => {
                drop(s);
                // SAFETY: the caller must ensure that the `new_size` does not overflow.
                // `layout.align()` comes from a `Layout` and is thus guaranteed to be valid.
                let new_layout =
                    unsafe { Layout::from_size_align_unchecked(new_size, layout.align()) };
                let new_ptr = self.alloc(new_layout);
                if !new_ptr.is_null() {
                    // SAFETY: the previously allocated block cannot overlap the newly allocated block.
                    // The safety contract for `dealloc` must be upheld by the caller.
                    unsafe {
                        core::ptr::copy_nonoverlapping(
                            old,
                            new_ptr,
                            core::cmp::min(layout.size(), new_size),
                        );
                        self.dealloc(old, layout);
                    }
                }
                new_ptr
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::Allocator;
    use core::alloc::Layout;

    #[macro_export]
    macro_rules! mem_test {
        (fn $name:ident($a:ident: &mut Allocator) { $($body:tt)* }) => {
            #[test]
            fn $name() {
                let mut mem = [core::mem::MaybeUninit::uninit(); 128];
                let $a = &mut Allocator::new(&mut mem);

                $($body)*
            }
        };
    }

    #[macro_rules_attribute::apply(mem_test)]
    fn allocate(a: &mut Allocator) {
        a.alloc(Layout::from_size_align(1, 1).unwrap());

        assert_eq!(a.available(), 127);
    }

    #[macro_rules_attribute::apply(mem_test)]
    fn allocate_all(a: &mut Allocator) {
        for _ in 0..128 {
            assert!(!a.alloc(Layout::from_size_align(1, 1).unwrap()).is_null());
        }

        assert!(a.alloc(Layout::from_size_align(1, 1).unwrap()).is_null());
    }

    #[macro_rules_attribute::apply(mem_test)]
    fn deallocate_last(a: &mut Allocator) {
        let layout = Layout::from_size_align(64, 1).unwrap();
        let ptr = a.alloc(layout);
        assert_eq!(a.available(), 64);

        unsafe { a.dealloc(ptr, layout) };

        assert_eq!(a.available(), 128);
    }

    #[macro_rules_attribute::apply(mem_test)]
    fn reallocate_non_last(a: &mut Allocator) {
        let layout = Layout::from_size_align(32, 1).unwrap();
        let ptr = a.alloc(layout);

        let data = b"abcdefghijklmnopqrstuvwxyz012345";
        unsafe {
            core::ptr::copy_nonoverlapping(data.as_ptr(), ptr, 32);
        }

        a.alloc(layout);

        unsafe {
            let new = a.realloc(ptr, layout, 64);
            assert_eq!(core::slice::from_raw_parts(new, 32), data);
            assert_eq!(a.available(), 0);
        };
    }

    #[macro_rules_attribute::apply(mem_test)]
    fn reallocate_last(a: &mut Allocator) {
        let layout = Layout::from_size_align(32, 1).unwrap();
        let ptr = a.alloc(layout);
        unsafe {
            a.realloc(ptr, layout, 64);
        }
        assert_eq!(a.available(), 64);
    }

    #[macro_rules_attribute::apply(mem_test)]
    fn reallocate_last_shrink(a: &mut Allocator) {
        let layout = Layout::from_size_align(64, 1).unwrap();
        let ptr = a.alloc(layout);
        unsafe {
            a.realloc(ptr, layout, 32);
        }
        assert_eq!(a.available(), 96);
    }
}
