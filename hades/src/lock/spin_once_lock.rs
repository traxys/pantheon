use core::{cell::UnsafeCell, marker::PhantomData, mem::MaybeUninit};

use super::SpinOnce;

/// Modeled on the standard library's OnceLock
pub struct SpinOnceLock<T> {
    once: SpinOnce,
    value: UnsafeCell<MaybeUninit<T>>,
    _marker: PhantomData<T>,
}

impl<T> SpinOnceLock<T> {
    pub const fn new() -> Self {
        Self {
            once: SpinOnce::new(),
            value: UnsafeCell::new(MaybeUninit::uninit()),
            _marker: PhantomData,
        }
    }

    fn is_initialized(&self) -> bool {
        self.once.is_completed()
    }

    pub fn get(&self) -> Option<&T> {
        if self.is_initialized() {
            // SAFETY: is_initialized was checked
            Some(unsafe { self.get_unchecked() })
        } else {
            None
        }
    }

    pub fn get_or_init<F>(&self, f: F) -> &T
    where
        F: FnOnce() -> T,
    {
        if let Some(v) = self.get() {
            return v;
        }
        self.initialize(f);
        debug_assert!(self.is_initialized());
        unsafe { self.get_unchecked() }
    }

    #[cold]
    fn initialize<F>(&self, f: F)
    where
        F: FnOnce() -> T,
    {
        let slot = &self.value;

        self.once.call_once(|| {
            let v = f();
            unsafe { (&mut *slot.get()).write(v) };
        })
    }

    pub unsafe fn get_unchecked(&self) -> &T {
        debug_assert!(self.is_initialized());
        (&*self.value.get()).assume_init_ref()
    }
}

unsafe impl<T: Sync + Send> Sync for SpinOnceLock<T> {}
unsafe impl<T: Send> Send for SpinOnceLock<T> {}
