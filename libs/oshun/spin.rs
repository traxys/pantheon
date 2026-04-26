use core::{
    cell::UnsafeCell,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    sync::atomic::{AtomicBool, Ordering},
};

use crate::{InterruptGuard, PrivilegeMode};

pub struct SpinLock<M, T> {
    locked: AtomicBool,
    inner: UnsafeCell<T>,
    mode: PhantomData<M>,
}

unsafe impl<T: Send, M> Send for SpinLock<M, T> {}
unsafe impl<T: Send, M> Sync for SpinLock<M, T> {}

pub struct SpinLockGuard<'a, M, T>
where
    M: PrivilegeMode,
{
    lock: &'a SpinLock<M, T>,
    _sie: InterruptGuard<M>,
}

impl<'a, M, T> Deref for SpinLockGuard<'a, M, T>
where
    M: PrivilegeMode,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.lock.inner.get() }
    }
}

impl<'a, M, T> DerefMut for SpinLockGuard<'a, M, T>
where
    M: PrivilegeMode,
{
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.lock.inner.get() }
    }
}

impl<'a, M, T> Drop for SpinLockGuard<'a, M, T>
where
    M: PrivilegeMode,
{
    fn drop(&mut self) {
        self.lock.locked.store(false, Ordering::Release);
    }
}

impl<M, T> SpinLock<M, T>
where
    M: PrivilegeMode,
{
    pub const fn new(v: T) -> Self {
        Self {
            locked: AtomicBool::new(false),
            inner: UnsafeCell::new(v),
            mode: PhantomData,
        }
    }

    pub fn lock(&self) -> SpinLockGuard<'_, M, T> {
        let _sie = InterruptGuard::new();

        while self
            .locked
            .compare_exchange(false, true, Ordering::Acquire, Ordering::Relaxed)
            .is_err()
        {
            core::hint::spin_loop()
        }

        SpinLockGuard { lock: self, _sie }
    }
}
