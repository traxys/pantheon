use core::{cell::Cell, ops::Deref};

use crate::PrivilegeMode;

use super::SpinOnceLock;

pub struct SpinLazy<M, T, F = fn() -> T> {
    once: SpinOnceLock<M, T>,
    init: Cell<Option<F>>,
}

impl<M: PrivilegeMode, T, F> SpinLazy<M, T, F> {
    pub const fn new(f: F) -> SpinLazy<M, T, F> {
        SpinLazy {
            once: SpinOnceLock::new(),
            init: Cell::new(Some(f)),
        }
    }
}

impl<M, T, F> SpinLazy<M, T, F>
where
    M: PrivilegeMode,
    F: FnOnce() -> T,
{
    pub fn force(this: &Self) -> &T {
        this.once.get_or_init(|| {
            this.init
                .take()
                .expect("we should only init the value once")()
        })
    }
}

// No &F is created from a &SpinLazy<T, F>
unsafe impl<M, T, F: Send> Sync for SpinLazy<M, T, F> where SpinOnceLock<M, T>: Sync {}

impl<M, T, F> Deref for SpinLazy<M, T, F>
where
    M: PrivilegeMode,
    F: FnOnce() -> T,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        Self::force(self)
    }
}
