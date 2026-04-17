use core::{cell::Cell, ops::Deref};

use super::SpinOnceLock;

pub struct SpinLazy<T, F = fn() -> T> {
    once: SpinOnceLock<T>,
    init: Cell<Option<F>>,
}

impl<T, F> SpinLazy<T, F> {
    pub const fn new(f: F) -> SpinLazy<T, F> {
        SpinLazy {
            once: SpinOnceLock::new(),
            init: Cell::new(Some(f)),
        }
    }
}

impl<T, F> SpinLazy<T, F>
where
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
unsafe impl<T, F: Send> Sync for SpinLazy<T, F> where SpinOnceLock<T>: Sync {}

impl<T, F> Deref for SpinLazy<T, F>
where
    F: FnOnce() -> T,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        Self::force(self)
    }
}
