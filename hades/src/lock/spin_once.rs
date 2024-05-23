use core::sync::atomic::{
    AtomicU8,
    Ordering::{Acquire, Release},
};

use crate::arch::SieGuard;

const INCOMLETE: u8 = 0;
const RUNNING: u8 = 1;
const COMPLETE: u8 = 2;

pub struct SpinOnce {
    state: AtomicU8,
}

impl SpinOnce {
    pub const fn new() -> Self {
        Self {
            state: AtomicU8::new(INCOMLETE),
        }
    }

    pub fn is_completed(&self) -> bool {
        self.state.load(Acquire) == COMPLETE
    }

    pub fn call_once<F>(&self, f: F)
    where
        F: FnOnce(),
    {
        if self.is_completed() {
            return;
        }

        let mut f = Some(f);
        // Make this a FnMut (not really though!)
        self.call(&mut || f.take().unwrap()());
    }

    #[cold]
    fn call(&self, f: &mut impl FnMut()) {
        let mut state = self.state.load(Acquire);
        loop {
            match state {
                INCOMLETE => {
                    let _guard = SieGuard::new();

                    if let Err(new) = self
                        .state
                        .compare_exchange_weak(state, RUNNING, Acquire, Acquire)
                    {
                        state = new;
                        continue;
                    }

                    f();

                    self.state.store(COMPLETE, Release);
                    return;
                }
                RUNNING => {
                    if self.state.load(Acquire) == COMPLETE {
                        return;
                    }

                    core::hint::spin_loop();
                }
                COMPLETE => return,
                _ => unreachable!("invalid state {state} for SpinOnce"),
            }
        }
    }
}
