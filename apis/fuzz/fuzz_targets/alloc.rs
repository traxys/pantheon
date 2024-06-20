#![no_main]

use std::alloc::Layout;
use std::mem::MaybeUninit;

use libfuzzer_sys::arbitrary::{self, Arbitrary};
use libfuzzer_sys::fuzz_target;

#[derive(Arbitrary, Debug, Clone, Copy)]
enum Align {
    One = 1,
    Two = 2,
    Four = 4,
    Eight = 8,
    Sixteen = 16,
    ThirtyTwo = 32,
}

#[derive(Arbitrary, Debug, Clone, Copy)]
struct AllocLayout {
    size: u16,
    align: Align,
}

impl Into<Layout> for AllocLayout {
    fn into(self) -> Layout {
        Layout::from_size_align(self.size as _, self.align as _).unwrap()
    }
}

#[derive(Arbitrary, Debug)]
enum AllocatorMethod {
    Malloc(AllocLayout),
    Free {
        index: usize,
    },
    Realloc {
        index: usize,
        new_layout: AllocLayout,
    },
}

fn range_overlap(a: usize, b: usize, x: usize, y: usize) -> bool {
    let (smallest, largest) = if a < x {
        ((a, b), (x, y))
    } else {
        ((x, y), (a, b))
    };

    largest.0 <= smallest.1
}

fn check_overlap(
    start: usize,
    allocs: &[(*mut u8, AllocLayout)],
    (new_ptr, new_layout): (*mut u8, AllocLayout),
    skip: Option<usize>,
) {
    let r = |ptr, layout: AllocLayout| {
        (
            ptr as usize - start,
            (ptr as usize) - start + layout.size as usize - 1,
        )
    };

    let (new_start, new_end) = r(new_ptr, new_layout);
    if new_start == new_end {
        return;
    }

    allocs
        .iter()
        .enumerate()
        .filter(|(idx, _)| Some(*idx) != skip)
        .for_each(|(_, &(ptr, layout))| {
            let (start, end) = r(ptr, layout);

            assert!(!range_overlap(new_start, new_end, start, end));
        })
}

fuzz_target!(|methods: Vec<AllocatorMethod>| {
    let mut allocs = vec![];
    let mut backing = vec![MaybeUninit::uninit(); 256 * 1024 * 1024];
    let start = backing.as_ptr() as usize;
    let alloc = apis::Allocator::new(&mut backing);

    for method in methods {
        match method {
            AllocatorMethod::Malloc(layout) => {
                let ptr = alloc.alloc(layout.into());
                if ptr.is_null() {
                    continue;
                }
                check_overlap(start, &allocs, (ptr, layout), None);
                allocs.push((ptr, layout.into()));
            }
            AllocatorMethod::Free { index } => {
                if index >= allocs.len() {
                    continue;
                }
                let (ptr, layout) = allocs.remove(index);
                unsafe { alloc.dealloc(ptr, layout.into()) }
            }
            AllocatorMethod::Realloc { index, new_layout } => {
                if let Some((ptr, layout)) = allocs.get_mut(index) {
                    let new = unsafe { alloc.realloc(*ptr, (*layout).into(), new_layout.into()) };
                    if new.is_null() {
                        continue;
                    }
                    *layout = new_layout.into();
                    *ptr = new;
                    check_overlap(start, &allocs, (new, new_layout), Some(index));
                }
            }
        }
    }
});
