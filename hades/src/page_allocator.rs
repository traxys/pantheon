use core::{
    cell::UnsafeCell,
    ops::Deref,
    sync::atomic::{self, AtomicBool, AtomicU16, AtomicU32},
};

use crate::{arch::PAGE_SIZE, lock::SpinLock, RAM_VIRTUAL_START};

pub const MAX_ORDER: usize = 12;

// SAFETY: a mutable reference to a free list can only be obtained with a lock on the page
// allocator
#[derive(Clone, Copy)]
struct FreeList {
    head: Option<&'static Page>,
}

impl FreeList {
    fn add(&mut self, page: &'static Page) {
        let state = unsafe { &mut (*page.state.get()).free };
        state.next = self.head;
        self.head = Some(page);
    }

    fn remove(&mut self, page: &Page) -> Option<&'static Page> {
        let mut prev: Option<&'static Page> = None;
        let mut current = self.head?;

        loop {
            if core::ptr::eq(current, page) {
                match prev {
                    Some(page) => unsafe {
                        (*page.state.get()).free.next = (*current.state.get()).free.next;
                    },
                    None => self.head = unsafe { (*current.state.get()).free.next },
                }

                return Some(current);
            } else {
                prev = Some(current);
                current = unsafe { (*current.state.get()).free.next? };
            }
        }
    }
}

impl core::fmt::Debug for FreeList {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut l = f.debug_list();

        let mut current = self.head;
        while let Some(page) = current {
            l.entry(&format_args!("{:p}", page));
            unsafe {
                current = (*page.state.get()).free.next;
            }
        }

        l.finish()
    }
}

impl core::fmt::Debug for PageAllocator {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_map().entries(self.free.iter().enumerate()).finish()
    }
}

#[derive(Clone, Copy)]
struct FreePageState {
    next: Option<&'static Page>,
}

pub union PageState {
    free: FreePageState,
}

#[derive(Debug)]
#[non_exhaustive]
pub struct Page {
    pub ref_count: AtomicU32,
    pub state: UnsafeCell<PageState>,
    // Relaxed operations are sufficient on this atomic, as it is only manipulated by the page
    // allocator
    allocated: AtomicU16,
}

unsafe impl Send for Page {}
unsafe impl Sync for Page {}

impl Page {
    fn index(&self, pages: &[Page]) -> usize {
        unsafe { (self as *const Page).offset_from(pages.as_ptr()) as usize }
    }
}

pub struct PageAllocator {
    free: [FreeList; MAX_ORDER + 1],
    pages: &'static [Page],
}

unsafe impl Send for PageAllocator {}

static PAGE_ALLOCATOR: SpinLock<PageAllocator> = SpinLock::new(PageAllocator {
    free: [FreeList { head: None }; MAX_ORDER + 1],
    pages: &[],
});

fn align(v: usize, exp: usize) -> usize {
    assert!(exp.is_power_of_two());

    (v + exp - 1) & !(exp - 1)
}

/// SAFETY: `start` must point to currently _unused_ physical memory
///
/// This function may only be called once
pub unsafe fn init(start: usize, mut length: usize) {
    static INIT: AtomicBool = AtomicBool::new(false);

    assert!(!INIT.load(atomic::Ordering::Relaxed));

    let max_page_size = PAGE_SIZE * (1 << MAX_ORDER);

    let start_aligned = align(start, max_page_size);

    // Remove start pages that are not aligned
    length -= start_aligned - start;
    // Remove end pages that don't fit in a max_page_size
    length -= length % max_page_size;

    let max_page_count = length / max_page_size;

    let page_count = length / PAGE_SIZE;
    let page_info_size = page_count * core::mem::size_of::<Page>();
    let required_pages_for_page_struct = page_info_size.div_ceil(max_page_size);

    let pages: &[Page] = unsafe {
        core::slice::from_raw_parts(
            (start_aligned + RAM_VIRTUAL_START as usize) as *const _,
            page_count,
        )
    };

    let mut free_pages = [FreeList { head: None }; MAX_ORDER + 1];
    for i in (required_pages_for_page_struct..max_page_count).rev() {
        let page_idx = i * (1 << MAX_ORDER);

        free_pages[MAX_ORDER].add(&pages[page_idx]);
    }

    let mut allocator = PAGE_ALLOCATOR.lock();

    allocator.free = free_pages;
    allocator.pages = pages;

    INIT.store(true, atomic::Ordering::Relaxed);
}

impl PageAllocator {
    fn alloc(&mut self, order: usize) -> Option<&'static [Page]> {
        match self.free[order].head.take() {
            Some(h) => {
                self.free[order].head = unsafe { (*h.state.get()).free.next };

                h.allocated.fetch_or(1 << order, atomic::Ordering::Relaxed);

                Some(&self.pages[h.index(self.pages)..h.index(self.pages) + (1 << order)])
            }
            None => {
                if order == MAX_ORDER {
                    None
                } else {
                    let larger = self.alloc(order + 1)?;
                    let buddy = &larger[larger.len() / 2];
                    self.free[order].add(buddy);

                    larger[0]
                        .allocated
                        .fetch_or(1 << order, atomic::Ordering::Relaxed);

                    Some(&larger[0..larger.len() / 2])
                }
            }
        }
    }

    fn free(&mut self, pages: &'static [Page]) {
        let order = pages.len().trailing_zeros() as usize;

        // Mark the page as free
        let prev = pages[0]
            .allocated
            .fetch_and(!(1 << order), atomic::Ordering::Relaxed);
        assert!(prev & (1 << order) != 0);

        // If it's a max order page no merging to be done
        if order < MAX_ORDER {
            let index = pages[0].index(self.pages);
            let (first_idx, buddy_index) = match index % (1 << (order + 1)) {
                0 => (index, index + (1 << order)),
                _ => {
                    let first = index - (1 << order);
                    (first, first)
                }
            };

            let buddy = &self.pages[buddy_index];
            let buddy_allocation = buddy.allocated.load(atomic::Ordering::Relaxed);

            if buddy_allocation & (1 << order) == 0 {
                self.free[order]
                    .remove(buddy)
                    .expect("buddy was not in the free list while it was un-allocated");

                buddy
                    .allocated
                    .fetch_and(!(1 << order), atomic::Ordering::Relaxed);

                return self.free(&self.pages[first_idx..first_idx + (1 << (order + 1))]);
            }
        }

        self.free[order].add(&pages[0]);
    }
}

pub struct PageAllocation(&'static [Page]);

impl Deref for PageAllocation {
    type Target = [Page];

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl Drop for PageAllocation {
    fn drop(&mut self) {
        PAGE_ALLOCATOR.lock().free(self.0);
    }
}

pub fn alloc_pages(order: usize) -> Option<PageAllocation> {
    if order > MAX_ORDER {
        return None;
    }

    let pages = PAGE_ALLOCATOR.lock().alloc(order)?;

    Some(PageAllocation(pages))
}

#[cfg(test)]
mod test {
    use crate::{
        hades_test,
        page_allocator::{alloc_pages, MAX_ORDER},
    };

    #[macro_rules_attribute::apply(hades_test)]
    fn pg_alloc_max_order() {
        let page = alloc_pages(MAX_ORDER).unwrap();

        let first_page = &page.0[0];

        assert!(
            first_page
                .allocated
                .load(core::sync::atomic::Ordering::Relaxed)
                & 1 << MAX_ORDER
                != 0
        );

        drop(page);

        assert!(
            first_page
                .allocated
                .load(core::sync::atomic::Ordering::Relaxed)
                & 1 << MAX_ORDER
                == 0
        );
    }

    #[macro_rules_attribute::apply(hades_test)]
    fn pg_alloc_split_max_order() {
        let page = alloc_pages(MAX_ORDER - 1).unwrap();

        let first_page = &page.0[0];
        let allocated = first_page
            .allocated
            .load(core::sync::atomic::Ordering::Relaxed);

        assert!(allocated & 1 << (MAX_ORDER - 1) != 0);
        assert!(allocated & 1 << MAX_ORDER != 0);

        drop(page);

        let allocated = first_page
            .allocated
            .load(core::sync::atomic::Ordering::Relaxed);

        assert!(allocated & 1 << (MAX_ORDER - 1) == 0);
        assert!(allocated & 1 << MAX_ORDER == 0);
    }
}
