use crate::{STATE, uart_println};

pub fn test_runner(tests: &[&dyn Fn()]) -> ! {
    uart_println!("=== Running {} tests ===", tests.len());

    STATE.lock().test.as_mut().unwrap().shutdown(0)
}
