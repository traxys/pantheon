use core::fmt::Write;

use ogma::DeviceTreeNode;

use crate::{STATE, SbiRet, SbiStatus};

pub struct Uart(*mut u8);

pub fn sbi_handler(function_id: usize, a0: usize, a1: usize, a2: usize) -> Option<SbiRet> {
    Some(match function_id {
        0 /* write bytes */ => {
            let num_bytes = a0;
            let base_addr_lo = a1;
            let base_addr_hi = a2;

            if base_addr_hi != 0 {
                /* Only 64 bits of physical memory is supported */
                return Some(SbiRet::err(SbiStatus::InvalidAddress));
            }

            let mut state = STATE.lock();
            let Some(uart) = state.uart.as_mut() else {
                return Some(SbiRet::err(SbiStatus::Denied));
            };

            let addr = base_addr_lo as *const u8;
            for i in 0..num_bytes {
                let byte = unsafe { *addr.add(i) };
                uart.send_byte(byte);
            }

            SbiRet::ok(num_bytes)
        }
        2 /* write byte */ => {
            let byte = a0;

            let mut state = STATE.lock();
            let Some(uart) = state.uart.as_mut() else {
                return Some(SbiRet::err(SbiStatus::Denied));
            };

            uart.send_byte(byte as u8);

            SbiRet::ok(0)
        },
        _ => return None,
    })
}

// SAFTEY: The uart may be accessed from any thread
unsafe impl Send for Uart {}

impl Uart {
    // const RBR: u8 = 0;
    const THR: u8 = 0;
    // const IER: u8 = 1;
    // const IIR: u8 = 2;
    const FCR: u8 = 2;
    const LCR: u8 = 3;
    // const MCR: u8 = 4;
    const LSR: u8 = 5;
    // const MSR: u8 = 6;
    // const SCR: u8 = 7;
    const DLL: u8 = 0;
    const DLM: u8 = 1;

    const BAUD_RATE: u32 = 115200;

    /// # SAFETY
    ///
    /// The device tree node must point to a valid UART
    pub unsafe fn new(node: &DeviceTreeNode) -> Option<Self> {
        if !node.compatible().unwrap().contains(&"ns16550a") {
            // Invalid UART node
            return None;
        }

        let freq = u32::from_be_bytes(node.raw_prop("clock-frequency")?.try_into().ok()?);
        let dla = freq / (16 * Uart::BAUD_RATE);
        let reg = node.reg()?[0];

        let mut this = Self(reg.address as *mut _);

        unsafe {
            // Write DLAB = 1
            this.uart_write(Uart::LCR, 1 << 7);

            // Write the divisor latch
            this.uart_write(Uart::DLL, dla as u8);
            this.uart_write(Uart::DLM, (dla >> 8) as u8);

            // FIFO Enable
            this.uart_write(Uart::FCR, 1 << 0);

            // 8 bit words with parity
            this.uart_write(Uart::LCR, 0b11 | (1 << 3));
        }

        Some(this)
    }

    pub fn send_byte(&mut self, v: u8) {
        while unsafe { self.uart_read(Uart::LSR) } & (1 << 5) == 0 {
            // Wait for the byte to have been sent
        }

        unsafe { self.uart_write(Uart::THR, v) }
    }

    /// # SAFETY
    ///
    /// The register being read must be safe
    pub unsafe fn uart_read(&mut self, register: u8) -> u8 {
        assert!(register < 8);

        // SAFTEY: The memory is in range of the UART regsiters
        unsafe { core::ptr::read_volatile(self.0.add(register as usize)) }
    }

    /// # SAFETY
    ///
    /// The register being written must be safe
    pub unsafe fn uart_write(&mut self, register: u8, value: u8) {
        assert!(register < 8);

        // SAFTEY: The memory is in range of the UART regsiters
        unsafe { core::ptr::write_volatile(self.0.add(register as usize), value) }
    }
}

impl Write for Uart {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        for &b in s.as_bytes() {
            self.send_byte(b);
        }

        Ok(())
    }
}
