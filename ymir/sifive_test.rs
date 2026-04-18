use ogma::DeviceTreeNode;

#[repr(u16)]
enum TestStatus {
    Fail = 0x3333,
    Pass = 0x5555,
    Reset = 0x7777,
}

#[repr(C)]
struct TestCommand {
    status: TestStatus,
    code: u16,
}

pub struct SifiveTest(*mut TestCommand);

#[allow(unused)]
impl SifiveTest {
    /// # SAFETY
    ///
    /// The device tree node must point to a valid Sifive test device
    pub unsafe fn new(node: &DeviceTreeNode) -> Option<Self> {
        if !node
            .compatible()?
            .iter()
            .any(|c| c.starts_with("sifive,test"))
        {
            // Invalid SifiveTest node
            return None;
        }

        let reg = node.reg()?[0];
        Some(SifiveTest(reg.address as *mut _))
    }

    pub fn shutdown(&mut self, code: u16) -> ! {
        unsafe {
            self.0.write_volatile(TestCommand {
                status: TestStatus::Pass,
                code,
            });
        }

        unreachable!()
    }

    pub fn panic(&mut self, code: u16) -> ! {
        unsafe {
            self.0.write_volatile(TestCommand {
                status: TestStatus::Fail,
                code,
            });
        }

        unreachable!()
    }

    pub fn reset(&mut self) -> ! {
        unsafe {
            self.0.write_volatile(TestCommand {
                status: TestStatus::Reset,
                code: 0,
            });
        }

        unreachable!()
    }
}
