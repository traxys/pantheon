//! Ogma is a library to interract with Device Trees
//!
//! The name comes from a Irish/Scottish figure who invented the Ogham alphabet
#![no_std]

mod parsing;

use apis::{
    boxed::Box,
    collections::{ApisError, Vec},
    Allocator,
};

/// Error when parsing a device tree
#[derive(Debug)]
pub enum DtError {
    /// No end marker for the device tree
    MissingEnd,
    /// Invalid magic number encountered
    InvalidMagic,
    /// Device tree version unsupported. Only version 17 is supported
    UnsupportedVersion(u32),
    /// Could not allocate memory for a device tree struct
    AllocationFailure(ApisError),
    /// Invalid struct section of the device tree binary
    MissingStructs {
        /// Size of the struct section declared in the header
        expected: usize,
        /// Size of the struct section parsed
        got: usize,
    },
    /// Error while parsing a node. See [DtNodeError].
    NodeError {
        /// Location in bytes from the start of the device tree of the error
        at: usize,
        /// Node error
        kind: DtNodeError,
    },
    /// Passed device tree is too short compared to the size declared in the header
    NotEnoughBytes {
        /// Size declared in the header
        expected: usize,
        /// Size availaible
        got: usize,
    },
}

#[derive(Debug)]
pub enum DtNodeError {
    /// Missing start marker for the node
    MissingStart,
    /// Missing end marker for the node
    MissingEnd,
    /// String is not null terminated
    UnterminatedString,
    /// Property name is not an UTF-8 string
    InvalidPropertyName,
    /// Malformed root property
    RootHasName,
    /// Property has an invalid length
    UnexpectedPropLen {
        /// Availaible length
        got: usize,
        /// Expected length
        expected: usize,
    },
    /// Ogma does not support this cell size
    UnhandledCellSize(u32),
}

impl From<ApisError> for DtError {
    fn from(e: ApisError) -> Self {
        Self::AllocationFailure(e)
    }
}

/// A DeviceTree reg property (address, size)
#[derive(Clone, Copy)]
pub struct DtReg {
    pub address: u64,
    pub size: u64,
}

impl core::fmt::Debug for DtReg {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("DtReg")
            .field("address", &format_args!("0x{:x}", self.address))
            .field("size", &format_args!("0x{:x}", self.size))
            .finish()
    }
}

/// A raw (unknown) device tree property
#[derive(Debug)]
pub struct RawDtProp<'d> {
    pub data: &'d [u8],
    pub name: &'d str,
}

impl<'d> core::fmt::Display for RawDtProp<'d> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{} = ", self.name)?;
        if self.data != [0]
            && self.data.ends_with(&[0])
            && self.data[0..self.data.len() - 1].is_ascii()
        {
            write!(
                f,
                "\"{}\"",
                core::str::from_utf8(&self.data[0..self.data.len() - 1]).unwrap()
            )
        } else {
            write!(f, "[")?;
            if !self.data.is_empty() {
                write!(f, "{:x}", self.data[0])?;
                for b in &self.data[1..] {
                    write!(f, " {b:x}")?;
                }
            }
            write!(f, "]")
        }
    }
}

/// A device tree property, some are parsed by Ogma. Unknown properties are parsed as [RawDtProp]
#[derive(Debug)]
pub enum DtProp<'a, 'd> {
    /// An unknown property
    Raw(RawDtProp<'d>),
    /// A model property
    Model(&'d str),
    /// A compatible property
    Compatible(Vec<'a, &'d str>),
    /// A register property
    Reg(Vec<'a, DtReg>),
    /// The address cells property
    AddressCells(u32),
    /// The size cells property
    SizeCells(u32),
}

impl<'a, 'd> core::fmt::Display for DtProp<'a, 'd> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            DtProp::Raw(raw) => write!(f, "{raw}"),
            DtProp::Model(model) => write!(f, "model = \"{model}\""),
            DtProp::Compatible(comp) => {
                write!(f, "compatible = \"{}\"", comp[0])?;
                for comp in comp.iter().skip(1) {
                    write!(f, ", \"{comp}\"")?;
                }
                Ok(())
            }
            DtProp::Reg(regs) => {
                write!(f, "reg = <")?;
                let mut has = false;
                for reg in regs.iter() {
                    if has {
                        write!(f, " ")?;
                    }

                    write!(f, "0x{:x} 0x{:x}", reg.address, reg.size)?;

                    has = true;
                }
                write!(f, ">")
            }
            DtProp::AddressCells(addr) => write!(f, "#address-cells = <{addr}>"),
            DtProp::SizeCells(size) => write!(f, "#size-cells = <{size}>"),
        }
    }
}

/// A device tree node
#[derive(Debug)]
pub struct DeviceTreeNode<'a, 'd> {
    pub name: &'d str,
    pub props: Vec<'a, DtProp<'a, 'd>>,
    pub children: Vec<'a, DeviceTreeNode<'a, 'd>>,
}

impl<'a, 'd> core::fmt::Display for DeviceTreeNode<'a, 'd> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        self.render_to(f, 0)
    }
}

/// A device tree, mostly contains children [DeviceTreeNode]
#[derive(Debug)]
pub struct DeviceTree<'a, 'd> {
    pub reserved: Vec<'a, DtReg>,
    pub root: DeviceTreeNode<'a, 'd>,
}

struct Indent(usize);

impl core::fmt::Display for Indent {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{:>1$}", "", self.0 * 4)
    }
}

impl<'a, 'd> DeviceTreeNode<'a, 'd> {
    fn render_to(&self, f: &mut core::fmt::Formatter<'_>, depth: usize) -> core::fmt::Result {
        writeln!(f, "{}{} {{", Indent(depth), self.name)?;
        for prop in self.props.iter() {
            writeln!(f, "{}{};", Indent(depth + 1), prop)?;
        }
        if !self.props.is_empty() && !self.children.is_empty() {
            writeln!(f, "{}", Indent(depth + 1))?;
        }
        for child in self.children.iter() {
            child.render_to(f, depth + 1)?;
        }
        writeln!(f, "{}}};", Indent(depth))
    }

    /// Find a direct child with the given name (excluding the address)
    pub fn child(&self, name: &str) -> Option<&Self> {
        self.children.iter().find(|n| match n.name.split_once('@') {
            None => n.name == name,
            Some((prefix, _)) => prefix == name,
        })
    }

    pub fn child_mut(&mut self, name: &str) -> Option<&mut Self> {
        self.children
            .iter_mut()
            .find(|n| match n.name.split_once('@') {
                None => n.name == name,
                Some((prefix, _)) => prefix == name,
            })
    }

    /// Get the register property of the node (if any)
    pub fn reg(&self) -> Option<&[DtReg]> {
        self.props.iter().find_map(|p| match p {
            DtProp::Reg(r) => Some(&**r),
            _ => None,
        })
    }
}

macro_rules! fdt_header {
    (struct $name:ident {
       $($field:ident: u32),* $(,)?
    }) => {
        #[derive(Debug)]
        #[allow(unused)]
        struct $name {
            $($field: u32,)*
        }

        impl $name {
            fn load(dtb: &[u8]) -> Self {
                let rest = dtb;
                $(
                    let ($field, rest) = rest.split_at(core::mem::size_of::<u32>());
                    let $field = u32::from_be_bytes($field.try_into().unwrap());
                )*

                let _ = rest;

                FdtHeader {
                    $($field,)*
                }
            }
        }
    };
}

fdt_header! {
    struct FdtHeader {
        magic: u32,
        totalsize: u32,
        off_dt_struct: u32,
        off_dt_strings: u32,
        off_mem_rsvmap: u32,
        version: u32,
        last_comp_version: u32,
        boot_cpuid_phys: u32,
        size_dt_strings: u32,
        size_dt_struct: u32,
    }
}

/// Load a device tree binary into the given [Allocator].
///
/// The returned bytes can be passed to the [DeviceTree::load] function to parse it.
///
/// # SAFETY
///
/// The `start` pointer must point to a device tree binary with a coherent header
/// The rest of the device tree can be malformed.
pub unsafe fn load_dtb<'a>(
    start: *const u8,
    a: &'a Allocator<'a>,
) -> Result<Box<'a, [u8]>, DtError> {
    unsafe fn read_u32_raw(ptr: *const u8) -> u32 {
        u32::from_be_bytes(*(ptr as *const [u8; 4]))
    }

    let magic = read_u32_raw(start);

    if magic != 0xd00dfeed {
        return Err(DtError::InvalidMagic);
    }

    let total_size = read_u32_raw(start.add(4));
    // Use an u32 to ensure that we have sufficient alignement
    let dtb = a.alloc(
        core::alloc::Layout::from_size_align(total_size as usize, 4).expect("Invalid layout"),
    );

    if dtb.is_null() {
        return Err(ApisError::OutOfMemory.into());
    }

    let mut dtb = Box::from_raw(core::slice::from_raw_parts_mut(dtb, total_size as usize));

    core::ptr::copy_nonoverlapping(start, dtb.as_mut_ptr(), dtb.len());

    Ok(dtb)
}

impl<'a, 'd> DeviceTree<'a, 'd> {
    /// Load a device tree from a device tree binary
    pub fn load(data: &'d [u8], a: &'a Allocator<'a>) -> Result<Self, DtError> {
        let mut offset = 0;
        let magic = parsing::read_u32(&mut offset, data);

        if magic != 0xd00dfeed {
            return Err(DtError::InvalidMagic);
        }

        let total_size = parsing::read_u32(&mut offset, data) as usize;
        if total_size > data.len() {
            return Err(DtError::NotEnoughBytes {
                expected: total_size,
                got: data.len(),
            });
        }

        let header = FdtHeader::load(data);
        if header.version < 17 || header.last_comp_version != 16 {
            return Err(DtError::UnsupportedVersion(header.last_comp_version));
        }

        let mut reserved = Vec::new(a);

        let mut rsv = &data[header.off_mem_rsvmap as usize..];
        loop {
            let (addr, next) = rsv.split_at(core::mem::size_of::<u64>());
            let (size, next) = next.split_at(core::mem::size_of::<u64>());
            rsv = next;

            let addr = u64::from_be_bytes(addr.try_into().unwrap());
            let size = u64::from_be_bytes(size.try_into().unwrap());

            if addr == 0 && size == 0 {
                break;
            }

            reserved.push(DtReg {
                address: addr,
                size,
            })?;
        }

        let structs = &data[header.off_dt_struct as usize
            ..(header.off_dt_struct + header.size_dt_struct) as usize];
        let strings = &data[header.off_dt_strings as usize
            ..(header.off_dt_strings + header.size_dt_strings) as usize];
        let mut offset = 0;
        let root = parsing::parse_node(&mut offset, structs, strings, a, true, None, None)?;

        let end = parsing::skip_nop(&mut offset, structs);

        if end != parsing::FDT_END {
            return Err(DtError::MissingEnd);
        }

        if offset != header.size_dt_struct as usize {
            return Err(DtError::MissingStructs {
                expected: header.size_dt_struct as usize,
                got: offset,
            });
        }

        Ok(Self { reserved, root })
    }
}

#[cfg(test)]
mod tests {
    use core::mem::MaybeUninit;

    use apis::Allocator;

    use crate::DeviceTree;

    #[test]
    fn virt() {
        // This is approximately the memory consumed by the parsed virt device tree
        let mut backing = [MaybeUninit::uninit(); 15 * 1024];
        let alloc = Allocator::new(&mut backing);

        let dtb = include_bytes!("../virt.dtb");
        DeviceTree::load(dtb, &alloc).unwrap();
    }
}
