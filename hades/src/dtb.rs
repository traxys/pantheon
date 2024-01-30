use crate::early_alloc::{
    collections::{EarlyAllocError, Vec},
    EarlyAllocator,
};

#[derive(Debug)]
pub enum DtError {
    MissingEnd,
    InvalidMagic,
    UnsupportedVersion(u32),
    AllocationFailure(EarlyAllocError),
    MissingStructs { expected: usize, got: usize },
    NodeError { at: usize, kind: DtNodeError },
}

#[derive(Debug)]
pub enum DtNodeError {
    MissingStart,
    MissingEnd,
    UnterminatedString,
    InvalidPropertyName,
    RootHasName,
    UnexpectedPropLen { got: usize, expected: usize },
    UnhandledCellSize(u32),
}

impl From<EarlyAllocError> for DtError {
    fn from(e: EarlyAllocError) -> Self {
        Self::AllocationFailure(e)
    }
}

#[derive(Debug)]
pub struct DtReg {
    pub address: u64,
    pub size: u64,
}

#[derive(Debug)]
pub struct RawDtProp<'a> {
    pub data: &'a [u8],
    pub name: &'a str,
}

#[derive(Debug)]
pub enum DtProp<'a> {
    Raw(RawDtProp<'a>),
    Model(&'a str),
    Compatible(Vec<'a, &'a str>),
    Reg(Vec<'a, DtReg>),
    AddressCells(u32),
    SizeCells(u32),
}

#[derive(Debug)]
pub struct DeviceTreeNode<'a> {
    pub name: &'a str,
    pub props: Vec<'a, DtProp<'a>>,
    pub children: Vec<'a, DeviceTreeNode<'a>>,
}

#[derive(Debug)]
pub struct DeviceTree<'a> {
    pub reserved: Vec<'a, DtReg>,
    pub root: DeviceTreeNode<'a>,
}

unsafe fn read_u32_raw(ptr: *const u8) -> u32 {
    u32::from_be_bytes(*(ptr as *const [u8; 4]))
}

fn align(value: usize, to: usize) -> usize {
    if value % to == 0 {
        value
    } else {
        value + (to - value % to)
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

const FDT_BEGIN_NODE: u32 = 0x00000001;
const FDT_END_NODE: u32 = 0x00000002;
const FDT_PROP: u32 = 0x00000003;
const FDT_NOP: u32 = 0x00000004;
const FDT_END: u32 = 0x00000009;

fn read_u32(offset: &mut usize, structs: &[u8]) -> u32 {
    let value = u32::from_be_bytes(
        structs[*offset..*offset + core::mem::size_of::<u32>()]
            .try_into()
            .unwrap(),
    );
    *offset += core::mem::size_of::<u32>();
    value
}

fn skip_nop(offset: &mut usize, structs: &[u8]) -> u32 {
    loop {
        let token = read_u32(offset, structs);

        if token != FDT_NOP {
            break token;
        }
    }
}

fn string(s: &[u8], offset: usize) -> Result<&str, DtError> {
    core::str::from_utf8(s).map_err(|_| DtError::NodeError {
        at: offset,
        kind: DtNodeError::InvalidPropertyName,
    })
}

fn string_list<'a, 'b>(
    l: &'b [u8],
    a: &'a EarlyAllocator<'a>,
    offset: usize,
) -> Result<Vec<'a, &'b str>, DtError> {
    let mut v = Vec::new(a);
    for s in l.split(|&e| e == 0).filter(|s| !s.is_empty()) {
        v.push(string(s, offset)?)?
    }
    Ok(v)
}

fn early_parse_node<'a>(
    offset: &mut usize,
    structs: &'a [u8],
    strings: &'a [u8],
    a: &'a EarlyAllocator<'a>,
    is_root: bool,
    address_cells: Option<u32>,
    size_cells: Option<u32>,
) -> Result<DeviceTreeNode<'a>, DtError> {
    fn null_terminated_str(s: &[u8], offset: usize) -> Result<&str, DtError> {
        let Some((end, _)) = s[offset..].iter().enumerate().find(|&(_, &c)| c == 0) else {
            return Err(DtError::NodeError {
                at: offset,
                kind: DtNodeError::UnterminatedString,
            });
        };

        string(&s[offset..offset + end], offset)
    }

    let first_token = skip_nop(offset, structs);

    if first_token != FDT_BEGIN_NODE {
        return Err(DtError::NodeError {
            at: *offset,
            kind: DtNodeError::MissingStart,
        });
    }

    let name = null_terminated_str(structs, *offset)?;

    if is_root && !name.is_empty() {
        return Err(DtError::NodeError {
            at: *offset,
            kind: DtNodeError::RootHasName,
        });
    }

    let name = if is_root { "/" } else { name };

    *offset += name.len() + 1;
    *offset = align(*offset, 4);

    let mut props = Vec::new(a);

    let mut child_address_cells = address_cells;
    let mut child_size_cells = size_cells;

    let mut next_token = loop {
        let token = skip_nop(offset, structs);

        if token != FDT_PROP {
            break token;
        }

        let length = read_u32(offset, structs) as usize;
        let name_offset = read_u32(offset, structs) as usize;

        let prop = RawDtProp {
            data: &structs[*offset..*offset + length],
            name: null_terminated_str(strings, name_offset)?,
        };

        let u32_prop = || {
            Ok::<_, DtError>(u32::from_be_bytes(prop.data.try_into().map_err(|_| {
                DtError::NodeError {
                    at: *offset,
                    kind: DtNodeError::UnexpectedPropLen {
                        expected: 4,
                        got: prop.data.len(),
                    },
                }
            })?))
        };

        enum CellKind {
            Addr,
            Size,
        }

        props.push(match prop.name {
            "model" => DtProp::Model(string(&prop.data[..prop.data.len() - 1], *offset)?),
            "compatible" => DtProp::Compatible(string_list(prop.data, a, *offset)?),
            "#address-cells" => {
                let cells = u32_prop()?;
                child_address_cells = Some(cells);
                DtProp::AddressCells(cells)
            }
            "#size-cells" => {
                let cells = u32_prop()?;
                child_size_cells = Some(cells);
                DtProp::SizeCells(cells)
            }
            "reg" => {
                let read_cell = |kind: CellKind, off: &mut usize| {
                    let cell_size = match kind {
                        CellKind::Addr => child_address_cells.unwrap_or(2),
                        CellKind::Size => child_size_cells.unwrap_or(1),
                    };

                    match cell_size {
                        0 => Ok(0),
                        1 => Ok(read_u32(off, prop.data) as u64),
                        2 => {
                            let high = read_u32(off, prop.data) as u64;
                            let low = read_u32(off, prop.data) as u64;

                            Ok(high << 32 | low)
                        }
                        _ => Err(DtError::NodeError {
                            at: *offset,
                            kind: DtNodeError::UnhandledCellSize(cell_size),
                        }),
                    }
                };

                let mut prop_offset = 0;
                let mut reg = Vec::new(a);
                while prop_offset < prop.data.len() {
                    let address = read_cell(CellKind::Addr, &mut prop_offset)?;
                    let size = read_cell(CellKind::Size, &mut prop_offset)?;

                    reg.push(DtReg { address, size })?;
                }

                DtProp::Reg(reg)
            }
            _ => DtProp::Raw(prop),
        })?;

        *offset += length;
        *offset = align(*offset, 4);
    };

    let mut children = Vec::new(a);

    let after_children = loop {
        next_token = if next_token == FDT_NOP {
            skip_nop(offset, structs)
        } else {
            next_token
        };

        if next_token != FDT_BEGIN_NODE {
            break next_token;
        }

        // Make the node start again the parse
        *offset -= 4;

        let child = early_parse_node(
            offset,
            structs,
            strings,
            a,
            false,
            child_address_cells,
            child_size_cells,
        )?;

        children.push(child)?;

        next_token = read_u32(offset, structs);
    };

    let end = if after_children == FDT_NOP {
        skip_nop(offset, structs)
    } else {
        after_children
    };

    if end != FDT_END_NODE {
        return Err(DtError::NodeError {
            at: *offset,
            kind: DtNodeError::MissingEnd,
        });
    }

    Ok(DeviceTreeNode {
        name,
        props,
        children,
    })
}

impl<'a> DeviceTree<'a> {
    pub unsafe fn early_load(start: *const u8, a: &'a EarlyAllocator) -> Result<Self, DtError> {
        let magic = read_u32_raw(start);

        if magic != 0xd00dfeed {
            return Err(DtError::InvalidMagic);
        }

        let total_size = read_u32_raw(start.add(4));
        let dtb = core::slice::from_raw_parts(start, total_size as usize);
        let header = FdtHeader::load(dtb);

        if header.version < 17 || header.last_comp_version != 16 {
            return Err(DtError::UnsupportedVersion(header.last_comp_version));
        }

        let mut reserved = Vec::new(a);

        let mut rsv = &dtb[header.off_mem_rsvmap as usize..];
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

        let structs = &dtb[header.off_dt_struct as usize
            ..(header.off_dt_struct + header.size_dt_struct) as usize];
        let strings = &dtb[header.off_dt_strings as usize
            ..(header.off_dt_strings + header.size_dt_strings) as usize];
        let mut offset = 0;
        let root = early_parse_node(&mut offset, structs, strings, a, true, None, None)?;

        let end = skip_nop(&mut offset, structs);

        if end != FDT_END {
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
