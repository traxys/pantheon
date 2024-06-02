use apis::{collections::Vec, Allocator};

use crate::{DeviceTreeNode, DtError, DtNodeError, DtProp, DtReg, RawDtProp};

const FDT_BEGIN_NODE: u32 = 0x00000001;
const FDT_END_NODE: u32 = 0x00000002;
const FDT_PROP: u32 = 0x00000003;
const FDT_NOP: u32 = 0x00000004;
pub(crate) const FDT_END: u32 = 0x00000009;

fn align(value: usize, to: usize) -> usize {
    if value % to == 0 {
        value
    } else {
        value + (to - value % to)
    }
}

pub(crate) fn read_u32(offset: &mut usize, structs: &[u8]) -> u32 {
    let value = u32::from_be_bytes(
        structs[*offset..*offset + core::mem::size_of::<u32>()]
            .try_into()
            .unwrap(),
    );
    *offset += core::mem::size_of::<u32>();
    value
}

pub(crate) fn skip_nop(offset: &mut usize, structs: &[u8]) -> u32 {
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
    a: &'a Allocator<'a>,
    offset: usize,
) -> Result<Vec<'a, &'b str>, DtError> {
    let mut v = Vec::new(a);
    for s in l.split(|&e| e == 0).filter(|s| !s.is_empty()) {
        v.push(string(s, offset)?)?
    }
    Ok(v)
}

fn null_terminated_str(s: &[u8], offset: usize) -> Result<&str, DtError> {
    let Some((end, _)) = s[offset..].iter().enumerate().find(|&(_, &c)| c == 0) else {
        return Err(DtError::NodeError {
            at: offset,
            kind: DtNodeError::UnterminatedString,
        });
    };

    string(&s[offset..offset + end], offset)
}

fn parse_node_props<'a, 'd>(
    offset: &mut usize,
    structs: &'d [u8],
    strings: &'d [u8],
    a: &'a Allocator<'a>,
    child_address_cells: &mut Option<u32>,
    child_size_cells: &mut Option<u32>,
) -> Result<(Vec<'a, DtProp<'a, 'd>>, u32), DtError> {
    let mut props = Vec::new(a);

    loop {
        let token = skip_nop(offset, structs);

        if token != FDT_PROP {
            break Ok((props, token));
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
                *child_address_cells = Some(cells);
                DtProp::AddressCells(cells)
            }
            "#size-cells" => {
                let cells = u32_prop()?;
                *child_size_cells = Some(cells);
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
    }
}

pub(crate) fn parse_node<'a, 'd>(
    offset: &mut usize,
    structs: &'d [u8],
    strings: &'d [u8],
    a: &'a Allocator<'a>,
    is_root: bool,
    address_cells: Option<u32>,
    size_cells: Option<u32>,
) -> Result<DeviceTreeNode<'a, 'd>, DtError> {
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

    let mut child_address_cells = address_cells;
    let mut child_size_cells = size_cells;

    let (props, mut next_token) = parse_node_props(
        offset,
        structs,
        strings,
        a,
        &mut child_address_cells,
        &mut child_size_cells,
    )?;

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

        let child = parse_node(
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
