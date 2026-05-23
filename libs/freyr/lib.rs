#![no_std]

use core::{
    ffi::CStr,
    fmt::{Debug, Display},
};

macro_rules! payload_read_impl {
    ($($fn:ident, $ty:ty);*) => {
        $(
            fn $fn(data: &[u8], start: usize) -> $ty {
                <$ty>::from_le_bytes(data[start..start + core::mem::size_of::<$ty>()].try_into().unwrap())
            }
        )*
    };
}

payload_read_impl!(payload_read_u8, u8);
payload_read_impl!(payload_read_u16, u16);
payload_read_impl!(payload_read_u32, u32);
payload_read_impl!(payload_read_u64, u64);

fn check_elf(data: &[u8]) -> bool {
    let magic = &data[0..4];
    if magic != [0x7f, b'E', b'L', b'F'] {
        wohpe::warn!("Invaild ELF magic");
        return false;
    }

    let class = payload_read_u8(data, 4);
    if class != 2 {
        wohpe::warn!("Only 64 bit ELF files are supported");
        return false;
    }

    let endianness = payload_read_u8(data, 5);
    if endianness != 1 {
        wohpe::warn!("Only little endian files are supported");
        return false;
    }

    let version = payload_read_u8(data, 6);
    if version != 1 {
        wohpe::warn!("Only version 1 is supported, version is {version}");
        return false;
    }

    // Ignore OS-ABI

    let version = payload_read_u32(data, 0x14);
    if version != 1 {
        wohpe::warn!("Only ELF version 1 is supported, got {version}");
    }

    true
}

pub struct RvFlags(u32);

pub enum FloatAbi {
    Soft,
    Single,
    Double,
    Quad,
}

impl RvFlags {
    pub fn rvc(&self) -> bool {
        (self.0 & 0b1) != 0
    }

    pub fn float_abi(&self) -> FloatAbi {
        match (self.0 << 1) & 0b11 {
            0 => FloatAbi::Soft,
            1 => FloatAbi::Single,
            2 => FloatAbi::Double,
            3 => FloatAbi::Quad,
            _ => unreachable!(),
        }
    }

    pub fn rve(&self) -> bool {
        (self.0 & 0x0008) != 0
    }

    pub fn tso(&self) -> bool {
        (self.0 & 0x0010) != 0
    }
}

impl Display for RvFlags {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let mut has = false;

        let mut write_flag = |name| -> core::fmt::Result {
            if has {
                write!(f, " | ")?;
            }

            write!(f, "{name}")?;

            has = true;

            Ok(())
        };

        if self.rvc() {
            write_flag("EF_RISCV_RVC")?;
        }

        let float_abi = match self.float_abi() {
            FloatAbi::Soft => "EF_RISCV_FLOAT_ABI_SOFT",
            FloatAbi::Single => "EF_RISCV_FLOAT_ABI_SINGLE",
            FloatAbi::Double => "EF_RISCV_FLOAT_ABI_DOUBLE",
            FloatAbi::Quad => "EF_RISCV_FLOAT_ABI_QUAD",
        };
        write_flag(float_abi)?;

        if self.rve() {
            write_flag("EF_RISCV_RVE")?;
        }

        if self.tso() {
            write_flag("EF_RISCV_TSO")?;
        }

        Ok(())
    }
}

impl Debug for RvFlags {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_tuple("RvFlags")
            .field(&format_args!("{:x} ({})", self.0, self))
            .finish()
    }
}

#[derive(Debug)]
pub struct ElfHeader {
    pub entrypoint: u64,
    pub flags: RvFlags,
    pub os_abi: u8,

    pub phoff: u64,
    pub phentsize: u16,
    pub phnum: u16,

    pub shoff: u64,
    pub shentsize: u16,
    pub shnum: u16,
    pub shstrndx: u16,
}

pub struct ElfFile<'a> {
    pub header: ElfHeader,
    content: &'a [u8],
}

#[derive(Debug)]
pub enum Error {
    Invalid,
    UnknownSectionKind(u32),
    OutOfBoundsSection(usize),
    InvalidString(usize),
    SectionNotFound,
    UnknownSymbolKind(u8),
    UnknownSymbolBinding(u8),
    UnknownSymbolVisibility(u8),
}

impl Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Error::Invalid => write!(f, "Invalid ELF file"),
            Error::UnknownSectionKind(s) => write!(f, "Section '{s}' is unknown"),
            Error::OutOfBoundsSection(s) => write!(f, "Section index {s} is out of bounds"),
            Error::InvalidString(s) => write!(f, "String at {s} is invalid"),
            Error::SectionNotFound => write!(f, "Section not found"),
            Error::UnknownSymbolKind(s) => write!(f, "Symbol kind '{s}' is unknown"),
            Error::UnknownSymbolBinding(s) => write!(f, "Symbol binding '{s}' is unknown"),
            Error::UnknownSymbolVisibility(s) => write!(f, "Symbol visiblity '{s}' is unknown"),
        }
    }
}

impl core::error::Error for Error {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        None
    }
}

macro_rules! _define_ranged_enum {
    ({ $($meta:tt)* } $name:ident [$($variants:tt)*];) => {
        $($meta)*
        pub enum $name {
            $($variants)*
        }
    };
    ({ $($meta:tt)* } $name:ident [$($variants:tt)*]; $v:ident = $lit:literal $(, $($rest:tt)*)?) => {
        _define_ranged_enum!{{ $($meta)* } $name [$($variants)* $v,]; $($($rest)*)?}
    };
    ({ $($meta:tt)* } $name:ident [$($variants:tt)*]; $v:ident = $start:literal..$end:literal $(, $($rest:tt)*)?) => {
        _define_ranged_enum!{{ $($meta)* } $name [$($variants)* $v(u32),]; $( $($rest)* )?}
    };
}

macro_rules! _define_ranged_parse {
    ($on:ident [$($arms:tt)*];) => {
        match $on {
            $($arms)*
            _ => None,
        }
    };
    ($on:ident [$($arms:tt)*]; $v:ident = $lit:literal $(, $($rest:tt)*)?) => {
        _define_ranged_parse!($on [$($arms)* $lit => {Some(Self::$v)}]; $($($rest)*)?)
    };
    ($on:ident [$($arms:tt)*]; $v:ident = $start:literal..$end:literal $(, $($rest:tt)*)?) => {
        _define_ranged_parse!($on [$($arms)* $start..$end => {Some(Self::$v($on))}]; $( $($rest)* )?)
    };
}

macro_rules! define_ranged {
    ($(#[$($meta:meta)*])* pub enum $name:ident {$($tt:tt)*}) => {
        _define_ranged_enum!{{ $(#[$($meta)*])* } $name []; $($tt)*}

        impl $name {
            fn parse(value: u32) -> Option<Self> {
                _define_ranged_parse!(value []; $($tt)*)
            }
        }
    };
}

define_ranged!(
    #[derive(Debug, Clone, Copy)]
    pub enum SectionKind {
        Null = 0,
        Progbits = 1,
        Symtab = 2,
        Strtab = 3,
        Rela = 4,
        Hash = 5,
        Dynamic = 6,
        Note = 7,
        Nobits = 8,
        Rel = 9,
        Shlib = 10,
        Dynsim = 11,
        InitArray = 14,
        FiniArray = 15,
        PreInitArray = 16,
        Group = 17,
        SymtabExtended = 18,
        Os = 0x60000000..0x70000000,
        Proc = 0x70000000..0x80000000,
        User = 0x80000000..0x90000000,
    }
);

#[derive(Debug, Clone, Copy)]
struct RawSectionHeader {
    name_offset: usize,
    kind: SectionKind,
    offset: usize,
    size: usize,
    entsize: usize,
    address: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct SectionHeader<'a> {
    pub name: &'a str,
    pub kind: SectionKind,
    pub entry_size: usize,
    pub address: usize,
}

define_ranged!(
    #[derive(Debug)]
    pub enum SymbolBinding {
        Local = 0,
        Global = 1,
        Weak = 2,
        Os = 10..13,
        Proc = 13..16,
    }
);

define_ranged!(
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub enum SymbolKind {
        None = 0,
        Object = 1,
        Func = 2,
        Section = 3,
        File = 4,
        Common = 5,
        Tls = 6,
        Os = 10..13,
        Proc = 13..16,
    }
);

define_ranged!(
    #[derive(Debug)]
    pub enum SymbolVisibility {
        Default = 0,
        Internal = 1,
        Hidden = 2,
        Protected = 3,
    }
);

#[derive(Debug)]
pub struct RawSymbol {
    name_offset: usize,
    value: usize,
    size: usize,
    section: usize,
    binding: SymbolBinding,
    kind: SymbolKind,
    visiblity: SymbolVisibility,
}

#[derive(Debug)]
pub struct Symbol<'a> {
    pub name: &'a str,
    pub value: usize,
    pub size: usize,
    pub section: Option<&'a str>,
    pub binding: SymbolBinding,
    pub kind: SymbolKind,
    pub visiblity: SymbolVisibility,
}

impl<'a> ElfFile<'a> {
    pub fn new(file: &'a [u8]) -> Result<ElfFile<'a>, Error> {
        if !check_elf(file) {
            return Err(Error::Invalid);
        }

        let os_abi = payload_read_u8(file, 0x07);
        wohpe::trace!("OS ABI: {os_abi}");

        let entrypoint = payload_read_u64(file, 0x18);
        wohpe::trace!("ELF entrypoint: 0x{entrypoint:x}");

        let flags = RvFlags(payload_read_u32(file, 0x30));
        wohpe::trace!("ELF RV flags: {flags}");

        let phoff = payload_read_u64(file, 0x20);
        wohpe::trace!("Program header offset: {phoff}");

        let phentsize = payload_read_u16(file, 0x36);
        wohpe::trace!("Program header entry size: {phentsize}");

        let phnum = payload_read_u16(file, 0x38);
        wohpe::trace!("Program header entry count: {phnum}");

        let shoff = payload_read_u64(file, 0x28);
        wohpe::trace!("Section header offset: {shoff}");

        let shentsize = payload_read_u16(file, 0x3a);
        wohpe::trace!("Section header entry size: {shentsize}");

        let shnum = payload_read_u16(file, 0x3c);
        wohpe::trace!("Section header entry count: {shnum}");

        let shstrndx = payload_read_u16(file, 0x3e);
        wohpe::trace!("Section header name entry: {shstrndx}");

        Ok(ElfFile {
            header: ElfHeader {
                os_abi,
                entrypoint,
                flags,
                phoff,
                phentsize,
                phnum,
                shoff,
                shentsize,
                shnum,
                shstrndx,
            },
            content: file,
        })
    }

    fn raw_header(&self, index: usize) -> Result<RawSectionHeader, Error> {
        if index >= self.header.shnum as usize {
            return Err(Error::OutOfBoundsSection(index));
        }

        let raw = &self.content
            [self.header.shoff as usize + self.header.shentsize as usize * index..]
            [..self.header.shentsize as usize];

        let name_offset = payload_read_u32(raw, 0x00) as usize;
        wohpe::trace!("Section {index} name offset: {name_offset}");
        let kind = payload_read_u32(raw, 0x04);
        let kind = SectionKind::parse(kind).ok_or(Error::UnknownSectionKind(kind))?;
        wohpe::trace!("Section {index} kind: {kind:?}");
        let offset = payload_read_u64(raw, 0x18) as usize;
        wohpe::trace!("Section {index} offset: {offset}");
        let size = payload_read_u64(raw, 0x20) as usize;
        wohpe::trace!("Section {index} offset: {size}");
        let entsize = payload_read_u64(raw, 0x38) as usize;
        let address = payload_read_u64(raw, 0x10) as usize;

        Ok(RawSectionHeader {
            name_offset,
            kind,
            offset,
            size,
            entsize,
            address,
        })
    }

    fn resolve_string(&self, strings: &'a [u8], offset: usize) -> Result<&'a str, Error> {
        let name = CStr::from_bytes_until_nul(&strings[offset..])
            .map_err(|_| Error::InvalidString(offset))?
            .to_str()
            .map_err(|_| Error::InvalidString(offset))?;

        Ok(name)
    }

    fn section_at(&self, index: usize) -> Result<(SectionHeader<'a>, Option<&'a [u8]>), Error> {
        let raw = self.raw_header(index)?;
        let shstr = self.raw_header(self.header.shstrndx as usize)?;
        let strings = &self.content[shstr.offset..][..shstr.size];
        let name = self.resolve_string(strings, raw.name_offset)?;
        wohpe::debug!("Section {} name: {} ({:?})", index, name, raw.kind);
        Ok((
            SectionHeader {
                name,
                kind: raw.kind,
                entry_size: raw.entsize,
                address: raw.address,
            },
            if matches!(raw.kind, SectionKind::Nobits) {
                None
            } else {
                Some(&self.content[raw.offset..][..raw.size])
            },
        ))
    }

    pub fn section(&self, name: &str) -> Result<(SectionHeader<'a>, Option<&'a [u8]>), Error> {
        for index in 0..self.header.shnum as usize {
            let (section, content) = self.section_at(index)?;

            if section.name == name {
                return Ok((section, content));
            }
        }

        Err(Error::SectionNotFound)
    }

    fn symbol(&self, entsize: usize, symtab: &'a [u8], index: usize) -> Result<RawSymbol, Error> {
        if index >= symtab.len() / entsize {
            panic!("Invalid symbol index {index}");
        }

        let data = &symtab[index * entsize..][..entsize];
        let name_offset = payload_read_u32(data, 0x00) as usize;
        wohpe::trace!("Symbol {index} name offset: 0x{name_offset:x}");
        let info = payload_read_u8(data, 0x04);
        wohpe::trace!("Symbol {index} info: 0x{info:x}");
        let other = payload_read_u8(data, 0x05);
        wohpe::trace!("Symbol {index} other: 0x{other:x}");
        let section = payload_read_u16(data, 0x06) as usize;
        wohpe::trace!("Symbol {index} section: {section}");
        let value = payload_read_u64(data, 0x08) as usize;
        wohpe::trace!("Symbol {index} value: 0x{value:x}");
        let size = payload_read_u64(data, 0x10) as usize;
        wohpe::trace!("Symbol {index} size: 0x{size:x}");

        Ok(RawSymbol {
            name_offset,
            value,
            size,
            section,
            binding: SymbolBinding::parse((info >> 4) as u32)
                .ok_or(Error::UnknownSymbolBinding(info >> 4))?,
            kind: SymbolKind::parse((info & 0xf) as u32)
                .ok_or(Error::UnknownSymbolKind(info & 0xf))?,
            visiblity: SymbolVisibility::parse(other as u32)
                .ok_or(Error::UnknownSymbolVisibility(other))?,
        })
    }

    pub fn symbols(&self) -> Result<impl Iterator<Item = Result<Symbol<'a>, Error>>, Error> {
        let (header, symtab) = self.section(".symtab")?;
        let symtab = symtab.expect("symtab section should not have Nobits");
        let count = symtab.len() / header.entry_size;
        let (_, symstr) = self.section(".strtab")?;
        let symstr = symstr.expect("symstr section should not have Nobits");

        wohpe::debug!("Reading {count} symbols from {header:?}");

        Ok((0..count).map(move |i| {
            let raw = self.symbol(header.entry_size, symtab, i)?;

            Ok(Symbol {
                name: self.resolve_string(symstr, raw.name_offset)?,
                binding: raw.binding,
                kind: raw.kind,
                section: if raw.section > self.header.shnum.into() {
                    None
                } else {
                    Some(self.section_at(raw.section)?.0.name)
                },
                value: raw.value,
                size: raw.size,
                visiblity: raw.visiblity,
            })
        }))
    }
}
