#![no_std]

use core::fmt::{Debug, Display};

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
}
