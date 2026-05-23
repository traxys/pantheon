use freyr::{ElfFile, SymbolKind};

fn main() {
    wohpe_env::init(wohpe_env::wohpe::LogLevel::Warn).unwrap();

    let mut args = std::env::args().skip(1);
    let binary = args.next().expect("Missing binary");
    let symbol = args.next().expect("Missing symbol");

    let binary = std::fs::read(binary).expect("Failed to read binary");
    let elf = ElfFile::new(&binary).unwrap();

    let symbol = elf
        .symbols()
        .unwrap()
        .find(|s| s.as_ref().unwrap().name == symbol)
        .expect("Symbol not found")
        .unwrap();

    if symbol.kind != SymbolKind::Object {
        panic!("Symbol {symbol:?} is not an object");
    }

    let (header, section) = elf
        .section(symbol.section.expect("symbol is not in a present section"))
        .unwrap();
    let section = section.expect("requested section is nobits");

    let offset = symbol.value - header.address;

    let value = &section[offset..][..symbol.size];

    let value = match symbol.size {
        1 => u8::from_le_bytes(value.try_into().unwrap()).to_string(),
        2 => u16::from_le_bytes(value.try_into().unwrap()).to_string(),
        4 => u32::from_le_bytes(value.try_into().unwrap()).to_string(),
        8 => u64::from_le_bytes(value.try_into().unwrap()).to_string(),
        _ => panic!("Unhandled size: {}", symbol.size),
    };

    println!("{value}");
}
