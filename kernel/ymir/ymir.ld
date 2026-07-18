OUTPUT_ARCH( "riscv" )

_ORIGIN = 0x80000000;
_SIZE = 33554432;

SECTIONS {
	. = _ORIGIN;

	.text : {
		*(.text.init);
		*(.text) *(.text.*);
	}

	.rodata : ALIGN(4096) {
		*(.srodata .srodata.*);
        *(.rodata .rodata.*);
	}

	.data ALIGN(4096) : {
		*(.sdata .sdata.* .sdata2 .sdata2.*);
        *(.data .data.*);
	}

	.bss ALIGN(4096) : {
		*(.sbss .sbss.* .bss .bss.*);
	}
}
