OUTPUT_ARCH( "riscv" )

_STACK_LEN = 1M;
_ORIGIN = 0x80000000;
_SIZE = 33554432;

SECTIONS {
	. = _ORIGIN;

	.text : {
		*(.text.init);
		*(.text) *(.text.*);
	}

	.rodata ALIGN(8) : {
		*(.srodata .srodata.*);
        *(.rodata .rodata.*);

		. = ALIGN(4);
	}

	.data ALIGN(4096) : {
		*(.sdata .sdata.* .sdata2 .sdata2.*);
        *(.data .data.*);
	}

	.bss ALIGN(8) : {
		*(.sbss .sbss.* .bss .bss.*);
	}

	.stack (NOLOAD) : {
		_estack = .;
		. += _STACK_LEN;
		_sstack = .;
	}
}
