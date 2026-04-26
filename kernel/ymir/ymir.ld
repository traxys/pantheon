OUTPUT_ARCH( "riscv" )

_STACK_LEN = 1M;
_TRAP_STACK_LEN = 512K;
_ORIGIN = 0x80000000;
_SIZE = 33554432;

SECTIONS {
	. = _ORIGIN;

	.text : {
		*(.text.init);
		*(.text) *(.text.*);
	}

	.rodata : ALIGN(8) {
		*(.srodata .srodata.*);
        *(.rodata .rodata.*);
	}

	.data ALIGN(8) : {
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

	.trap_stack (NOLOAD) : {
		_etrapstack = .;
		. += _TRAP_STACK_LEN;
		_strapstack = .;
	}
}
