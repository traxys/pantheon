OUTPUT_ARCH( "riscv" )
ENTRY(_start)

_STACK_LEN = 4K;
_ORIGIN = 0xffffffff80000000;

SECTIONS {
	. = _ORIGIN;

	.text : {
		*(.text) *(.text.*);
	}

	.rodata : {
		*(.srodata .srodata.*);
		*(.rodata .rodata.*);
	}

	.data : {
		*(.sdata .sdata.* .sdata2 .sdata2.*);
		. = ALIGN(8);
        PROVIDE( __global_pointer$ = . + 0x800 );
        *(.data .data.*);
	}

    .bss (NOLOAD): ALIGN(8) {
        *(.sbss .sbss.* .bss .bss.*);
    }
}
