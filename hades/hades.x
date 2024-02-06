_RAM_START = 0x80000000;
_KERNEL_START = _RAM_START + 0x2000000;

_PHYSICAL_STACK = _KERNEL_START + 16M;
_VIRTUAL_STACK = 0xffffffffbfc00000;

_STACK_LEN = 2M;
_KERNEL_CODE_VIRTUAL = 0xffffffffc0000000;

_KERNEL_VIRTUAL_RAM_START = _KERNEL_CODE_VIRTUAL + (_KERNEL_START - _RAM_START);

SECTIONS
{
	. = _KERNEL_START;

	.physical_boot.text : {
		_boot_start = .;
		*(.init)
		_boot_end = ABSOLUTE(.);
	}

	. = _VIRTUAL_STACK;

	.stack (NOLOAD) : AT(_PHYSICAL_STACK)
	{
		_estack = .;
		. += _STACK_LEN;
		_sstack = .;
	}

	_KERNEL_VIRTUAL_CODE_START = _KERNEL_VIRTUAL_RAM_START + (_boot_end - _KERNEL_START);
	HIDDEN(_KERNEL_VA_CODE_OFFSET = _KERNEL_VIRTUAL_CODE_START - _boot_end);
	. = _KERNEL_VIRTUAL_CODE_START;

	.text : AT(ADDR(.text) - _KERNEL_VA_CODE_OFFSET) {
		*(.text.abort);
		*(text .text.*)
	}

	.rodata ALIGN(4) : AT(ADDR(.rodata) - _KERNEL_VA_CODE_OFFSET) {
		*(.srodata .srodata.*);
    	*(.rodata .rodata.*);

		
		/* 4-byte align the end (VMA) of this section.
		   This is required by LLD to ensure the LMA of the following .data
		   section will have the correct alignment. */
		. = ALIGN(4);
	}

	.data ALIGN(8) : AT(ADDR(.data) - _KERNEL_VA_CODE_OFFSET) {
		_sidata = LOADADDR(.data);
    	_sdata = .;
		PROVIDE(__global_pointer$ = . + 0x800);
		*(.sdata .sdata.* .sdata2 .sdata2.*);
    	*(.data .data.*);
		. = ALIGN(8);
		_edata = .;
	}

	.bss ALIGN(8) (NOLOAD) : AT(ADDR(.data) - _KERNEL_VA_CODE_OFFSET) {
		_sbss = .;
    	*(.sbss .sbss.* .bss .bss.*);
    	. = ALIGN(8);
    	_ebss = .;
	}
}
