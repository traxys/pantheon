MEMORY
{
	RAM : ORIGIN = 0x80200000, LENGTH = 16M
	EARLY_HEAP : ORIGIN = 0x82000000, LENGTH = 1M
}

REGION_ALIAS("REGION_TEXT", RAM);
REGION_ALIAS("REGION_RODATA", RAM);
REGION_ALIAS("REGION_DATA", RAM);
REGION_ALIAS("REGION_BSS", RAM);
REGION_ALIAS("REGION_HEAP", RAM);
REGION_ALIAS("REGION_STACK", RAM);

SECTIONS
{
        ._early_heap (NOLOAD) :
        {
                . = ALIGN(8);
                PROVIDE ( early_heap_start = . );
                . = . + 1M;
                . = ALIGN(8);
        } >EARLY_HEAP
}
