#alias b := build
alias r := run

opensbi:
	make -C opensbi PLATFORM=generic

run *EXTRA_ARGS:
	qemu-system-riscv64 {{EXTRA_ARGS}} -M virt -m 2G -nographic \
		-bios opensbi/build/platform/generic/firmware/fw_jump.bin

debug: (run "-gdb tcp::1234 -S")
gdb:
	gdb opensbi/build/platform/generic/firmware/fw_jump.elf \
		-ex 'target remote localhost:1234'
