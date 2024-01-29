#alias b := build
alias r := run

kernel_path := "./hades/target/riscv64gc-unknown-none-elf/debug/hades"

kernel:
	cd hades && cargo build

qemu_args := "-M virt -m 2G -nographic"

raw_run *EXTRA_ARGS:
	qemu-system-riscv64 {{EXTRA_ARGS}} {{qemu_args}}

run *EXTRA_ARGS: (raw_run EXTRA_ARGS "-kernel" kernel_path)
_krun kernel:
	cd hades && qemu-system-riscv64 {{qemu_args}} -kernel {{kernel}}

debug: (run "-gdb tcp::1234 -S")
gdb:
	gdb {{kernel_path}} \
		-ex 'target remote localhost:1234'
