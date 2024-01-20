#alias b := build
alias r := run

kernel_path := "./hades/target/riscv64gc-unknown-none-elf/debug/hades"

kernel:
	cd hades && cargo build

run *EXTRA_ARGS:
	qemu-system-riscv64 {{EXTRA_ARGS}} -M virt -m 2G -nographic \
		-kernel {{kernel_path}}

debug: (run "-gdb tcp::1234 -S")
gdb:
	gdb {{kernel_path}} \
		-ex 'target remote localhost:1234'
