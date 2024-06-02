# pantheon

Pantheon is a full blown OS (not only the kernel), meant to be quite POSIX compliant.
It is mainly meant as a personal project to allow me to learn different concepts by implementing them with few dependencies.

It only targets RISC-V 64 on the QEMU virt machine.

## kernel: hades

The kernel is named `hades`. It is going to be a monolithic kernel for ease of development.

## Utility libraries

- `apis`: A simple bump allocator
- `ogma`: A library to handle Device Tree
