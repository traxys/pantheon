gdb_py := justfile_directory() + / 'pantheon-gdb'

gdb_args := "--directory " + gdb_py + " " + \
			"-iex 'add-auto-load-safe-path " + gdb_py + "' " + \
			"-iex 'set print pretty on'"

export PYTHONPATH := env("PYTHONPATH", "") + ":" + gdb_py

ymir-run-dbg:
	vvk run --binary ::ymir::ymir -- -s -S

ymir-gdb:
	cgdb {{gdb_args}} -ex 'target remote :1234' $(vvk run --binary ::ymir::ymir --path) 
