#!/usr/bin/env bash
#
# Usage: ./wohpe-qemu.sh <read_static> <payload> [qemu args...]

read_static=$1
payload=$2
shift 2
args=("$@")

run() {
	qemu-system-riscv64 -bios "$payload" "${args[@]}" "$@"
}

if ! size=$(env -u PT_LOG "$read_static" "$payload" WOHPE_DIRECTIVE_SIZE) ||
	! addr=$(env -u PT_LOG "$read_static" "$payload" WOHPE_DIRECTIVE_INITIAL); then
	run
	exit $?
fi

if [[ $(wc -c <<<"$PT_LOG") -gt $size ]]; then
	echo "Log directive is too large" >&2
	exit 1
fi

tmp_dir=$(mktemp -d)
trap 'rm -rf -- $tmp_dir' EXIT

echo -ne "$PT_LOG\0" >"$tmp_dir/wohpe"

run -device "loader,file=$tmp_dir/wohpe,addr=$addr,force-raw=on"
exit $?
