#!/usr/bin/env bash

first=()

while [[ "$1" != -- ]]; do
	first+=("$1")
	shift
done

shift

"${first[@]}"&
"$@"
