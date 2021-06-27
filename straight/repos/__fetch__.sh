#!/usr/bin/env bash

set -euo pipefail

if [ $# -eq 1 ]; then
	start_name=$1
elif [ $# -gt 1 ]; then
	echo "bad usage" >&2
	exit 1
else
	start_name=
fi

cd "$(dirname "$0")"

repos=(*)

for dir in "${repos[@]}"; do
	if [[ ! -d $dir || $dir = "." || $dir = ".." ]]; then
		continue
	fi
	if [[ $start_name ]]; then
		if [[ $start_name = "$dir" ]]; then
			start_name=
		fi
		continue
	fi
	pushd "$dir"
	echo "$dir"
	git fetch
	if [ "$(git rev-parse HEAD)" != "$(git rev-parse "@{u}")" ]; then
		bash -i
	fi
	popd
done

echo "ALL DONE"\!
