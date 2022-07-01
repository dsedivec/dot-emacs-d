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
		retry=1
		while [ "$retry" -eq 1 ]; do
			if ! bash -i; then
				echo "Shell exited non-zero.  I'm worried for you."
				answer=
				while [ -z "$answer" ]; do
					read -rp "Abort, Retry, Ignore? " answer
					case "$answer" in
						[Aa])
							exit 1
							;;

						[Rr])
							retry=1
							;;

						[Ii])
							retry=0
							;;

						*)
							echo "Invalid response, expected one of: A R I"
							answer=
							;;
					esac
				done
			else
				retry=0
			fi
		done
	fi
	popd
done

echo "ALL DONE"\!
