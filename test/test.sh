#!/usr/bin/env sh
set -e
input="$1"
output="$(basename -s .ly $input)"
./bin/lymacro "$1" | lilypond -I ~/lib/ly -o "$output" -
