#!/usr/bin/env sh
set -e
input="$1"
output="build/score.mei"
./bin/ly2mei "$input" > "$output"
verovio "$output"
inkview "${output%.mei}.svg" &
