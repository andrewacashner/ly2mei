#!/usr/bin/env sh

clang++ -Wall -Werror -Wextra -Wpedantic -lmei -std=c++20 \
    -o bin/"$1" "$1".cpp
#    -O1 -g -fsanitize=address -fno-omit-frame-pointer
