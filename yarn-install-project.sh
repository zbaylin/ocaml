#!/usr/bin/env bash
set -e
set -x
# Check if space exists in path
if [[ "$(pwd)" =~ ( |\') ]]
then
    echo "ERROR: refuse to work with a path containing space: \"$(pwd)\""
    exit 1
fi
./configure -no-cfi -prefix $(pwd)
make -j world.opt
make install
