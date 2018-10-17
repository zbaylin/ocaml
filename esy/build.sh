#! /bin/sh

set -e

make -j1 world.opt
make install
make clean
