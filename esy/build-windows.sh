#! /bin/sh

set -e

# build-windows.sh
# Execute build steps for Windows as described here:
# https://github.com/ocaml/ocaml/blob/4.02/README.win32

make -f Makefile.nt world
make -f Makefile.nt bootstrap
make -f Makefile.nt opt
make -f Makefile.nt opt.opt
make -f Makefile.nt install
