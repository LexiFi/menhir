#!/bin/sh

# This script rebuilds Menhir and MenhirLib from scratch, so as to make sure
# that we are testing the current development version.

. ./config.sh

# Recompile.
rm -f $SRC/installation.ml
rm -rf $BUILD
echo "Building Menhir..."
if ! make -C $SRC >/dev/null ; then
  echo "Could not build Menhir. Stop."
  exit 1
fi
echo "Building MenhirLib..."
if ! make -C $SRC library >/dev/null ; then
  echo "Could not build MenhirLib. Stop."
  exit 1
fi

# Re-install MenhirLib.
ocamlfind remove menhirLib
ocamlfind install menhirLib $SRC/META $BUILD/menhirLib.cmi $BUILD/menhirLib.cmo $BUILD/menhirLib.cmx $BUILD/menhirLib.o

