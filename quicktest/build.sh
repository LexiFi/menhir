#!/bin/bash

# This script rebuilds Menhir and MenhirLib from scratch, so as to make sure
# that we are testing the current development version.

. ./config.sh

# Removing $BUILD ensures that Menhir is rebuilt from scratch, but this is
# a bit slow.
# rm -rf $BUILD

# Recompile.
rm -f $SRC/installation.ml
echo "Building Menhir and MenhirLib..."
if ! make -C $SRC >/dev/null ; then
  echo "Could not build Menhir. Stop."
  exit 1
fi

# Re-install MenhirLib.
echo "Removing old MenhirLib..."
ocamlfind remove menhirLib
echo "Installing new MenhirLib..."
cp -f $SRC/menhirLib.META $SRC/META
trap "{ rm -f $SRC/META ; }" EXIT
if ! ocamlfind install menhirLib \
  $SRC/META $BUILD/menhirLib.cmi $BUILD/menhirLib.cmo \
  $BUILD/menhirLib.cmx $BUILD/menhirLib.o ; then
  echo "Could not install MenhirLib. Stop."
  exit 1
fi
