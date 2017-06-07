#!/bin/bash

# This script rebuilds Menhir and MenhirLib from scratch, so as to make sure
# that we are testing the current development version.

# Maybe we should just remove this script and use "opam pin" instead.

. ./config.sh

# Removing $BUILD ensures that Menhir is rebuilt from scratch, but this is
# a bit slow.
# rm -rf $BUILD

# Recompile.
rm -f $SRC/installation.ml
echo "Building Menhir, MenhirLib, MenhirSdk..."
if ! make -C $SRC stage1 sdk >/dev/null ; then
  echo "Could not build Menhir. Stop."
  exit 1
fi

# Re-install MenhirLib and MenhirSdk.
echo "Removing old MenhirLib and MenhirSdk..."
ocamlfind remove menhirLib || true
ocamlfind remove menhirSdk || true
echo "Installing new MenhirLib..."
cp -f $SRC/menhirLib.META $SRC/META
trap "{ rm -f $SRC/META ; }" EXIT
if ! ocamlfind install menhirLib \
  $SRC/META $BUILD/menhirLib.cmi $BUILD/menhirLib.cmo \
  $BUILD/menhirLib.cmx $BUILD/menhirLib.o ; then
  echo "Could not install MenhirLib. Stop."
  exit 1
fi
echo "Installing new MenhirSdk..."
cp -f $SRC/menhirSdk.META $SRC/META
if ! ocamlfind install menhirSdk \
  $SRC/META $SDKDIR/menhirSdk.cmi $SDKDIR/menhirSdk.cmo \
  $SDKDIR/menhirSdk.cmx $SDKDIR/menhirSdk.o; then
  echo "Could not install MenhirSdk. Stop."
  exit 1
fi
