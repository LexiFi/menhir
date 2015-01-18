#!/bin/bash

# This script runs the speed test in gene/ (where the test input
# is a randomly-generated arithmetic expression).

. ./config.sh

# The time command.
if which gtime >/dev/null ; then
  TIME=gtime
else
  TIME=time
fi

# Testing ocamlyacc is optional.
test_ocamlyacc=false

# Make sure Menhir and MenhirLib are up-to-date.
./build.sh

# Remove any stale performance measurements.
rm -f gene/*.time

# Build the parser with the code back-end.
# Do not link MenhirLib.
echo "Building (code)..."
make -C $GENE MENHIR="$MENHIR" clean all >/dev/null
cp -RH $GENE/gene.native $GENE/gene.code

# Build the parser with the table back-end.
# Do link MenhirLib.
echo "Building (table)..."
make -C $GENE MENHIR="$MENHIR --table" TAGS="-tags 'package(menhirLib)'" clean all >/dev/null
cp -RH $GENE/gene.native $GENE/gene.table

# (Optionally) Build the parser with ocamlyacc.
if $test_ocamlyacc; then
  echo "Building (ocamlyacc)..."
  make -C $GENE OCAMLBUILD="ocamlbuild -use-ocamlfind" clean all >/dev/null
  cp -RH $GENE/gene.native $GENE/gene.ocamlyacc
fi

# A loop with several test sizes.

for size in 1000000 5000000 10000000 ; do
  echo "Test size: $size"

  # Dry run (measures the random generation time).
  echo Dry run:
  $TIME -f "%U" $GENE/gene.code --size $size --dry-run 2> $GENE/dry.time
  cat $GENE/dry.time

  # Run the code back-end.
  echo Code back-end:
  $TIME -f "%U" $GENE/gene.code --size $size > $GENE/code.out 2> $GENE/code.time
  cat $GENE/code.time

  # Run the table back-end.
  echo Table back-end:
  $TIME -f "%U" $GENE/gene.table --size $size > $GENE/table.out 2> $GENE/table.time
  cat $GENE/table.time

  # Avoid a gross mistake.
  if ! diff -q $GENE/code.out $GENE/table.out ; then
    echo CAUTION: the code and table back-ends disagree!
    echo Code:
    cat $GENE/code.out
    echo Table:
    cat $GENE/table.out
    exit 1
  fi

  # (Optionally) Run the ocamlyacc parser.

  if $test_ocamlyacc; then
    echo ocamlyacc:
    $TIME -f "%U" $GENE/gene.ocamlyacc --size $size > $GENE/ocamlyacc.out 2> $GENE/ocamlyacc.time
    cat $GENE/ocamlyacc.time
  fi

  # Compute some statistics.
  ocaml speed.ml

done

