#!/bin/bash

# This script runs the speed test in gene/ (where the test input
# is a randomly-generated arithmetic expression).

. ./config.sh

# The time command.
if which -s gtime ; then
  TIME=gtime
else
  TIME=time
fi

# Make sure Menhir and MenhirLib are up-to-date.
./build.sh

# Build the parser with the code back-end.
echo "Building (code)..."
make -C $GENE clean >/dev/null
make -C $GENE MENHIR="$MENHIR" >/dev/null

# Dry run (measures the random generation time).
echo Dry run:
$TIME -f "%U" $GENE/gene.native --dry-run 2> $GENE/dry.time
cat $GENE/dry.time

# Run the code back-end.
echo Code back-end:
$TIME -f "%U" $GENE/gene.native > $GENE/code.out 2> $GENE/code.time
cat $GENE/code.time

# Build the parser with the table back-end.
echo "Building (table)..."
make -C $GENE clean >/dev/null
make -C $GENE MENHIR="$MENHIR --table" >/dev/null

# Run the table back-end.
echo Table back-end:
$TIME -f "%U" $GENE/gene.native > $GENE/table.out 2> $GENE/table.time
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

# Compute some statistics.
ocaml speed.ml
