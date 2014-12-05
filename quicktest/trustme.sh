#!/bin/bash

# This script re-generates the reference files used by test.sh for comparison.
# One should call it only when one trusts that Menhir is currently working!

. ./config.sh

# Make sure Menhir and MenhirLib are up-to-date.
./build.sh

# Build the parser with the code back-end and run it.
echo "Building (code)..."
make -C $CALC clean >/dev/null
make -C $CALC MENHIR="$MENHIR --trace" >/dev/null
for f in $DATA/*.real.in ; do
  b=${f%.in}
  echo "($b) Reconstructing reference output and trace..."
  $CALC/calc < $f > $b.ref.out 2> $b.ref.err
done

# Run the reference interpreter.
for f in $DATA/*.ideal.in ; do
  b=${f%.in}
  echo "($b) Reconstructing reference output and trace..."
  $MENHIR --trace --interpret $CALC/parser.mly < $f > $b.ref.out 2> $b.ref.err
done

echo "Done."
