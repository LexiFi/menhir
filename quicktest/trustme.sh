#!/bin/sh

# This script re-generates the reference files used by test.sh for comparison.
# One should call it only when one trusts that Menhir is currently working!

. ./config.sh

# Make sure Menhir and MenhirLib are up-to-date.
./build.sh

# Build the parser with the code back-end and run it.
echo "Building (code)..."
make clean >/dev/null
make MENHIR="$MENHIR --trace" >/dev/null
for f in *.real.in ; do
  b=${f%.in}
  echo "($b) Reconstructing reference output and trace..."
  ./calc < $f > $b.ref.out 2> $b.ref.err
done

# Run the reference interpreter.
for f in *.ideal.in ; do
  b=${f%.in}
  echo "($b) Reconstructing reference output and trace..."
  $MENHIR --trace --interpret parser.mly < $f > $b.ref.out 2> $b.ref.err
done

echo "Done."
