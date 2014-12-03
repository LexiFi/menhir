#!/bin/sh

# This script re-generates the reference files used by test.sh for comparison.
# One should call it only when one trusts that Menhir is currently working!

. ./config.sh

# Make sure Menhir and MenhirLib are up-to-date.
./build.sh

# Build the parser with the code back-end and run it.
echo "Building and running (code)..."
make clean >/dev/null
make MENHIR="$MENHIR --trace" >/dev/null
echo "122 + 2 * 3 + 128" | ./calc > ref.out 2> ref.err

# Run the reference interpreter.
echo "Running the reference interpreter..."
echo "INT PLUS INT TIMES INT PLUS INT EOL" | $MENHIR --trace --interpret parser.mly > interpret-ref.out 2> interpret-ref.err

echo "Done."
