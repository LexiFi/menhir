#!/bin/sh

# This script checks that the code back-end, the table back-end, and the
# reference interpreter appear to be working correctly. It uses the calc
# demo for this purpose.

. ./config.sh

# Make sure Menhir and MenhirLib are up-to-date.
./build.sh

# Build the parser with the code back-end and run it.
echo "Building and running (code)..."
make clean >/dev/null
make MENHIR="$MENHIR --trace" >/dev/null
for f in *.real.in ; do
  b=${f%.in}
  ./calc < $f > $b.code.out 2> $b.code.err
done

# Build the parser with the table back-end and run it.
echo "Building and running (table)..."
make clean >/dev/null
make MENHIR="$MENHIR --trace --table" >/dev/null
for f in *.real.in ; do
  b=${f%.in}
  ./calc < $f > $b.table.out 2> $b.table.err
done

# Run the reference interpreter.
echo "Running the reference interpreter..."
echo "INT PLUS INT TIMES INT PLUS INT EOL" | $MENHIR --trace --interpret parser.mly > interpret.out 2> interpret.err

echo "Comparing results..."

# Compare the results to the reference.
for mode in code table ; do
  for f in *.real.in ; do
    b=${f%.in}
    if ! diff -q $b.ref.out $b.$mode.out >/dev/null ; then
      echo "($f) The $mode parser produces a wrong result!"
      echo "Expected:"
      cat $b.ref.out
      echo "Got:"
      cat $b.$mode.out
    fi
  done
  if ! diff -q $b.ref.err $b.$mode.err >/dev/null ; then
    echo "($f) The $mode parser produces a wrong trace!"
    diff $b.ref.err $b.$mode.err
  fi
done

# Check the results of the reference interpreter.
if ! diff -q interpret-ref.out interpret.out >/dev/null ; then
  echo "The reference interpreter produces a wrong result!"
  echo "Expected:"
  cat interpret-ref.out
  echo "Got:"
  cat interpret.out
fi
if ! diff -q interpret-ref.err interpret.err >/dev/null ; then
  echo "The reference interpreter produces a wrong trace!"
  diff interpret-ref.err interpret.err
fi

echo "Done."
