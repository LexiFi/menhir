#!/bin/bash

# This script checks that the code back-end, the table back-end, and the
# reference interpreter appear to be working correctly. It uses the calc
# demo for this purpose.

. ./config.sh

# Make sure Menhir and MenhirLib are up-to-date.
./build.sh

# Build the parser with the code back-end and run it.
echo "Building and running (code)..."
make -C $CALC MENHIR="$MENHIR --trace" clean >/dev/null
make -C $CALC MENHIR="$MENHIR --trace"       >/dev/null
for f in $DATA/*.real.in ; do
  b=${f%.in}
  $CALC/calc < $f > $b.code.out 2> $b.code.err
done

# Build the parser with the table back-end and run it.
echo "Building and running (table)..."
make -C $CALC MENHIR="$MENHIR --trace --table" clean >/dev/null
make -C $CALC MENHIR="$MENHIR --trace --table"       >/dev/null
for f in $DATA/*.real.in ; do
  b=${f%.in}
  $CALC/calc < $f > $b.table.out 2> $b.table.err
done

# Run the reference interpreter.
echo "Running the reference interpreter..."
for f in $DATA/*.ideal.in ; do
  b=${f%.in}
  $MENHIR --trace --interpret $CALC/parser.mly < $f > $b.interpret.out 2> $b.interpret.err
done

echo "Comparing results..."

# Compare the results to the reference.
for f in $DATA/*.real.in ; do
  b=${f%.in}
  for mode in code table ; do
    echo "($b) Checking output of $mode parser..."
    if ! diff -q $b.ref.out $b.$mode.out >/dev/null ; then
      echo "($b) The $mode parser produces a wrong result!"
      echo "Expected:"
      cat $b.ref.out
      echo "Got:"
      cat $b.$mode.out
    fi
    echo "($b) Checking trace of $mode parser..."
    if ! diff -q $b.ref.err $b.$mode.err >/dev/null ; then
      echo "($b) The $mode parser produces a wrong trace!"
      diff $b.ref.err $b.$mode.err
    fi
  done
done

# Check the results of the reference interpreter.
for f in $DATA/*.ideal.in ; do
  b=${f%.in}
  echo "($b) Checking output of reference interpreter..."
  if ! diff -q $b.ref.out $b.interpret.out >/dev/null ; then
    echo "The reference interpreter produces a wrong result!"
    echo "Expected:"
    cat $b.ref.out
    echo "Got:"
    cat $b.interpret.out
  fi
  echo "($b) Checking trace of reference interpreter..."
  if ! diff -q $b.ref.err $b.interpret.err >/dev/null ; then
    echo "The reference interpreter produces a wrong trace!"
    diff $b.ref.err $b.interpret.err
  fi
done

echo "Done."
