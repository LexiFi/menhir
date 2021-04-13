#!/bin/bash

# This script runs the speed test in gene/ (where the test input
# is a randomly-generated arithmetic expression).

# The time command.
if command -v gtime >/dev/null ; then
  TIME=gtime
else
  TIME=time
fi

# Remove any stale performance measurements.
rm -f src/*.time

# A loop with several test sizes.

for size in 5000000 ; do
  echo "Test size: $size"

  # echo Dry run:
  # $TIME -f "%U" src/code/gene.exe --size $size --dry-run

  # Run the code back-end.
  echo Running the code back-end...
  src/code/gene.exe --size $size > src/code.out 2> src/code.time

  # Run the code back-end.
  echo Running the code back-end...
  src/old_code/gene.exe --size $size > src/old_code.out 2> src/old_code.time
  # cat src/code.time

  # Run the table back-end.
  echo Running the table back-end...
  src/table/gene.exe --size $size > src/table.out 2> src/table.time
  # cat src/table.time

  # Avoid a gross mistake.
  if ! diff -q src/code.out src/table.out ; then
    echo CAUTION: the code and table back-ends disagree!
    echo Code:
    cat src/code.out
    echo Table:
    cat src/table.out
    exit 1
  fi

  # Avoid a gross mistake.
  if ! diff -q src/code.out src/old_code.out ; then
    echo CAUTION: the new and old code back-ends disagree!
    echo New:
    cat src/code.out
    echo Old:
    cat src/old_code.out
    exit 1
  fi

  # Run the ocamlyacc parser.
  echo Running ocamlyacc...
  src/ocamlyacc/gene.exe --size $size > src/ocamlyacc.out 2> src/ocamlyacc.time
  # cat src/ocamlyacc.time

  # Avoid another mistake.
  if ! diff -q src/code.out src/ocamlyacc.out ; then
    echo CAUTION: Menhir and ocamlyacc disagree!
    echo Menhir:
    cat src/code.out
    echo ocamlyacc:
    cat src/ocamlyacc.out
    exit 1
  fi

  # Compute some statistics.
  echo
  ocaml speed.ml

done
