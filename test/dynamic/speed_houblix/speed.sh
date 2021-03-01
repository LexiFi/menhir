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

for file in src/*.tokens ; do
  echo "Test file: $file"
  base=${file%.tokens};
  # echo Dry run:
  # $TIME -f "%U" src/code/gene.exe --size $size --dry-run

  # Run the code back-end.
  echo Running the code back-end...
  src/code/houblix.exe --input $file > $base.code.out 2> $base.code.time
  # cat src/code.time

  # Run the code back-end.
  echo Running the old code back-end...
  src/old_code/houblix.exe --input $file > $base.old_code.out 2> $base.old_code.time
  # cat src/code.time

  # Run the table back-end.
  echo Running the table back-end...
  src/table/houblix.exe --input $file > $base.table.out 2> $base.table.time
  # cat src/table.time

  # Avoid a gross mistake.
  if ! diff -q $base.code.out $base.table.out ; then
    echo CAUTION: the code and table back-ends disagree!
    echo Code:
    cat src/code.out
    echo Table:
    cat src/table.out
    exit 1
  fi
  
  # Avoid a gross mistake.
  if ! diff -q $base.code.out $base.old_code.out ; then
    echo CAUTION: the code and old code back-ends disagree!
    echo Code:
    cat src/code.out
    echo Table:
    cat src/table.out
    exit 1
  fi

  # Run the ocamlyacc parser.
  #echo Running ocamlyacc...
  #src/ocamlyacc/gene.exe --size $size > src/ocamlyacc.out 2> src/ocamlyacc.time
  # cat src/ocamlyacc.time

  # Avoid another mistake.
  #if ! diff -q src/code.out src/ocamlyacc.out ; then
  #  echo CAUTION: Menhir and ocamlyacc disagree!
  #  echo Menhir:
  #  cat src/code.out
  #  echo ocamlyacc:
  #  cat src/ocamlyacc.out
  #  exit 1
  #fi

done
# Compute some statistics.
  echo
  ocaml speed.ml
