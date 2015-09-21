#!/bin/bash

# This script runs menhir on every grammar in ../bench/good,
# aborting if it takes too long.

TIMEOUT=30

export OCAMLRUNPARAM=b
make clean || exit 1
make bootstrap || exit 1
rm -f log
for f in ../bench/good/*.mly ; do
  echo $f | tee -a log
  (timeout $TIMEOUT time _stage2/menhir.native -v -lg 1 -la 1 --list-errors $f) 2>> log
done
echo "Number of grammars that could not be handled in $TIMEOUT seconds:"
grep aborting log | wc -l
