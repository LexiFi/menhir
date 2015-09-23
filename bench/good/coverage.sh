#!/bin/bash

# This script runs menhir --list-errors on every grammar in bench/good,
# aborting if it takes too long.

TIMEOUT=120

export OCAMLRUNPARAM=b
SRC=../../src
MENHIR=../../src/_stage2/menhir.native

# Compile Menhir.
make -C $SRC clean || exit 1
make -C $SRC bootstrap || exit 1
# Remove any leftover output files.
rm -f log lr.csv
# Print the header of lr.csv. (This should be kept in sync with LRijkstra.ml.)
echo "grammar,terminals,nonterminals,size,states,trie,facts,edges,time,heap" > lr.csv
# Try every grammar. (Only the single-file grammars, that is.)
for f in `ls *.mly | egrep -v '.*-([1-9]).mly'` ; do
  echo "Now dealing with: $f" | tee -a log
  (timeout $TIMEOUT $MENHIR -lg 1 -la 2 --list-errors --lalr $f) >>log 2>&1
  echo "Done dealing with: $f" >> log
done
echo "Number of grammars that could not be handled in $TIMEOUT seconds:"
grep aborting log | wc -l
