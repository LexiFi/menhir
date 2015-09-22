#!/bin/bash

# This script runs menhir --list-errors on every grammar in bench/good,
# aborting if it takes too long.

TIMEOUT=2

export OCAMLRUNPARAM=b
SRC=../../src
MENHIR=../../src/_stage1/menhir.native

# Compile Menhir.
make -C $SRC || exit 1
# Remove any leftover output files.
rm -f log lr.csv
# Print the header of lr.csv. (This should be kept in sync with LRijkstra.ml.)
echo "grammar,terminals,nonterminals,size,states,trie,facts,edges,time" > lr.csv
# Try every grammar.
for f in *.mly ; do
  echo $f | tee -a log
  (timeout $TIMEOUT $MENHIR -lg 1 -la 2 --list-errors $f) &>> log
done
echo "Number of grammars that could not be handled in $TIMEOUT seconds:"
grep aborting log | wc -l
