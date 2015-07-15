#!/bin/bash

# Example usage: ./compare.sh mezzo
# This script processes mezzo.mly using two versions of Menhir:
# 1- the last committed version;
# 2- the current (uncommitted) version.
# It then compares the output.

# The variable OPT can be used to pass extra options to Menhir.
# Use as follows:
# OPT=--lalr ./compare.sh ocaml

BENCH=../bench/good
MENHIR=_stage1/menhir.native
BASE="-v -lg 1 -la 1"

if [ $# -eq 0 ]
then
  echo "$0: at least one argument expected"
  exit 1
fi

export OCAMLRUNPARAM=b

# Go back to the last committed version.
git stash
echo "Compiling (last committed version)..."
(
  make &> compile.old || exit 1
  for FILE in "$@"
  do
   echo "Running ($FILE.mly)..."
   { time $MENHIR $BASE $OPT $BENCH/$FILE.mly ; } &>$FILE.old
  done
)

# Come back to the current (uncommitted) version.
git stash pop
echo "Compiling (current uncommitted version)..."
(
  make &> compile.new || exit 1
  for FILE in "$@"
  do
    echo "Running ($FILE.mly)..."
    { time $MENHIR $BASE $OPT $BENCH/$FILE.mly ; } &>$FILE.new
  done
)

# Diff.
for FILE in "$@"
do
  echo "Diffing ($FILE.mly)..."
  diff $FILE.old $FILE.new
done
