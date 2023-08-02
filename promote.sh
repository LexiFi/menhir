#!/bin/bash
set -euo pipefail

# This script promotes a specific test, named on the command line.

# To promote a test means to create or update its expected-output files.

# Examples:
#   ./promote.sh good/mezzo
#   ./promote.sh bad/option

# If this is a newly created test, then [make depend] should be run first
# for dune to know about this test.

for name in "$@"
do
  # Construct the names of the expected-output files.
  if [[ $name =~ ^good/.* ]] ; then
    # A positive test.
    expected="NAME.opp.exp NAME.exp NAME.automaton.exp NAME.automaton.resolved.exp NAME.conflicts.exp"
  elif [[ $name =~ ^bad/.* ]] ; then
    # A negative test.
    expected="NAME.exp"
  else
    # Unrecognized.
    echo "Don't know what to do with '$name'."
    echo "This script handles tests whose name begins with good/ or bad/."
    exit 1
  fi
  expected="${expected//NAME/test/static/$name}"
  # Ask dune to update the expected-output files.
  echo "Promoting $name..."
  dune build $expected --auto-promote
    # TODO this command does not have the desired behavior
    #      (to rebuild/promote just the files $expected)
done
