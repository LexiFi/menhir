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
  for backend_folder in src/*.backend ; do
    
    backend2=${backend_folder%.backend};
    backend=${backend2:4};
    echo "Running the $backend back-end..."
    $backend_folder/houblix.exe --input $file > $base.$backend.out 2> $base.$backend.time
  done
done
# Compute some statistics.
  echo
  ocaml speed.ml
