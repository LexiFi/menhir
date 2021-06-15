#!/bin/bash

# This script runs the speed test in gene/ (where the test input
# is a randomly-generated arithmetic expression).

# The time command.
if command -v gtime >/dev/null ; then
  TIME=gtime
else
  TIME=time
fi

# Create times directory if it does not exist
mkdir -p times

# Remove any stale performance measurements.
rm -f times/*.time
benchname=$(basename $(pwd))
echo ""
tput sc
# A loop with several test sizes.
for file in sentences/*.tokens ; do
  base2=${file%.tokens};
  base=${base2:10};
  for backend_folder in backends/*.backend ; do
    backend2=${backend_folder%.backend};
    backend=${backend2:9} ;
    echo -n "running [test file: $file] [backend: $backend] ..." ;
    $backend_folder/main.exe --input $file > times/$base.$backend.out 2> times/$base.$backend.time ;
    tput rc ;
    tput el
  done
done
ocaml speed.ml $benchname
tput sc