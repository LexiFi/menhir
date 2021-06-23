#!/bin/bash

# Create times directory if it does not exist
mkdir -p times

# Remove any stale performance measurements.
rm -f times/*.time
benchname=$(basename "$(pwd)")

# Print a new line
echo ""

# Save the cursor position
tput sc

# A loop with several input sentences.
for file in sentences/*.tokens ; do
  base2=${file%.tokens};
  base=${base2:10};
  # A loop with several backends
  for backend_folder in backends/*.backend ; do
    backend2=${backend_folder%.backend};
    backend=${backend2:9} ;
    echo -n "running [test file: $file] [backend: $backend] ..." ;
    "$backend_folder"/main.exe --input "$file" > times/"$base"."$backend".out 2> times/"$base"."$backend".time ;
    tput rc ;
    tput el
  done
done
./speed.exe "$benchname"
tput sc