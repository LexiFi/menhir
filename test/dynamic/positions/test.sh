#!/bin/bash
set -euo pipefail

# We try every input file whose name matches *.in.
# We parse it using each of the parsers,
# and compare the results pairwise.

MAIN="calc"
TARGETS="ocamlyacc menhir_code menhir_code_inline menhir_table menhir_table_inline"

# Announce this test.

echo "Position check: testing..."

# List the input files.

inputs=$(ls input/ | sed -e "s/.in$//")

# Create the output files.

for target in $TARGETS ; do
  rm -rf $target/output
  mkdir -p $target/output
done

for f in $inputs ; do
  # echo "Processing $f..."
  for target in $TARGETS ; do
    # echo "  ($target)" ;
    $target/$MAIN.exe < input/$f.in > $target/output/$f.out
  done
done

# Compare the output files.

rm -rf log
mkdir log

msg="Position check: all comparisons passed, OK."
exitcode=0

for pair in \
  ocamlyacc/menhir_code \
  ocamlyacc/menhir_table \
  menhir_table/menhir_table_inline \
  menhir_code/menhir_code_inline
do
  left=${pair%/*}
  right=${pair#*/}
  for f in $inputs ; do
    out=output/$f.out
    log=log/$f.$left.$right.log
    if ! diff $left/$out $right/$out > $log ; then
      echo "$left versus $right: $f: FAILURE"
      cat $log
      msg="Position check: some comparisons failed, FAILURE."
      exitcode=1
    fi
  done
done

# Print a summary line.

echo $msg
exit $exitcode
