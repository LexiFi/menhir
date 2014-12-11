#!/bin/sh

# This script tries to find the Menhir executable.
# This is useful because we would like the demos
# to work regardless of whether Menhir has been
# installed already.

# A normal user does not need this script. One can
# assume that Menhir has been installed.

# First attempt: find Menhir in the src directory
# of the Menhir distribution.
# This loop assumes that we are somewhere within
# the Menhir distribution, so by going up, we will
# end up at the root of the distribution.
while ! [ -d src ] ; do
  cd ..
done
LOCAL=src/_stage1/menhir.native
if ls $LOCAL >/dev/null 2>/dev/null ; then
  echo `pwd`/$LOCAL
  exit 0
fi

# Second attempt: find Menhir in the PATH.
if which menhir >/dev/null ; then
  echo menhir
  exit 0
fi

echo Error: could not find Menhir.
exit 1

