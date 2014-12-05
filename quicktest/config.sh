SRC=../src
BUILD=$SRC/_stage1

# An absolute path for Menhir.
MENHIR=$BUILD/menhir.native
if which greadlink >/dev/null ; then
  MENHIR=`greadlink -f $MENHIR`
else
  MENHIR=`readlink -f $MENHIR`
fi

CALC=calc
DATA=calc-data

GENE=gene
