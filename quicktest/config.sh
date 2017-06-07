SRC=../src
# Make SRC an absolute path.
# (We need at least MENHIR to be an absolute path.)
if command -v greadlink >/dev/null ; then
  SRC=`greadlink -f $SRC`
else
  SRC=`readlink -f $SRC`
fi

BUILD=$SRC/_stage1
SDKDIR=$SRC/_sdk

MENHIR=$BUILD/menhir.native

CALC=calc
DATA=calc-data
GENE=gene
