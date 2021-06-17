#!/bin/bash

# This script downloads the pfff grammars.

BASE="https://raw.githubusercontent.com/returntocorp/pfff/develop/"

LANGUAGES="cpp go java js php python sql"

for LANG in $LANGUAGES ; do
  echo -n "Downloading parser: $LANG..." ;
  wget -O "pfff_$LANG.mly" "$BASE/lang_$LANG/parsing/parser_$LANG.mly" ||
  wget -O "pfff_$LANG.mly" "$BASE/lang_$LANG/parsing/Parser_$LANG.mly" ;
  echo " done"
done
