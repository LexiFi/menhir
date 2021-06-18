#!/bin/bash

# This script downloads the acgtk grammar,
# which is split over several files.

# We keep the separation into multiple files,
# but we must rename the files so as to follow
# the convention of our test script.

BASE="https://gitlab.inria.fr/ACG/dev/ACGtk/-/raw/master/src/grammars"

wget -O acgtk1.mly "$BASE/file_parser.mly"
wget -O acgtk2.mly "$BASE/sig_parser.mly"
wget -O acgtk3.mly "$BASE/lex_parser.mly"
wget -O acgtk4.mly "$BASE/term_type_parser.mly"
wget -O acgtk5.mly "$BASE/bound_term_parser.mly"
