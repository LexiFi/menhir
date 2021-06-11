# Benchmarks

This directory contains benchmarks for menhir.
The general shape of the benchmarks is specified in the `template` directory.
Every directory this neither `build` nor `template` contains a parser that we
wish to benchmark. Two files are needed to specify such a parser :
a `parser.mly` file, containing an entry point called `main`, and a
`simple_lexer.mll` file, that is a lexer that accepts a sentence printed as a
senquence of the tokens name, separated by new lines. This lexer is very similar
for every parser, so we may automate this part later.

Calling the `build.sh` script copies the file at the right position in the
`build` directory.

You may launch the benchmarks using the `benchmark` rule from the main makefile,
it will call the `build.sh` script before launching it.
