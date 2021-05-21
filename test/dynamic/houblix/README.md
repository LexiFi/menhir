#### Houblix tests ####

This directory contains tests on the grammar of `hopix`, an ML-like language
which has been developed for a compilation course.

The `src` subdirectory contains the source code of `hopix`, along with multiple
"back-ends". A back-end is a set of Menhir settings. Each back-end is described
by a directory whose name ends in `.backend`. Each such directory contains a `dune`
file that specifies the desired options.

The `tests` subdirectory contains test directories, whose names end either
with `.sexp.tests` or with `.pretty.tests`. Pretty tests are tests were a
`hopix` source file is parsed and pretty-printed, and where the pretty-printed
output is compared against a reference. The `sexp` tests are the same, except
that an s-expression representation of the AST is printed. These tests are
stricter, as they check that correct positions are recorded in the AST.

The `tests` directory also contains the script `test.ml`, which generates a
file named `dune.auto` in the `tests` directory and in each of the
subdirectories `*.tests`. Running this script via the command `ocaml test.ml`
is sufficient. You should run this command every time you add or delete either
a test or a back-end.

You can run the tests in this directory by typing `dune build @test_houblix`.
