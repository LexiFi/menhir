# Houblix tests

This directory contains tests on the grammar of hopix, an ml-like language
developped for a compilation course.
The `src` subdirectory contains the source of hopix, along with multiple
`backends` that is directory containing a dune file specifying menhir options,
whose name ends in `.backend`.
The `tests` subdirectory contains tests directories, whose names end either with
`.sexp.tests` or with `.pretty.tests`. Pretty tests are tests were a hopix is
parsed, and then pretty printed, and the pretty printed output is compared.
The sexp tests are the same, except that the s-expression representation of the
AST is printed. This tests are more strict because it checks that the correct
positions are recorded in the AST.

In the `tests` directory, there is also a `test.ml` that generates a dune.auto
for the `tests` directory, and each `.tests` directories.
It has no dependencies, so `ocaml test.ml` is sufficient to regenerate the dune
files. You should call it every time you add or delete either a test or a
backend.

You can run only these tests with `dune build @test_houblix`.