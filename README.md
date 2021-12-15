# Menhir

Menhir is an LR(1) parser generator for OCaml.

Menhir has a [home page](http://cambium.inria.fr/~fpottier/menhir/).

## Installation

The latest released version of Menhir can be easily installed via
`opam`, OCaml's package manager. Just type `opam install menhir`.

For manual installation, see [INSTALLATION.md](INSTALLATION.md).

Some instructions for developers can be found in [HOWTO.md](HOWTO.md).

## Main Authors and Contributors

Menhir was created in 2005 by **François Pottier**
and **Yann Régis-Gianas**.

In the beginning, Yann Régis-Gianas contributed the front-end (parsing `.mly`
files, joining multiple `.mly` files, expanding away `%inline` definitions,
expanding away parameterized nonterminal symbols, etc.) while François Pottier
contributed the middle-end (the LR(1) construction algorithms, the LR(1)
conflict explanation algorithm, etc.) and the first back-end (producing OCaml
code).

A second back-end (producing OCaml tables, and relying on the interpreter in
the `menhirLib` library) was implemented in 2008. An initial implementation by
Raja Boujbel and Guillaume Bau was completed and polished by François Pottier.

A third back-end (producing Coq tables, and relying on a verified validator
and a verified interpreter in the `coq-menhirlib` library) was implemented in
2011 by Jacques-Henri Jourdan.

The incremental API, the inspection API, the notion of *attribute*, and the
library `MenhirSdk` were contributed between 2014 and 2017 by Frédéric Bour.

The switch to the `dune` build system was made in 2019 with the help of
Nicolás Ojeda Bär.

The reachability algorithm behind `menhir --list-errors` was implemented in
2015 by François Pottier. A much faster algorithm was contributed in 2021 by
Frédéric Bour.

The new code back-end, which produces efficient and well-typed OCaml code, was
implemented throughout the year 2021 by Émile Trotignon and François Pottier.
It relies on a new intermediate language, StackLang.

Today, the principal maintainer of Menhir is François Pottier.

The intellectual property of Menhir (including the OCaml libraries `menhirLib`
and `menhirSdk`) lies with Inria. The intellectual property of the Coq library
`coq-menhirlib` lies with Inria and CNRS.
