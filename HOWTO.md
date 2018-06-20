# Developer guide

This guide is intended for new Menhir developers, and should explain how
things work.

For the moment, there is not much information in it.

## Build Instructions

There are two ways of recompiling Menhir after making a change in the sources.

To perform a single compilation pass, just go down into the `src` directory
and type `make`. This produces an executable file named `_stage1/menhir.native`.

To go further and check that Menhir can process its own grammar,
still in the `src` directory,
type `make bootstrap`.
This produces an executable file named `_stage2/menhir.native`.

`make bootstrap` occasionally fails for no good reason. In that case,
use `make clean` before attempting `make bootstrap` again.

## About the Module Ordering

Some toplevel modules have side effects and must be executed in the
following order:

| Settings		| parses the command line |
| PreFront		| reads the grammar description files |
| TokenType		| deals with `--only-tokens` and exits |
| Front			| deals with `--depend`, `--infer`, `--only-preprocess`, and exits |
| Grammar		| performs a number of analyses of the grammar |
| Lr0			| constructs the LR(0) automaton |
| Slr			| determines whether the grammar is SLR |
| Lr1			| constructs the LR(1) automaton |
| Conflict		| performs default conflict resolution and explains conflicts |
| Invariant		| performs a number of analyses of the automaton |
| Interpret		| deals with `--interpret` and exits |
| Back			| produces the output and exits |

A few artificial dependencies have been added in the code in order
to ensure that this ordering is respected by `ocamlbuild`.
