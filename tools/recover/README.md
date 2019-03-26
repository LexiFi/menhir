# Menhir-recover

This tool makes Menhir-generated parsers resilient to errors.

When a parser is successfully processed with Menhir-recover, parse always
succeed (a semantic value -- an AST -- is always produced).

This is done by generating input to complete the parse. Doing so requires at
least the ability to generate semantic values (e.g to finish the parsing of `1
+`, Menhir-recover needs to be told how to produce integers).

But beside that the algorithm is grammar-independent.  Making a grammar
error-resilient or maintaining an error-resilient grammar requires few work.

## Using resilient-menhir

The integration relies on a few features of Menhir:

1. grammar attributes, which are used to pass meta-data to resilient-menhir
   (how to produce integers in the case above)
2. Menhir ability to save the grammar and the LR automaton in a
   program-friendly form (`--cmly`)
3. a custom interpretation loop that will handle errors at run-time.

In practice, 1. means that some changes in the grammar are required, 2. means
that a few more steps are needed in the build system to connect Menhir to
Menhir-recover, and 3. means that the parser is invoked in a slightly different
way during execution.

This tool only covers part 1. and 2. The runtime library is provided
separately. (TODO: runtime library is not there yet)

The `MenhirSdk` module lets us access Menhir representation of the grammar and
the automaton.

## The recovery pipeline

The algorithm for recovery splits the work in three main steps.

*Synthesis* answer the local question of producing the symbols necessary to
finish a rule.  It does not need to look at the stack, just at the automaton.

For instance, if the user wrote invalid input after a parenthesis `(` in an
arithmetic language, *Synthesis* will find a way to reach the corresponding
`)`.

*Recovery* establishes plans to finish parsing by combining multiple syntheses.
It does so by enumerating enough prefixes of the stack to know that synthesis
will always be able to progress.
This ensures that recovery is complete: any state the parser can reach can be
recovered from.

These two steps are static. They happen at compile time and can be customized
using _grammar attributes_ (see [Menhir manual, section 13.2](http://gallium.inria.fr/~fpottier/menhir/manual.html#sec78)).

*Dynamic recovery* is the last step and happen during execution. 
Using knowledge produced by synthesis and recovery , it will act on a parser.
Interesting behaviors can be obtained by doing partial recovery and then
resuming the normal Menhir parser, but these decisions do not affect the static
analysis.

In other words:
- *Synthesis* establishes a list of tactics to work with the grammar. 
- *Recovery* establishes strategies that apply tactics repeatedly .
- *Dynamic recovery* applies those strategies to a concrete instance of a
  parser.

**TODO**: Find better names :]

**TODO**: The static and dynamic split in Recovery might be revisited.
Static step is useful to guarantee that recovery is complete.
Different algorithms can deal with the dynamic step.

Recovery currently is greedy.
**TODO**: Implement it as an instance of Dijkstra short-path algorithm.
There are good reasons to believe that in practice, it will be linear in the
size of the stack.
