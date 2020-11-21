# TODO

## Automaton

* Although `StackSymbols` computes a known suffix of the stack that is
  directly deduced from the LR(1) items that appear inside each state, it
  would be possible to infer a longer known suffix in each state by performing
  a data flow analysis. Implement this analysis and use it when displaying the
  known suffix of the stack in a `.messages` file and/or in an `.automaton`
  file. It could be useful in the code back-end as well.

## Performance

* Conflict explanation can be very slow when there are many conflicts.
  Can we do something about it?

* In the table back-end, writing a specialized version of `PackedIntArray.get`
  to the generated `.ml` file could allow a performance gain. (Suggested by
  Frédéric Bour.)

## Enhancements

* Extensions to the incremental API:
    * Expose a function `lookahead` of type `'a env -> token option`.
    * Expose `print_checkpoint`.
    * Define a printer for stacks.

* Extensions to the inspection API:
    * Offer a way of mapping an initial state (number) back to its
      nonterminal symbol.
    * Expose the number of states (useful for memoization).
    * Expose the number of productions.
    * Expose an isomorphism between `token` and `xtoken`,
      defined as `exists 'a. ('a terminal * 'a)`.
      The functions `T.token2terminal` and `T.token2value` offer one direction,
      from `token` to `xtoken`. The other direction would have to be generated.

* The inspection API does not allow determining the current state in the
  special case where the stack is empty (which implies that we are in an
  initial state). Could this state be exposed?
  One problem is that `incoming_symbol` would then need to return an `option`.
  The data constructor `Nil` should contain the initial state.
  Beware: applying `items` to this initial state would produce
  the special item `S' -> S #`, creating a problem.
  Should the initial state have a different type?
  Or should we just expose the start symbol, not the start state?

## Minor suggestions for enhancements

* Add a new flag `--show-stdlib` to print `standard.mly`. (Suggested by
  Gabriel Scherer.)

* Allow `%type` declarations to appear in the rule section.

* Allow the generated types (`token`, etc.) to be annotated with OCaml
  attributes such as `[@@deriving show]`.
  The only question is how to request this most elegantly: in the `.mly` file
  (with what syntax?) or via the command line. (Issue #6.)

* Add a command line option to control how `MenhirLib` should be named in the
  generated code. (Suggested by Gabriel Scherer.)

* Allow a rule of the form `foo == bar` as sugar for `foo: bar { $1 }`?
  (Allow parameters. Allow several alternatives on the right-hand side,
  as long as each alternative has length 1.) (Frédéric Bour.)
  (Gabriel Scherer,
   [A suggestion for a more expressive production syntax](https://sympa.inria.fr/sympa/arc/menhir/2017-05/msg00003.html).)
  (Always parenthesize production groups that share a semantic action?)
  (Or just disallow them?)

* Move more parts of the module `ErrorReports` from CompCert to MenhirLib.
  In `show`, might want to apply `shorten` to the output of `f`.

* Complete the implementation of `--only-preprocess-for-ocamlyacc`.
  Currently, the position keywords (`$startpos`, etc.) are not supported.
  Translate them to calls to the `Parsing` module.
  Document this command.

* The documentation says not to use `_i`,
  but `--only-preprocess` produces it.
  Should we avoid producing it?
  Should we check that it is not used?

* `--compile-errors` could warn about messages that are wider than 80 columns.

* Could `--compile-errors` warn statically about uses of `$i` that are
  out of range?

* Implement --copy-errors to copy error messages from one .messages
  file to another (for those states that exist in both files).
  Or --merge-errors which merges two .messages file,
  recognizing the default message as irrelevant.
  (Gabriel Scherer.)

* Allow writing `%token FOO [@unused]`.
  This should be equivalent to passing `--unused-token FOO`
  on the command line.
  Add an analogous mechanism for nonterminal symbols that are known
  to be unreachable.

* Produce well-chosen (predictable) names for anonymous rules?

* Recognize `#` directives in the `.mly` file and take them into account.

* Allow `--trace` mode to use some printing function other than `fprintf
  stderr`. That would allow the programmer to decide at runtime whether
  a trace should be printed, for instance. Also, give users access to
  the function `print_token` that is generated in `--trace` mode?
  (Issue #6.)

* Preserve the directory name in the `#` directives that we emit.
  (Not sure if this is necessary.)

* In the file `.automaton.resolved`, show which states have default
  reductions. (When this is the case, do not show individual reduction
  actions.)

## Scripts

* In `make data`, the message `TIMEOUT after 60 seconds` should be printed
  only if the exit code is 124.

## Installation

* `generate-printers` should not be a demo but a tool, and should be
  installed. That would solve issue #6. Even more convenient would be
  to build its functionality directly into Menhir.

## Minor bugs

* A drawback of point-free semantic actions: if the application of the OCaml
  identifier to the tuple is ill-typed, then we get a type error in the mock
  file. Can this be fixed? Generate `#` directives around the application node.

## Demos

* Clean up the `calc-inspection` demo, which uses an undocumented/unfinished
  approach to error reporting.

## Major new features

* Currently we warn about nonterminals that generate the empty language. Could
  we allow this on purpose? If the right-hand side of a production provably
  generates the empty language, we could remove this production, and come back
  to the current setting where every nonterminal symbol is inhabited. The
  point is that this allows defining the empty language by `void: void` and
  using `void` as an actual parameter in parameterized definitions. This
  allows making extensible definitions and instantiating them with "no
  extension". Probably a `%void` annotation would be needed to indicate that a
  symbol is intended to generate an empty language.

* Add a facility to produce a set of *valid* sentences that reach *every*
  reachable state of the automaton and exercise *every* reduction. This could
  be useful in regression testing.

* Think about implementing Adams and Might's technique for intersecting a
  grammar with a tree automaton. (As a preprocessing step. Think about the
  interaction with parameterized symbols and with `%inline`.) See also de
  Souza Amorim, Steindorfer & Visser.

* Integrate Frédéric Bour's work on concrete syntax trees.
    * Produce a definition of the specific type of CSTs for this grammar.
      (Before expansion.)
    * Produce a grammar where the semantic actions build CSTs.
    * Define a generic (internal) type of CSTs and
      produce a function that maps generic CSTs to specific CSTs.
    * "Uninliner" : map CSTs for the expanded grammar
      back to CSTs for the source grammar?
    * "Unparsing" : map an arbitrary CST to one that can be printed
      and re-parsed identically, while inserting a minimal number of
      "parentheses". Ideally, with a static guarantee of success,
      and in linear time.

* Allow the user to choose the type of "source code intervals" that are stored
  in the stack (instead of imposing a pair of locations, `$startpos` and
  `$endpos`) and to choose how intervals are combined. See [Frédéric's
  branch](https://gitlab.inria.fr/fpottier/menhir/tree/fred-abstract-locations).
  See also Frédéric's email to François entitled `Menhir avec $loc`
  (2018/07/26).

* Implement loop detection. If a grammar contains a loop (that is, a series of
  productions of the form `A -> B -> C -> ... -> A`), then it is infinitely
  ambiguous. Nullable symbols must be taken into account. As soon as there
  exists a production of the form `A -> alpha B beta` where `alpha` and `beta`
  are nullable, add the production `A -> B`. Then, test whether there exists
  a cycle of unit productions.

## Documentation

* Explain how to simulate inherited attributes
  (cf. post by David Chemouil on the mailing list).

* Document the fact that `yacc` and Menhir do not have the same behavior
  concerning default reductions (one performs the default reduction *before*
  calling the lexer, the other does it the other way around). The difference
  is observable only when the semantic action has a side effect which
  influences the lexer (i.e., only when there is a "lexer hack").

* Document the philosophical difference about conflicts between us and Denny &
  Malloy. They think conflicts are a feature and the state-merging technique
  should preserve the behavior of the automaton even in the presence of
  conflicts. We think conflicts should be avoided and it is ok for
  state-merging to alter the behavior of the automaton in the presence of
  conflicts.

## Code Cleanup

* Ideally, `PartialGrammar` should avoid renaming the parameters of
  a rule, when there is no need to do so. The renamed parameter is
  visible in sort unification error messages and in messages about
  duplicate formal parameters (`test/bad/duplicate-formal.mly`).

* Grep for `artificial dependency` and remove these artificial dependencies.
  Ideally, there should be no implicit (global) state ("the" grammar, "the"
  automaton). The grammar or the automaton should be explicit parameters
  everywhere. That would be a large change, though.

## Standard library

* Add more list forms (left-recursive lists, flexible lists, etc.).

* We might wish to add `epsilon` (which recognizes the empty string and
  produces a unit value), `nil` (which recognizes the empty string and
  produces an empty list), etc. However, they would need to be marked
  `%unreachable` (i.e., do not warn if they are unreachable).

## Bugs

* Confirm the following bug reports and turn them into gitlab issues.

* Message from Valentin Gatien-Baron (2010/01/09). Is this a known bug with
  `--explain`, i.e., the fact that conflicts in unreachable states cannot be
  explained? Can it be fixed? Should we just avoid reporting and explaining
  conflicts in unreachable states? (One might also wish to remove the states
  that become unreachable during conflict resolution, but that would require
  re-numbering states, which seems painful and desirable.) See `belloeil.mly`.
  See also Adrien Guatto's `reduced_parser.mly`. Also check `parser.mly`, sent
  by Andrej Bauer on 2016/01/21. Once this is issue is fixed, improve the test
  suite to verify that we never have unexplainable conflicts.

* When we are in error-handling mode and we attempt to reduce (because the
  current state can reduce), if the semantic action uses `$syntaxerror`, then
  we loop (because we catch the exception and end up in error-handling mode
  and in the same state). Bug reported by Gabriel Scherer about OCaml's
  grammar, bug number 0007847. Could we detect this case and handle it
  properly?

* The function `rewind` contains an incorrect comment ("every stack
  description either is definite or contains at least one represented state").
  Find out why it is incorrect. Find out what the implications for `rewind`
  are; does this imply that `rewind` can return `Die` too early?
