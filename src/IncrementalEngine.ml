open General

(* This signature describes the incremental LR engine. *)

(* In this mode, the user controls the lexer, and the parser suspends
   itself when it needs to read a new token. *)

module type INCREMENTAL_ENGINE = sig

  type token

  (* The type ['a result] represents an intermediate or final result of the
     parser. An intermediate result is a suspension: it records the parser's
     current state, and allows parsing to be resumed. The parameter ['a] is
     the type of the semantic value that will eventually be produced if the
     parser succeeds. *)

  (* [Accepted] and [Rejected] are final results. [Accepted] carries a
     semantic value. *)

  (* [InputNeeded] is an intermediate result. It means that the parser wishes
     to read one token before continuing. *)

  (* [Shifting] is an intermediate result. It means that the parser is taking
     a shift transition. It exposes the state of the parser before and after
     the transition. The Boolean parameter tells whether the parser intends to
     request a new token after this transition. (It always does, except when
     it is about to accept.) *)

  (* [AboutToReduce] is an intermediate result. It means that the parser is
     about to perform a reduction step. It exposes the parser's current
     state as well as the production that is about to be reduced. *)

  (* [HandlingError] is an intermediate result. It means that the parser has
     detected an error and is currently handling it, in several steps. *)

  type env

  type production

  type 'a result = private
    | InputNeeded of env
    | Shifting of env * env * bool
    | AboutToReduce of env * production
    | HandlingError of env
    | Accepted of 'a
    | Rejected

  (* [offer] allows the user to resume the parser after it has suspended
     itself with a result of the form [InputNeeded env]. [offer] expects the
     old result as well as a new token and produces a new result. It does not
     raise any exception. *)

  val offer:
    'a result ->
    token * Lexing.position * Lexing.position ->
    'a result

  (* [resume] allows the user to resume the parser after it has suspended
     itself with a result of the form [AboutToReduce (env, prod)] or
     [HandlingError env]. [resume] expects the old result and produces a new
     result. It does not raise any exception. *)

  val resume:
    'a result ->
    'a result

  (* The abstract type ['a lr1state] describes the non-initial states of the
     LR(1) automaton. The index ['a] represents the type of the semantic value
     associated with this state's incoming symbol. *)

  type 'a lr1state

  (* An element is a pair of a non-initial state [s] and a semantic value [v]
     associated with the incoming symbol of this state. The idea is, the value
     [v] was pushed onto the stack just before the state [s] was entered. Thus,
     for some type ['a], the type [s] has type ['a lr1state] and the value [v]
     has type ['a]. In other words, the type [element] is an existential type. *)

  type element =
    | Element: 'a lr1state * 'a * Lexing.position * Lexing.position -> element

  (* The parser's stack is (or, more precisely, can be viewed as) a stream of
     elements. The type [stream] is defined by the module [General]. *)

  type stack =
    element stream

  (* This is the parser's stack, a stream of elements. This stream is empty if
     the parser is in an initial state; otherwise, it is non-empty.  The LR(1)
     automaton's current state is the one found in the top element of the
     stack. *)

  val stack: env -> stack

  (* These are the start and end positions of the current lookahead token. It
     is legal to invoke this function only after at least one token has been
     offered to the parser via [offer]. In other words, it is illegal to
     invoke it in an initial state. *)

  val positions: env -> Lexing.position * Lexing.position

end

(* This signature is a fragment of the inspection API that is made available
   to the user when [--inspection] is used. This fragment contains type
   definitions for symbols. *)

module type SYMBOLS = sig

  (* The type ['a terminal] represents a terminal symbol. The type ['a
     nonterminal] represents a nonterminal symbol. In both cases, the index
     ['a] represents the type of the semantic values associated with this
     symbol. The concrete definitions of these types are generated. *)

  type 'a terminal
  type 'a nonterminal

  (* The type ['a symbol] represents a terminal or nonterminal symbol. It is
     the disjoint union of the types ['a terminal] and ['a nonterminal]. *)

  type 'a symbol =
    | T : 'a terminal -> 'a symbol
    | N : 'a nonterminal -> 'a symbol

  (* The type [xsymbol] is an existentially quantified version of the type
     ['a symbol]. This type is useful in situations where the index ['a]
     is not statically known. *)

  type xsymbol = 
    | X : 'a symbol -> xsymbol

end

(* This signature describes the inspection API that is made available to the
   user when [--inspection] is used. *)

module type INSPECTION = sig

  (* The types of symbols are described above. *)

  include SYMBOLS

  (* The type ['a lr1state] is meant to be the same as in [INCREMENTAL_ENGINE]. *)

  type 'a lr1state

  (* The type [production] is meant to be the same as in [INCREMENTAL_ENGINE].
     It represents a production of the grammar. A production can be examined
     via the functions [lhs] and [rhs] below. *)

  type production

  (* An LR(0) item is a pair of a production [prod] and a valid index [i] into
     this production. That is, if the length of [rhs prod] is [n], then [i] is
     comprised between 0 and [n], inclusive. *)

  type item =
      production * int

  (* Ordering functions. *)

  val compare_terminals: _ terminal -> _ terminal -> int
  val compare_nonterminals: _ nonterminal -> _ nonterminal -> int
  val compare_symbols: xsymbol -> xsymbol -> int
  val compare_words: xsymbol list -> xsymbol list -> int
  val compare_productions: production -> production -> int
  val compare_items: item -> item -> int

  (* [incoming_symbol s] is the incoming symbol of the state [s], that is,
     the symbol that the parser must recognize before (has recognized when)
     it enters the state [s]. This function gives access to the semantic
     value [v] stored in a stack element [Element (s, v, _, _)]. Indeed,
     by case analysis on the symbol [incoming_symbol s], one discovers the
     type ['a] of the value [v]. *)

  val incoming_symbol: 'a lr1state -> 'a symbol

  (* [lhs prod] is the left-hand side of the production [prod]. This is
     always a non-terminal symbol. *)

  val lhs: production -> xsymbol

  (* [rhs prod] is the right-hand side of the production [prod]. This is
     a (possibly empty) sequence of (terminal or nonterminal) symbols. *)

  val rhs: production -> xsymbol list

  (* [items s] is the set of the LR(0) items in the LR(0) core of the LR(1)
     state [s]. This set is not epsilon-closed. This set is presented as a
     list, in an arbitrary order. *)

  val items: _ lr1state -> item list

  (* [nullable nt] tells whether the non-terminal symbol [nt] is nullable.
     That is, it is true if and only if this symbol produces the empty
     word [epsilon]. *)

  val nullable: _ nonterminal -> bool

  (* [first nt t] tells whether the FIRST set of the nonterminal symbol [nt]
     contains the terminal symbol [t]. That is, it is true if and only if
     [nt] produces a word that begins with [t]. *)

  val first: _ nonterminal -> _ terminal -> bool

  (* [xfirst] is analogous to [first], but expects a first argument of type
     [xsymbol] instead of [_ terminal]. *)

  val xfirst: xsymbol -> _ terminal -> bool

  (* [foreach_terminal] enumerates the terminal symbols, including [error].
     [foreach_terminal_but_error] enumerates the terminal symbols, excluding
     [error]. *)

  val foreach_terminal:           (xsymbol -> 'a -> 'a) -> 'a -> 'a
  val foreach_terminal_but_error: (xsymbol -> 'a -> 'a) -> 'a -> 'a

end

(* This signature combines the incremental API and the inspection API. *)

module type EVERYTHING = sig

  include INCREMENTAL_ENGINE

  include INSPECTION
    with type 'a lr1state := 'a lr1state
    with type production := production

end

