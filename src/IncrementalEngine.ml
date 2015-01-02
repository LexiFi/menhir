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

  (* [AboutToReduce] is an intermediate result. It means that the parser is
     about to perform a reduction step. It does not need more input at this
     point. The parser suspends itself at this point only in order to give the
     user an opportunity to observe this reduction step. *)

  (* [HandlingError] is an intermediate result. It means that the parser has
     detected an error and is currently handling it, in several steps. It does
     not need more input at this point. The parser suspends itself at this
     point only in order to give the user an opportunity to handle this error
     in a different manner, if desired. *)

  type env

  type production

  type 'a result = private
    | InputNeeded of env
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

  (* A stream is a list whose elements are produced on demand. *)

  type 'a stream =
      'a head Lazy.t

  and 'a head =
    | Nil
    | Cons of 'a * 'a stream

  (* We offer a read-only view of the parser's state as a stream of elements. *)

  val view: env -> element stream

end

(* TEMPORARY comment/document *)

module type INSPECTION = sig

  type xsymbol

  type production

  val lhs: production -> xsymbol

  val rhs: production -> xsymbol list

end

