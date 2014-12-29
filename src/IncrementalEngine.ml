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

  (* [HandlingError] is also an intermediate result. It means that the parser
     has detected an error and is currently handling it, in several steps. It
     does not need more input at this point. The parser suspends itself at
     this point only in order to give the user an opportunity to handle this
     error in a different manner, if desired. *)

  (* The type [('a, 'pc) env] is shared by [InputNeeded] and [HandlingError].
     As above, the parameter ['a] is the type of the final semantic value.
     The phantom type parameter ['pc] is instantiated with [input_needed]
     or [handling_error], as appropriate. This prevents the user from
     calling [offer] when she should call [handle], or vice-versa. *)

  type input_needed
  type about_to_reduce
  type handling_error

  type ('a, 'pc) env

  type production

  type 'a result =
    | InputNeeded of ('a, input_needed) env
    | AboutToReduce of ('a, about_to_reduce) env * production
    | HandlingError of ('a, handling_error) env
    | Accepted of 'a
    | Rejected

  (* [offer] allows the user to resume the parser after it has suspended
     itself with a result of the form [InputNeeded env]. [offer] expects [env]
     as well as a new token and produces a new result. It does not raise any
     exception. *)

  val offer:
    ('a, input_needed) env ->
    token * Lexing.position * Lexing.position ->
    'a result

  (* [handle] allows the user to resume the parser after it has suspended
     itself with a result of the form [HandlingError env]. [handle] expects
     [env] and produces a new result. It does not raise any exception. *)

  val handle:
    ('a, handling_error) env ->
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

  val view: (_, _) env -> element stream

end
