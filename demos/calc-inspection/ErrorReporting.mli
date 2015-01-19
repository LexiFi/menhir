module Make
  (I : MenhirLib.IncrementalEngine.EVERYTHING)
  (User : sig

    (* In order to submit artificial tokens to the parser, we need a function
       that converts a terminal symbol to a token. Unfortunately, we cannot
       (in general) auto-generate this code, because it requires making up
       semantic values of arbitrary OCaml types. *)

    val terminal2token: _ I.terminal -> I.token

  end)
: sig

  open I

  (* An explanation is a description of what the parser has recognized in the
     recent past and what it expects next. For now, an explanation is just an
     item. *)

  type explanation =
      item

  (* We build lists of explanations. These explanations may originate in
     distinct LR(1) states. *)

  type explanations =
      explanation list

  (* TEMPORARY *)

  type reader =
    unit -> token * Lexing.position * Lexing.position

  (* TEMPORARY *)

  exception Error of explanations

  val entry: 'a I.result -> (Lexing.lexbuf -> token) -> Lexing.lexbuf -> 'a

end
