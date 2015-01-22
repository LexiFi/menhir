(* This module is part of MenhirLib. *)

(* An explanation is a description of what the parser has recognized in the
   recent past and what it expects next. *)

type ('item, 'symbol) explanation = {

  (* An explanation is based on an item. *)
  item: 'item;

  (* A past. This is a non-empty sequence of (terminal and non-terminal)
     symbols, each of which corresponds to a range of the input file. These
     symbols correspond to the first half (up to the bullet) of the item's
     right-hand side. In short, they represent what we have recognized in
     the recent past. *)
  past: ('symbol * Lexing.position * Lexing.position) list;

  (* A future. This is a non-empty sequence of (terminal and non-terminal)
     symbols These symbols correspond to the second half (after the bullet)
     of the item's right-hand side. In short, they represent what we expect
     to recognize in the future, if this item is a good prediction. *)
  future: 'symbol list;

  (* A goal. This is a non-terminal symbol. It corresponds to the item's
     left-hand side. In short, it represents the reduction that we will
     be able to perform if we successfully recognize this future. *)
  goal: 'symbol

}

module Make
  (I : IncrementalEngine.EVERYTHING)
  (User : sig

    (* In order to submit artificial tokens to the parser, we need a function
       that converts a terminal symbol to a token. Unfortunately, we cannot
       (in general) auto-generate this code, because it requires making up
       semantic values of arbitrary OCaml types. *)

    val terminal2token: _ I.terminal -> I.token

  end)
: sig

  open I

  (* We build lists of explanations. These explanations may originate in
     distinct LR(1) states. *)

  (* TEMPORARY *)

  type reader =
    unit -> token * Lexing.position * Lexing.position

  (* TEMPORARY *)

  exception Error of (Lexing.position * Lexing.position) * (item, xsymbol) explanation list

  val entry: 'a I.result -> (Lexing.lexbuf -> token) -> Lexing.lexbuf -> 'a

end
