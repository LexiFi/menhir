(* $Header: /home/pauillac/formel1/fpottier/cvs/hmx/sets/main.ml,v 1.1 2001/03/23 12:29:45 fpottier Exp $ *)

(* This module implements a concrete toplevel typing loop. *)

module Loop = Loop.Make (struct

  exception LexerError
    = Lexer.Error

  type phrase
    = Sets.Hm.phrase

  let parse =
    Parser.phrase Lexer.token

  type scheme =
    Sets.Hm.scheme

  type environment =
    Sets.Hm.environment

  let run =
    Sets.Hm.run

  module Print =
    Sets.System.Print

  let handle = function
    | Sets.System.Inconsistency message ->
	message
    | other ->
	raise other

end)

