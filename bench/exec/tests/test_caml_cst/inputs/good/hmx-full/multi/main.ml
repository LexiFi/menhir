(* $Header: /home/pauillac/formel1/fpottier/cvs/hmx/multi/main.ml,v 1.1 2001/03/23 16:50:57 fpottier Exp $ *)

(* This module implements a concrete toplevel typing loop. *)

module Loop = Loop.Make (struct

  exception LexerError
    = Lexer.Error

  type phrase
    = Multi.Hm.phrase

  let parse =
    Parser.phrase Lexer.token

  type scheme =
    Multi.Hm.scheme

  type environment =
    Multi.Hm.environment

  let run =
    Multi.Hm.run

  module Print =
    Multi.System.Print

  let handle = function
    | Multi.System.Inconsistency message ->
	message
    | other ->
	raise other

end)

