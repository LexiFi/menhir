(* $Header: /home/pauillac/formel1/fpottier/cvs/hmx/loop.ml,v 1.3 2000/06/16 13:23:37 fpottier Exp $ *)

(* Given an arbitrary typechecker, this module implements a generic toplevel typing loop. *)

module type TypeChecker = sig

  (* This exception is assumed to be raised by the lexer. It carries an error message and two locations (i.e.
     character counts). *)

  exception LexerError of string * int * int

  (* Phrases, and a lexer/parser which creates them. *)

  type phrase

  val parse: Lexing.lexbuf -> phrase

  (* Type schemes, type environments, and the type inference algorithm. *)

  type scheme

  type environment =
      (string * scheme) list

  val run: environment -> phrase -> string * scheme

  (* Printing type schemes. *)

  module Print : sig

    val reset: unit -> unit
    val scheme: scheme -> string

  end

  (* This function is passed unknown exceptions (which are assumed to be internal typing exceptions), and is supposed
     to either return a printable error message, or re-raise the exception. *)

  val handle: exn -> string

end

module Make
    (T : TypeChecker)
= struct

  let env =
    ref []

  let handle_phrase phrase =
    T.Print.reset();
    let x, scheme = T.run !env phrase in
    Printf.printf "\n%s : %s\n\n" x (T.Print.scheme scheme);
    flush stdout;
    env := (x, scheme) :: (List.remove_assoc x !env)

  let failure message =
    Printf.printf "%s.\n\n" message;
    flush stdout

  let handle_channel channel =
    let lexbuf = Lexing.from_channel channel in
    while true do
      try
	print_string "? ";
	flush stdout;
	handle_phrase (T.parse lexbuf)
      with
      | T.LexerError (message, start_loc, end_loc) ->
	  failure (Printf.sprintf
		     "%s at characters %d-%d" message start_loc end_loc)
      | Parsing.Parse_error ->
	  failure "Parse error"
      | Sys_error message ->
	  failure ("System error: " ^ message)
      |	other ->
	  failure (T.handle other)
    done

  let () =
    handle_channel stdin

end

