(* The module [Driver] serves to offer a unified API to the parser,
   which could be produced by either ocamlyacc or Menhir. *)

(* This is the Menhir-specific driver. We wish to handle syntax errors
   in a more ambitious manner, so as to help our end users understand
   their mistakes. *)

open MenhirLib.General        (* streams: Nil, Cons *)
open Parser.MenhirInterpreter (* incremental API to our parser *)

(* [fail] is invoked if a syntax error is encountered. *)

let fail lexbuf checkpoint =
  match checkpoint with
  | HandlingError env ->
      (* The parser has suspended itself because of a syntax error. Stop.
         Find out which state the parser is currently in. *)
      let stack = stack env in
      let s : int =
        match Lazy.force stack with
        | Nil ->
            (* Hmm... The parser is in its initial state. Its number is
               usually 0. This is a BIG HACK. TEMPORARY *)
            0
        | Cons (Element (s, _, _, _), _) ->
            number s
      in
      (* Display a nice error message. In principle, the table found in
         [ParserMessages] should be complete, so we should obtain
         a nice message. If [Not_found] is raised, we produce a generic
         message, which is better than nothing. Note that the OCaml code
         in [ParserMessages] is auto-generated based on the table in
         [ParserMessages.messages]. *)
      let message =
        try
          ParserMessages.message s
        with Not_found ->
          Printf.sprintf "Unknown syntax error (in state %d).\n" s
      in
      (* Hack: remove the final newline, because [Error.error] adds one. *)
      let message = String.sub message 0 (String.length message - 1) in
      (* Display our message and die. *)
      Error.error (Positions.lexbuf lexbuf) message
  | _ ->
      (* This cannot happen. *)
      assert false

(* The entry point. *)

let grammar lexer lexbuf =
  loop_handle
    (fun v -> v)
    (fail lexbuf)
    (lexer_lexbuf_to_supplier lexer lexbuf)
    (Parser.Incremental.grammar())

