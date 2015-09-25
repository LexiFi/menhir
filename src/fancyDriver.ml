(* The module [Driver] serves to offer a unified API to the parser,
   which could be produced by either ocamlyacc or Menhir. *)

(* This is the Menhir-specific driver. We wish to handle syntax errors
   in a more ambitious manner, so as to help our end users understand
   their mistakes. *)

(* TEMPORARY a lot of code is copied from [Engine]. Can we avoid it? *)

open MenhirLib.General        (* streams: Nil, Cons *)
open Parser.MenhirInterpreter (* incremental API to our parser *)

(* The loop which drives the parser. At each iteration, we analyze a
   result produced by the parser, and act in an appropriate manner.
   We have to do this in order to get ahold of the current state when
   a syntax error is encountered. *)

let rec loop lexer lexbuf (result : 'a result) : 'a =
  let open Lexing in
  match result with
  | InputNeeded _ ->
      (* The parser needs a token. Request one from the lexer,
         and offer it to the parser, which will produce a new
         result. Then, repeat. *)
      let token = lexer lexbuf in
      let result = offer result (token, lexbuf.lex_start_p, lexbuf.lex_curr_p) in
      loop lexer lexbuf result
  | Shifting _
  | AboutToReduce _ ->
      let result = resume result in
      loop lexer lexbuf result
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
            (* We are missing a conversion [lr1state -> int]. TEMPORARY *)
            Obj.magic (s : _ lr1state)
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
  | Accepted v ->
      v
  | Rejected ->
      (* The parser rejects this input. This cannot happen because
         we stop as soon as the parser reports [HandlingError]. *)
      assert false

(* The entry point. *)

let grammar lexer lexbuf =
  loop lexer lexbuf (Parser.Incremental.grammar())

