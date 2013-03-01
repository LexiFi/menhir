let algebraic =
  ref true

let () =
  Arg.parse [
    "--algebraic", Arg.Set algebraic, " Use algebraic (that is, infix) notation";
    "--reverse", Arg.Clear algebraic, " Use reverse Polish (that is, postfix) notation";
  ] (fun _ -> ()) (Printf.sprintf "Usage: %s <options>" Sys.argv.(0))

let main =
  if !algebraic then
    Algebraic.main
  else
    Reverse.main

let () =
  let stdinbuf = Lexing.from_channel stdin in
  while true do
    (* Read line by line. *)
    let linebuf = Lexing.from_string (Lexer.line stdinbuf) in
    try
      (* Run the parser on a single line of input. *)
      Printf.printf "%d\n%!" (main Lexer.token linebuf)
    with
    | Lexer.Error msg ->
	Printf.fprintf stderr "%s%!" msg
    | Algebraic.Error
    | Reverse.Error ->
	Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start linebuf)
  done
