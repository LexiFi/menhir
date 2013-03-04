let () =
  let stdinbuf = Lexing.from_channel stdin in
  while true do
    (* Read line by line. *)
    let linebuf = Lexing.from_string (Lexer.line stdinbuf) in
    try
      (* Run the parser on a single line of input. *)
      Printf.printf "%d\n%!" (Parser.main Lexer.token linebuf)
    with
    | Lexer.Error msg ->
	Printf.fprintf stderr "%s%!" msg
    | Parser.Error ->
	Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start linebuf)
  done
