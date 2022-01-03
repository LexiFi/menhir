rule read =
  parse
  | "FOO" { Parser.FOO }
  | "BAR" { Parser.BAR }
  | _     { failwith ("Unexpected char: " ^ Lexing.lexeme lexbuf) }
  | eof   { Parser.EOL }
