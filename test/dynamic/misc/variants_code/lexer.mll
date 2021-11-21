rule read =
  parse
  | "T" { Parser.T }
  | _   { failwith ("Unexpected char: " ^ Lexing.lexeme lexbuf) }
  | eof { Parser.EOF }
