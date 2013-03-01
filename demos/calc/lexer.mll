{
  open Parser

  exception Error of string

}

rule line = parse
| ([^'\n']* '\n') as line
    { line }
| eof
    { exit 0 }

and token = parse
| [' ' '\t']
    { token lexbuf }
| '\n'
    { EOL }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { TIMES }
| '/'
    { DIV }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| eof
    { exit 0 }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

