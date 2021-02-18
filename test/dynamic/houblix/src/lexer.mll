{ (* -*- tuareg -*- *)
  open Error
  open ParserTokens
  open LexerHelper


(*
 

  let get_char_quoted s =
    let s' = String.sub s 1 (String.length (s - 2))


  let char_of_string_atom atom =
   ' '*)
}

let digit = ['0' - '9']
let hexdigit = digit | ['a' - 'f'] | ['A' - 'F']
let lowercase_letter = ['a' - 'z']
let uppercase_letter = ['A' - 'Z']
let letter = lowercase_letter | uppercase_letter
let newline = ('\010' | '\013' | "\013\010")
let blank   = [' ' '\009' '\012']
let atom_code = ('\\' digit (digit ?)  (digit ?)) | ("\\0x" hexdigit (hexdigit ?))
let underscore = "_"
let printable = [' ' - '~']
let char_printable = [' ' - '&' '(' - '~']
let string_printable = [' ' - '!' '#' - '~']
let atom = char_printable | atom_code | "\\t" | "\\r" | "\\b" | "\\n" | "\\'" | "\\\\"
let string_atom = string_printable | atom_code | "\\t" | "\\r" | "\\b" | "\\n" | "\\\"" | "\\\\"
let char = "'" atom "'"
let lpar = "("
let rpar = ")"
let backslash = "\\"
let ampersand = "&"
let double_ampersand = "&&"
let pipepipe = "||"
let equal_question = "=?" 
let lbrack_equal_question = "<=?"
let rbrack_equal_question = ">=?"
let lbrack_question = "<?"
let rbrack_question = ">?"
let open_com = "/*"
let close_com = "*/"
let dot = "."
let exclamation = "!"
let pipe = "|"
let colon = ":"
let semi_colon = ";"
let equal = "="
let plus = "+"
let minus = "-"
let star = "*"
let slash = "/"
let langle = "<"
let rangle = ">"
let arrow = "->"
let comma = ","
let lbrack = "["
let rbrack = "]"
let lcbrack = "{"
let rcbrack = "}"
let colon_equal = ":="
let lowercase_id = lowercase_letter ((letter | digit | '_')*)
let uppercase_id = uppercase_letter ((letter | digit | '_')*)

let number = '-'? ((digit+) | ("0x" (hexdigit+)) | ("0b" (('0' | '1')+)) | ("0o" (['0' - '7']+))) 

rule comment depth = parse
  | open_com  { comment (depth + 1) lexbuf        }
  | close_com { if depth = 0 then 
                  ( token lexbuf )
                else 
                  ( comment (depth  - 1) lexbuf ) }
  | _         { comment depth lexbuf              }

and string accumulator = parse
 | "\""        { STRING(
                  String.of_seq 
                    (List.to_seq 
                      (List.map (char_of_atom lexbuf)
                        (List.rev accumulator))))                                    }
 | string_atom { string ((Lexing.lexeme lexbuf) :: accumulator) lexbuf               }
 | eof         { error "during lexing" (Position.cpos lexbuf) "Unterminated string." }

and token = parse
  (** Layout *)
  | newline               { next_line_and token lexbuf }
  | blank+                { token lexbuf               }
  | open_com              { comment 0 lexbuf           }
  (* char *)
  | "'" atom "'"          { CHAR (char_of_atom lexbuf (unqote(Lexing.lexeme lexbuf)))  }
  | "\""                  { string [] lexbuf                                           }
  | number                { try
                              INT(Int64.of_int (int_of_string (Lexing.lexeme lexbuf)))  
                            with
                              Failure  _ (*"int_of_string"*) -> 
                                global_error "during parsing" "Syntax error."          }
  (* atomic lexemes *)
  | "let"                 { LET                 }
  | "type"                { TYPE                }
  | "fun"                 { FUN                 }
  | "for"                 { FOR                 }
  | "in"                  { IN                  }
  | "ref"                 { REF                 }
  | "if"                  { IF                  }
  | "else"                { ELSE                }
  | "while"               { WHILE               }
  | "extern"              { EXTERN              }
  | "and"                 { AND                 }
  | "switch"              { SWITCH              }
  | "do"                  { DO                  }
  | "to"                  { TO                  }
  | lcbrack               { LCBRACK             }
  | rcbrack               { RCBRACK             }
  | backslash             { BACKSLASH           }
  | semi_colon            { SEMICOLON           }
  | comma                 { COMMA               }
  | arrow                 { ARROW               }
  | lpar                  { LPAR                }
  | rpar                  { RPAR                }
  | lbrack                { LBRACK              } 
  | rbrack                { RBRACK              } 
  | pipepipe              { PIPEPIPE            }
  | equal_question        { EQUALQUESTION       }
  | lbrack_equal_question { LANGLEEQUALQUESTION }
  | rbrack_equal_question { RANGLEEQUALQUESTION }
  | lbrack_question       { LANGLEQUESTION      }
  | rbrack_question       { RANGLEQUESTION      }
  | dot                   { DOT                 }
  | exclamation           { EXCLAMATION         }
  | pipe                  { PIPE                }
  | colon                 { COLON               }
  | equal                 { EQUAL               }
  | plus                  { PLUS                }
  | minus                 { MINUS               }
  | star                  { STAR                }
  | slash                 { SLASH               }
  | langle                { LANGLE              }
  | rangle                { RANGLE              }
  | double_ampersand      { DOUBLEAMPERSAND     }
  | ampersand             { AMPERSAND           }
  | colon_equal           { COLONEQUAL          }
  | eof                   { EOF                 }
  (* identifiers *)
  | lowercase_id          { LOWERCASE_ID(Lexing.lexeme lexbuf)                       }
  | uppercase_id          { UPPERCASE_ID(Lexing.lexeme lexbuf)                       }
  | "`" lowercase_id      { TYPE_VARIABLE(Lexing.lexeme lexbuf)                      }
  | underscore            { UNDERSCORE                                               }
  (** Lexing error. *)
  | _               { error "during lexing" (Position.cpos lexbuf) "unexpected character." }

