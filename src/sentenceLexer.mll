(* This lexer is used to read the sentences provided on the standard input
   channel when [--interpret] is enabled. *)

{

  open Lexing
  open SentenceParser
  open Grammar

  (* Updates the line counter, which is used in some error messages. *)

  let update_loc lexbuf n =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + n;
      pos_bol = pos.pos_cnum;
    }

  (* A short-hand. *)

  let error1 lexbuf msg =
    Error.error (Positions.one (lexeme_start_p lexbuf)) msg

}

let newline   = ('\010' | '\013' | "\013\010")

let whitespace = [ ' ' '\t' ';' ]

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']

let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']

let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9'] (* '\'' forbidden *)

let comment = '#' [^'\010''\013']* newline

let skip = newline whitespace* newline

rule lex = parse
  | (lowercase identchar *) as lid
      { try
	  let nt = Nonterminal.lookup lid in
	  if StringSet.mem lid Front.grammar.UnparameterizedSyntax.start_symbols then
	    NONTERMINAL nt
	  else
	    error1 lexbuf (Printf.sprintf "\"%s\" is not a start symbol." lid)
	with Not_found ->
	  error1 lexbuf (Printf.sprintf "\"%s\" is not a known non-terminal symbol." lid)
      }
  (* An identifier that begins with an uppercase letter is considered a
     terminal symbol. *)
  | (uppercase identchar *) as uid
      { try
	  TERMINAL (Terminal.lookup uid)
	with Not_found ->
	  error1 lexbuf (Printf.sprintf "\"%s\" is not a known terminal symbol." uid)
      }
  (* Whitespace is ignored. *)
  | whitespace
      { lexbuf.lex_start_p <- lexbuf.lex_curr_p; lex lexbuf }
  (* The end of a line is translated to [EOL]. *)
  | newline
      { update_loc lexbuf 1; EOL }
  (* A comment is ignored. *)
  | comment
      { update_loc lexbuf 1; lexbuf.lex_start_p <- lexbuf.lex_curr_p; lex lexbuf }
  (* The end of file is translated to [EOF]. *)
  | eof
      { EOF }
  (* A colon. *)
  | ':'
      { COLON }
  | _
      { error1 lexbuf "unexpected character." }

(* Note that [block] cannot fail. *)

and block buffer = parse
  (* A blank line (or the end of file) signals the end of this block of text. *)
  | (newline as n) whitespace* newline
      { update_loc lexbuf 2; Buffer.add_string buffer n; Buffer.contents buffer }
  | eof
      { Buffer.contents buffer }
  (* Anything else is considered part of the text.
     (There is no syntax for comments here.) *)
  | newline as n
      { update_loc lexbuf 1; Buffer.add_string buffer n; block buffer lexbuf }
  | _ as c
      { Buffer.add_char buffer c; block buffer lexbuf }

(* Note that [skip] cannot fail. *)

and skip = parse
  (* Whitespace, newlines, comments are ignored. *)
  | whitespace
      { lexbuf.lex_start_p <- lexbuf.lex_curr_p; skip lexbuf }
  | newline
  | comment
      { update_loc lexbuf 1; lexbuf.lex_start_p <- lexbuf.lex_curr_p; skip lexbuf }
  (* Anything else causes us to stop. *)
  | _
  | eof
      { () }
