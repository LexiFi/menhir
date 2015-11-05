{

  open Lexing
  open Parser
  open Positions

  (* This wrapper saves the current lexeme start, invokes its argument,
     and restores it. This allows transmitting better positions to the
     parser. *)

  let savestart lexbuf f =
    let startp = lexbuf.lex_start_p in
    let token = f lexbuf in
    lexbuf.lex_start_p <- startp;
    token

  (* Updates the line counter, which is used in some error messages. *)

  let update_loc lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

  (* Extracts a chunk out of the source file. *)

  let chunk ofs1 ofs2 =
    let contents = Error.get_file_contents() in
    let len = ofs2 - ofs1 in
    String.sub contents ofs1 len

  (* Overwrites an old character with a new one at a specified
     offset in a [bytes] buffer. *)

  let overwrite content offset c1 c2 =
    assert (Bytes.get content offset = c1);
    Bytes.set content offset c2

  (* Check that only allowed indices are used in semantic actions. *)
  let check_producers_indices allowed_producers pkeywords =
    List.iter (fun pkeyword ->
      match Positions.value pkeyword with
      | Keyword.PPosition (Keyword.PRightDollar 0, Keyword.WhereEnd, _) ->
          (* As a special case, [$endpos($0)] is allowed. *)
          ()
      | Keyword.PDollar idx
      | Keyword.PPosition (Keyword.PRightDollar idx, _, _) ->
  	  if not (0 <= idx - 1 && idx - 1 < Array.length allowed_producers) then
	    Error.error [ Positions.position pkeyword ]
	      "$%d refers to a nonexistent symbol." idx
	  else
            allowed_producers.(idx - 1) |> Option.iter (fun x ->
	      Error.error [ Positions.position pkeyword ]
	        "please do not say: $%d. Instead, say: %s." idx x
            )
      | _ ->
         ()
    ) pkeywords

  (* In-place transformation of keywords. We turn our keywords into
     valid OCaml identifiers by replacing '$', '(', and ')' with '_'.
     Bloody. *)

  let transform_keywords ofs1 (pkeywords : Keyword.parsed_keyword located list) (content : bytes) =
    List.iter (function { value = keyword; position = pos } ->
      let pos = start_of_position pos in
      let ofs = pos.pos_cnum - ofs1 in
      overwrite content ofs '$' '_';
      match keyword with
      | Keyword.PDollar _
      | Keyword.PPosition (Keyword.PLeft, _, _) ->
	  ()
      | Keyword.PSyntaxError ->
	  (* $syntaxerror is replaced with
	     (raise _eRR) *)
          let source = "(raise _eRR)" in
          Bytes.blit_string source 0 content ofs (String.length source)
      | Keyword.PPosition (subject, where, _) ->
	  let ofslpar =
	    match where with
	    | Keyword.WhereStart ->
		ofs + 9
	    | Keyword.WhereEnd ->
		ofs + 7
	  in
	  overwrite content ofslpar '(' '_';
	  match subject with
	  | Keyword.PLeft ->
	      assert false
	  | Keyword.PRightDollar i ->
	      overwrite content (ofslpar + 1) '$' '_';
	      overwrite content (ofslpar + 2 + String.length (string_of_int i)) ')' '_'
	  | Keyword.PRightNamed id ->
	      overwrite content (ofslpar + 1 + String.length id) ')' '_'
    ) pkeywords

  (* In an OCaml header, there should be no keywords. This is just a sanity check. *)

  let no_keywords pkeywords =
    match pkeywords with
    | [] ->
        ()
    | { value = _; position = pos } :: _ ->
        Error.error [pos] "a Menhir keyword cannot be used in an OCaml header."

  (* Creates a stretch. *)

  let mk_stretch pos1 pos2 parenthesize pkeywords = 
    (* Read the specified chunk of the file. *)
    let ofs1 = pos1.pos_cnum
    and ofs2 = pos2.pos_cnum in
    let raw_content : string = chunk ofs1 ofs2 in
    (* Transform the keywords, if there are any. (This explicit test
       allows saving one string copy and keeping just one live copy.) *)
    let content : string =
      match pkeywords with
      | [] ->
          raw_content
      | _ :: _ ->
        let content : bytes = Bytes.of_string raw_content in
        transform_keywords ofs1 pkeywords content;
        Bytes.unsafe_to_string content
    in
    (* Add whitespace so that the column numbers match those of the source file.
       If requested, add parentheses so that the semantic action can be inserted
       into other code without ambiguity. *)
    let content =
      if parenthesize then
	  (String.make (pos1.pos_cnum - pos1.pos_bol - 1) ' ') ^ "(" ^ content ^ ")"
      else
	(String.make (pos1.pos_cnum - pos1.pos_bol) ' ') ^ content
    in
    (* After parsing, every occurrence [$i] is replaced by [_i] in
       semantic actions. *)
    let rewritten_pkeywords = Keyword.(
      let rewrite_index i =
	"_" ^ string_of_int i
      in
      let rewrite_subject = function
	| PLeft -> Left
        | PRightDollar 0 -> Before
	| PRightDollar i -> RightNamed (rewrite_index i)
	| PRightNamed n -> RightNamed n
      in
      Misc.map_opt (fun pk ->
	let position = Positions.position pk in
	match Positions.value pk with
        | PDollar _ -> None
	| PPosition (s, w, f) -> Some (Positions.with_pos position (Position (rewrite_subject s, w, f)))
	| PSyntaxError -> Some (Positions.with_pos position SyntaxError)
      ) pkeywords
    ) in
    {
      Stretch.stretch_filename = Error.get_filename();
      Stretch.stretch_linenum = pos1.pos_lnum;
      Stretch.stretch_linecount = pos2.pos_lnum - pos1.pos_lnum;
      Stretch.stretch_content = content;
      Stretch.stretch_raw_content = raw_content;
      Stretch.stretch_keywords = rewritten_pkeywords
    } 

  (* Translates the family of position-related keywords to abstract
     syntax. *)

  let mk_keyword lexbuf w f n id =
    let where =
      match w with
      | Some _ ->
	  Keyword.WhereStart
      | None ->
	  Keyword.WhereEnd
    and flavor =
      match f with
      | Some _ ->
	  Keyword.FlavorPosition
      | None ->
	  Keyword.FlavorOffset
    and subject =
      match n, id with
      | Some n, None ->
	  Keyword.PRightDollar (int_of_string n)
      | None, Some id ->
	  Keyword.PRightNamed id
      | None, None ->
	  Keyword.PLeft
      | Some _, Some _ ->
          assert false
    in
    let keyword = Keyword.PPosition (subject, where, flavor) in
    with_cpos lexbuf keyword

  (* Objective Caml's reserved words. *)

  let reserved =
    let table = Hashtbl.create 149 in
    List.iter (fun word -> Hashtbl.add table word ()) [
      "and";
      "as";
      "assert";
      "begin";
      "class";
      "constraint";
      "do";
      "done";
      "downto";
      "else";
      "end";
      "exception";
      "external";
      "false";
      "for";
      "fun";
      "function";
      "functor";
      "if";
      "in";
      "include";
      "inherit";
      "initializer";
      "lazy";
      "let";
      "match";
      "method";
      "module";
      "mutable";
      "new";
      "object";
      "of";
      "open";
      "or";
      "parser";
      "private";
      "rec";
      "sig";
      "struct";
      "then";
      "to";
      "true";
      "try";
      "type";
      "val";
      "virtual";
      "when";
      "while";
      "with";
      "mod";
      "land";
      "lor";
      "lxor";
      "lsl";
      "lsr";
      "asr";
    ];
    table

  (* Short-hands. *)

  let error1 pos =
    Error.error (Positions.one pos)

  let error2 lexbuf =
    Error.error (Positions.two lexbuf.lex_start_p lexbuf.lex_curr_p)

}

let newline = ('\010' | '\013' | "\013\010")

let whitespace = [ ' ' '\t' ';' ]

let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']

let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']

let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9'] (* '\'' forbidden *)

let poskeyword = 
  '$'
  (("start" as w) | "end")
  (("pos" as f) | "ofs")
  ( '(' ( '$' (['0'-'9']+ as n) | ((lowercase identchar*) as id)) ')')?

let previouserror =
  "$previouserror"

let syntaxerror =
  "$syntaxerror"

rule main = parse
| "%token"
    { TOKEN }
| "%type"
    { TYPE }
| "%left"
    { LEFT }
| "%right"
    { RIGHT }
| "%nonassoc"
    { NONASSOC }
| "%start"
    { START }
| "%prec"
    { PREC }
| "%public"
    { PUBLIC }
| "%parameter"
    { PARAMETER }
| "%inline"
    { INLINE }
| "%on_error_reduce"
    { ON_ERROR_REDUCE }
| "%%"
    { (* The token [PERCENTPERCENT] carries a stretch that contains
         everything that follows %% in the input file. This string
         must be created lazily. The parser decides (based on the
         context) whether this stretch is needed. If it is indeed
         needed, then constructing this stretch drives the lexer
         to the end of the file. *)
      PERCENTPERCENT (lazy (
        let openingpos = lexeme_end_p lexbuf in
        let closingpos = finish lexbuf in
        mk_stretch openingpos closingpos false []
      )) }
| ":"
    { COLON }
| ","
    { COMMA }
| "="
    { EQUAL }
| "("
    { LPAREN }
| ")"
    { RPAREN }
| "|"
    { BAR }
| "?"
    { QUESTION }
| "*"
    { STAR }
| "+"
    { PLUS }
| (lowercase identchar *) as id
    { if Hashtbl.mem reserved id then
        error2 lexbuf "this is an Objective Caml reserved word."
      else
	LID (with_pos (cpos lexbuf) id)
    }
| (uppercase identchar *) as id
    { UID (with_pos (cpos lexbuf) id) }
| "//" [^ '\010' '\013']* newline (* skip C++ style comment *)
| newline
    { update_loc lexbuf; main lexbuf }
| whitespace+
    { main lexbuf }
| "/*"
    { comment (lexeme_start_p lexbuf) lexbuf; main lexbuf }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf; main lexbuf }
| "<"
    { savestart lexbuf (ocamltype (lexeme_end_p lexbuf)) }
| "%{"
    { savestart lexbuf (fun lexbuf ->
        let openingpos = lexeme_end_p lexbuf in
        let closingpos, pkeywords = action true openingpos [] lexbuf in
        no_keywords pkeywords;
        HEADER (mk_stretch openingpos closingpos false [])
      ) }
| "{"
    { savestart lexbuf (fun lexbuf ->
        let openingpos = lexeme_end_p lexbuf in
        let closingpos, pkeywords = action false openingpos [] lexbuf in
        ACTION (
	  fun allowed_producers_indices ->
	  let stretch = mk_stretch openingpos closingpos true pkeywords in
	  check_producers_indices allowed_producers_indices pkeywords;
	  Action.from_stretch stretch
	)
      ) }
| eof
    { EOF }
| _
    { error2 lexbuf "unexpected character(s)." }

(* Skip C style comments. *)

and comment openingpos = parse
| newline
    { update_loc lexbuf; comment openingpos lexbuf }
| "*/"
    { () }
| eof
    { error1 openingpos "unterminated comment." }
| _
    { comment openingpos lexbuf }

(* Collect an O'Caml type delimited by angle brackets. Angle brackets can
   appear as part of O'Caml function types and variant types, so we must
   recognize them and *not* treat them as a closing bracket. *)

and ocamltype openingpos = parse
| "->"
| "[>"
    { ocamltype openingpos lexbuf }
| '>'
    { OCAMLTYPE (Stretch.Declared (mk_stretch openingpos (lexeme_start_p lexbuf) true [])) }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf; ocamltype openingpos lexbuf }
| newline
    { update_loc lexbuf; ocamltype openingpos lexbuf }
| eof
    { error1 openingpos "unterminated Objective Caml type." }
| _
    { ocamltype openingpos lexbuf }

(* Collect O'Caml code delimited by curly brackets. Any occurrences of
   the special ``$i'' identifiers are recorded in the accumulating
   parameter [pkeywords]. Nested curly brackets must be properly
   counted. Nested parentheses are also kept track of, so as to better
   report errors when they are not balanced. *)

and action percent openingpos pkeywords = parse
| '{'
    { let _, pkeywords = action false (lexeme_end_p lexbuf) pkeywords lexbuf in
      action percent openingpos pkeywords lexbuf }
| ("}" | "%}") as delimiter
    { match percent, delimiter with
      | true, "%}"
      | false, "}" ->
	  (* This is the delimiter we were instructed to look for. *)
	  lexeme_start_p lexbuf, pkeywords
      | _, _ ->
	  (* This is not it. *)
	  error1 openingpos "unbalanced opening brace."
    }
| '('
    { let _, pkeywords = parentheses (lexeme_end_p lexbuf) pkeywords lexbuf in
      action percent openingpos pkeywords lexbuf }
| '$' (['0'-'9']+ as n)
    { let pkeyword = with_cpos lexbuf (Keyword.PDollar (int_of_string n)) in
      action percent openingpos (pkeyword :: pkeywords) lexbuf }
| poskeyword
    { let pkeyword = mk_keyword lexbuf w f n id in
      action percent openingpos (pkeyword :: pkeywords) lexbuf }
| previouserror
    { error2 lexbuf "$previouserror is no longer supported." }
| syntaxerror
    { let pkeyword = with_cpos lexbuf Keyword.PSyntaxError in
      action percent openingpos (pkeyword :: pkeywords) lexbuf }
| '"'
    { string (lexeme_start_p lexbuf) lexbuf;
      action percent openingpos pkeywords lexbuf }
| "'"
    { char lexbuf;
      action percent openingpos pkeywords lexbuf }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf;
      action percent openingpos pkeywords lexbuf }
| newline
    { update_loc lexbuf;
      action percent openingpos pkeywords lexbuf }
| ')'
| eof
    { error1 openingpos "unbalanced opening brace." }
| _
    { action percent openingpos pkeywords lexbuf }

and parentheses openingpos pkeywords = parse
| '('
    { let _, pkeywords = parentheses (lexeme_end_p lexbuf) pkeywords lexbuf in
      parentheses openingpos pkeywords lexbuf }
| ')'
    { lexeme_start_p lexbuf, pkeywords }
| '{'
    { let _, pkeywords = action false (lexeme_end_p lexbuf) pkeywords lexbuf in
      parentheses openingpos pkeywords lexbuf }
| '$' (['0'-'9']+ as n)
    { let pkeyword = with_cpos lexbuf (Keyword.PDollar (int_of_string n)) in
      parentheses openingpos (pkeyword :: pkeywords) lexbuf }
| poskeyword
    { let pkeyword = mk_keyword lexbuf w f n id in
      parentheses openingpos (pkeyword :: pkeywords) lexbuf }
| previouserror
    { error2 lexbuf "$previouserror is no longer supported." }
| syntaxerror
    { let pkeyword = with_cpos lexbuf Keyword.PSyntaxError in
      parentheses openingpos (pkeyword :: pkeywords) lexbuf }
| '"'
    { string (lexeme_start_p lexbuf) lexbuf; parentheses openingpos pkeywords lexbuf }
| "'"
    { char lexbuf; parentheses openingpos pkeywords lexbuf }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf; parentheses openingpos pkeywords lexbuf }
| newline
    { update_loc lexbuf; parentheses openingpos pkeywords lexbuf }
| '}'
| eof
    { error1 openingpos "unbalanced opening parenthesis." }
| _
    { parentheses openingpos pkeywords lexbuf }

(* Skip O'Caml comments. Comments can be nested and can contain
   strings or characters, which must be correctly analyzed. (A string
   could contain begin-of-comment or end-of-comment sequences, which
   must be ignored; a character could contain a begin-of-string
   sequence.) *)

and ocamlcomment openingpos = parse
| "*)"
    { () }
| "(*"
    { ocamlcomment (lexeme_start_p lexbuf) lexbuf; ocamlcomment openingpos lexbuf }
| '"'
    { string (lexeme_start_p lexbuf) lexbuf; ocamlcomment openingpos lexbuf }
| "'"
    { char lexbuf; ocamlcomment openingpos lexbuf }
| newline
    { update_loc lexbuf; ocamlcomment openingpos lexbuf }
| eof
    { error1 openingpos "unterminated Objective Caml comment." }
| _
    { ocamlcomment openingpos lexbuf }

(* Skip O'Caml strings. *)

and string openingpos = parse
| '"' 
   { () }
| '\\' newline
| newline
   { update_loc lexbuf; string openingpos lexbuf }
| '\\' _
   (* Upon finding a backslash, skip the character that follows,
      unless it is a newline. Pretty crude, but should work. *)
   { string openingpos lexbuf }
| eof 
   { error1 openingpos "unterminated Objective Caml string." }
| _
   { string openingpos lexbuf }

(* Skip O'Caml characters. A lone quote character is legal inside
   a comment, so if we don't recognize the matching closing quote,
   we simply abandon. *)

and char = parse
| '\\'? newline "'"
   { update_loc lexbuf }
| [^ '\\' '\''] "'"
| '\\' _ "'"
| '\\' ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
| '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
| ""
   { () } 

(* Read until the end of the file. This is used after finding a %%
   that marks the end of the grammar specification. We update the
   current position as we go. This allows us to build a stretch
   for the trailer. *)

and finish = parse
| newline
    { update_loc lexbuf; finish lexbuf }
| eof
    { lexeme_start_p lexbuf }
| _
    { finish lexbuf }
