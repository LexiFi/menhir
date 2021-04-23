%{
  (**/**)
  open Wktxt_type
  open Wktxt_parsing_functions

%}

%token<string> HTML_BLOCK
%token<int> HEADER
%token<Wktxt_type.order * int> LIST
%token<Wktxt_type.def_type * int> DEFLIST
%token<string> STRING NOWIKI
%token<char> CHAR
%token<string> LINK
%token ITALIC BOLD BOLDITALIC
%token EOF HRULE EMPTYLINE
%token TABLE_START TABLE_END TABLE_TITLE TABLE_NEW_LINE
%token<Wktxt_type.table_cell> TABLE_CELL

%start document
%type <Wktxt_type.document> document

%%

document:
  | b = block* EOF { List.flatten b }
;

block:
  | EMPTYLINE* b = block { b }
  | h = HEADER i = inline(regular)+ HEADER EMPTYLINE* {
      [ Header (h, (List.flatten i)) ]
    }
  | TABLE_START title = preceded(TABLE_TITLE, inline(regular)+)? l1 = pair(TABLE_CELL, inline(regular)*)* l2 = table_line* TABLE_END {
      let lines =
        match l1 with
        | [] -> l2
        | _ -> (get_table_line l1) :: l2
      in
      match title with
      | None -> [ Table ([], lines) ]
      | Some title -> [ Table (List.flatten title, lines) ]
    }
  | l = pair(LIST, inline(regular)+)+ EMPTYLINE* {
      match l with
      | [] -> []
      | ((list_type, _), _) :: _ -> parse_list 0 l list_type
    }
  | l = pair(DEFLIST, inline(regular)+)+ EMPTYLINE* {
      [DefList (get_def_blocks l 1)]
    }
  | s = HTML_BLOCK EMPTYLINE* {
      [ NoWikiBlock s ]
    }
  | HRULE EMPTYLINE* {
      [ Hrule ]
    }
  | i = inline(regular)+ EMPTYLINE* {
     match List.flatten i with
     | [ NoWiki s ] -> [ NoWikiBlock s ]
     | x ->
        if List.for_all (function String s -> String.trim s = "" | _ -> false) x
        then []
        else [ Paragraph x ]
    }
;

table_line:
  | TABLE_NEW_LINE l = pair(TABLE_CELL, inline(regular)*)* {
      get_table_line l
    }

(* inlines *)

(* Closing tag is optionnal in order to handle bad input.
   Menhir resolve this conflict by shifting by default so this is OK
*)
regular:
  | ITALIC i = inline(noformat)+ ITALIC? { [Italic (List.flatten i)] }
  | BOLD i = inline(noformat)+ BOLD? { [Bold (List.flatten i)] }
  | BOLDITALIC i = inline(noformat)+ BOLDITALIC? {
      [Bold [ Italic (List.flatten i) ]]
    }
  | BOLDITALIC i1 = inline(noformat)+ ITALIC i2 = inline(noformat)+ BOLD? {
      [Bold (Italic (List.flatten i1) :: (List.flatten i2))]
    }
  | BOLDITALIC i1 = inline(noformat)+ BOLD i2 = inline(noformat)+ ITALIC? {
      [Italic (Bold (List.flatten i1) :: (List.flatten i2))]
    }
  | ITALIC i1 = inline(noformat)+ BOLD i2 = inline(noformat)+ BOLDITALIC? {
      [Italic ( (List.flatten i1) @ [Bold (List.flatten i2)] )]
    }
  | BOLD i1 = inline(noformat)+ ITALIC i2 = inline(noformat)+ BOLDITALIC? {
      [Bold ( (List.flatten i1) @ [Italic (List.flatten i2)] )]
    }
  | BOLD i1 = inline(noformat)+ ITALIC i2 = inline(noformat)+ ITALIC i3 = inline(noformat)+ BOLD? {
      [Bold (List.flatten i1 @ [Italic (List.flatten i2)] @ List.flatten i3)]
    }
  | ITALIC i1 = inline(noformat)+ BOLD i2 = inline(noformat)+ BOLD i3 = inline(noformat)+ ITALIC? {
      [Italic (List.flatten i1 @ [Bold (List.flatten i2)] @ List.flatten i3)]
    }
  | BOLD i1 = inline(noformat)+ BOLDITALIC i2 = inline(noformat)+ ITALIC? {
      [Bold (List.flatten i1) ; Italic (List.flatten i2)]
    }
  | ITALIC i1 = inline(noformat)+ BOLDITALIC i2 = inline(noformat)+ BOLD? {
      [Italic (List.flatten i1) ; Bold (List.flatten i2)]
    }
;

noformat:
  | error { failwith "error" }
;

inline(param):
  | s = STRING { [String s] }
  | c = CHAR { [String (String.make 1 c)] }
  | s = NOWIKI { [NoWiki s] }
  | x = LINK { [Link x] }
  | p = param { p }
;

%%
