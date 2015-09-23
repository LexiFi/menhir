(* This lexer is used to cut an input into segments, delimited
   by blank lines. It produces a list of segments, where each
   segment is a pair of positions. *)

{

  open Lexing

}

let newline    = ('\010' | '\013' | "\013\010")

let whitespace = [ ' ' '\t' ';' ]

let comment    = '#' [^'\010''\013']* newline

(* In the idle state, we skip whitespace, newlines and comments
   (while updating the liner counter). If we reach the end of file,
   we return the list of all segments found so far. If we reach a
   non-blank non-comment character, we record its position and
   switch to the busy state. *)

rule idle segments = parse
| whitespace
    { idle segments lexbuf }
| newline
    { new_line lexbuf; idle segments lexbuf }
| comment
    { new_line lexbuf; idle segments lexbuf }
| eof
    { List.rev segments }
| _
    { let opening = lexbuf.lex_start_p in
      busy segments opening false lexbuf }

(* In the busy state, we skip everything, maintaining one bit
   [just_saw_a_newline], until [just_saw_a_newline] is true
   and we find a second newline. This marks the end of a
   segment, and we revert back to the idle state. If we
   reach the end of file, we consider that this is also
   the end of a segment. *)

and busy segments opening just_saw_a_newline = parse
| whitespace
    { busy segments opening just_saw_a_newline lexbuf }
| newline
    { new_line lexbuf;
      (* The newline that we just saw is already included in the segment.
         This one is not included. *)
      let closing = lexbuf.lex_start_p in
      if just_saw_a_newline then
        let segment = (opening, closing) in
        let segments = segment :: segments in
        idle segments lexbuf
      else
        busy segments opening true lexbuf }
| comment
    { new_line lexbuf; busy segments opening true lexbuf }
| eof
    { let closing = lexbuf.lex_start_p in
      let segment = (opening, closing) in
      let segments = segment :: segments in
      List.rev segments }
| _
    { busy segments opening false lexbuf }

