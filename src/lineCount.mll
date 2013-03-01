(* This simple function counts the number of newline characters
   in a string. *)

let newline = ('\010' | '\013' | "\013\010")

let ordinary = [^ '\010' '\013']+

rule count n = parse
| eof
    { n }
| newline
    { count (n + 1) lexbuf }
| ordinary
    { count n lexbuf }

