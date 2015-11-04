open Lexing

let print nt startpos endpos =
  Printf.printf "%s: startpos = %s/%03d,\n%s:   endpos = %s/%03d\n"
    nt
    startpos.pos_fname
    startpos.pos_cnum
    nt
    endpos.pos_fname
    endpos.pos_cnum

