open StackLang

type t = cell_info

(** [make ?typ hold_state hold_semv hold_startpos hold_endpos] *)
let make ?typ hold_state hold_semv hold_startpos hold_endpos =
  {typ; hold_state; hold_semv; hold_startpos; hold_endpos}

let empty = make false false false false
