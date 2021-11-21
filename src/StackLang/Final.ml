module N =
  Grammar.Nonterminal

let sub final1 final2 =
  Option.sub N.equal final1 final2

let lub final1 final2 =
  match final1, final2 with
  | None, final
  | final, None ->
      Some final
  | Some nt1, Some nt2 ->
      if N.equal nt1 nt2 then Some final1 else None
