include Map.Make(String)

exception StrictAdd of string

let strict_add x d m =
  if mem x m then
    raise (StrictAdd x)
  else
    add x d m

let union m1 m2 =
  fold add m2 m1

