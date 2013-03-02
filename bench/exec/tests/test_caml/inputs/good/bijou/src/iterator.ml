type 'a iterator =
    'a list ref

let make xs =
  ref xs

let next i =
  match !i with
  | x :: xs ->
      i := xs;
      x
  | [] ->
      assert false

let copy i =
  ref !i

