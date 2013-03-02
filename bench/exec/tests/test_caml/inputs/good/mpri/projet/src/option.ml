let map f = function
  | None ->
      None
  | Some x ->
      Some (f x)

let iter f o =
  match o with
  | None ->
      ()
  | Some x ->
      f x

let fold f o accu =
  match o with
  | None ->
      accu
  | Some x ->
      f x accu

let concat f o1 o2 =
  match o1, o2 with
  | None, o
  | o, None ->
      o
  | Some x1, Some x2 ->
      Some (f x1 x2)

