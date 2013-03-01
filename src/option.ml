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

