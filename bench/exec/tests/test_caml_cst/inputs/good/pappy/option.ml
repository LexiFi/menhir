type 'a t = 'a option

let iter f = function
  | Some x ->
      f x
  | None ->
      ()

let map f = function
  | Some x ->
      Some (f x)
  | None ->
      None

let fold f accu = function
  | Some x ->
      f accu x
  | None ->
      accu

