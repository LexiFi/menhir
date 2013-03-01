type 'a t =
  unit -> 'a

let create suspension =
  suspension

let rec force suspension =
  suspension()

let map f suspension =
  create (fun () ->
    f (force suspension)
  )

let fold f accu suspension =
  assert false (* for the time being *)

let fold2 f accu suspension1 suspension2 =
  assert false (* for the time being *)

