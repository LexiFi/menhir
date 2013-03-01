let iterator xs =
  let state =
    ref xs
  in
  let next () =
    match !state with
    | x :: xs ->
	state := xs;
	x
    | [] ->
	assert false
  in
  next

let memoize f =
  let table = Hashtbl.create 1023 in
  function x ->
    try
      Hashtbl.find table x
    with Not_found ->
      let y = f x in
      Hashtbl.add table x y;
      y

(* [qfold f accu q] repeatedly takes an element [x] off the queue [q]
   and applies [f] to the accumulator and to [x], until [q] becomes
   empty. Of course, [f] can add elements to [q] as a side-effect.

   We allocate an option to ensure that [qfold] is tail-recursive. *)

let rec qfold f accu q =
  match
    try
      Some (Queue.take q)
    with Queue.Empty ->
      None
  with
  | Some x ->
      qfold f (f accu x) q
  | None ->
      accu

