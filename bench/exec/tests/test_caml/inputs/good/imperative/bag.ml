module Make (X : sig

  (* Items. *)

  type item

  (* Each item must carry a [mark] field. *)

  val get_mark: item -> bool
  val set_mark: item -> bool -> unit

end) = struct

  open X

  let queue : item Queue.t =
    Queue.create()

  let add x =
    if not (get_mark x) then begin
      Queue.add x queue;
      set_mark x true
    end

  let choose () =
    try
      let x = Queue.take queue in
      assert (get_mark x);
      set_mark x false;
      Some x
    with Queue.Empty ->
      None

  let mem x =
    get_mark x

  let rec repeat f =
    match choose() with
    | None ->
	()
    | Some x ->
	f x;
	repeat f

end

