(* This module defines sets of strings, maps over strings,
   and auxiliary functions over these data structures. *)

module StringSet = Set.Make(String)

module StringMap = Map.Make(String)

module StringAux = struct

  let map f set =
    StringSet.fold (fun x set ->
      StringSet.add (f x) set
    ) set StringSet.empty

  let set_to_map set default =
    StringSet.fold (fun x map ->
      StringMap.add x default map
    ) set StringMap.empty

  let domain map =
    StringMap.fold (fun k _ s ->
      StringSet.add k s
    ) map StringSet.empty

  let print set =
    let _, accu = StringSet.fold (fun x (first, accu) ->
      false, accu ^ (if not first then ", " else "") ^ x
    ) set (true, "") in
    accu

  let modify map key change =
    let previous = StringMap.find key !map in
    let updated = change previous in
    map := StringMap.add key updated !map

  let merge map key set action =
    let previous = StringMap.find key !map in
    let updated = StringSet.union previous set in
    if not (StringSet.equal previous updated) then begin
      map := StringMap.add key updated !map;
      action key
    end

  let new_list_manager channel empty opening separator closing =
    let void = ref true in
    let element () =
      (* The user signals that he is about to display a new
	 list element. *)
      output_string channel (if !void then opening else separator);
      void := false
    and finished () =
      (* The user signals that he is finished. *)
      output_string channel (if !void then empty else closing)
    in
    element, finished

  let show_list empty opening separator closing = function
    | [] ->
	empty
    | x :: xs ->
	opening ^
	x ^
	List.fold_left (fun accu x ->
	  accu ^
	  separator ^
	  x
	) "" xs ^
	closing

end

