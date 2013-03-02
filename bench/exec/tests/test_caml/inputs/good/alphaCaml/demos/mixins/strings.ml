(* This module defines sets and maps over strings. *)

module StringSet = Set.Make(String)

module StringMap = struct

  include Map.Make(String)

  let singleton k v =
    add k v empty

  let foldv f accu map =
    fold (fun _ x accu ->
      f accu x
    ) map accu

  let rec foldv2 f accu m1 m2 =
    let accu, m2 =
      fold (fun k d1 (accu, m2) ->
	try
	  let d2 = find k m2 in
	  let accu = f accu d1 d2 in
	  let m2 = remove k m2 in
	  accu, m2
	with Not_found ->
	  raise (Invalid_argument "StringMap.foldv2")
      ) m1 (accu, m2)
    in
    if is_empty m2 then
      accu
    else
      raise (Invalid_argument "StringMap.foldv2")

  let domain map =
    fold (fun k _ s ->
      StringSet.add k s
    ) map StringSet.empty

  let union m1 m2 =
    fold add m1 m2

  let diff map set =
    StringSet.fold remove set map

  let elements m =
    fold (fun k d elements ->
      (k, d) :: elements
    ) m []

end

