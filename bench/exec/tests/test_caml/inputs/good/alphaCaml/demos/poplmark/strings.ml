(* This module defines maps over strings. *)

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

end

