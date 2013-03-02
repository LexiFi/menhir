let fold_map_left op f accu xs =
  List.fold_left (fun accu x ->
    op accu (f x)
  ) accu xs

let fold_map_right op f xs accu =
  List.fold_right (fun x accu ->
    op (f x) accu
  ) xs accu

let flat_map f xs =
  fold_map_right (@) f xs []

let pairs xs =
  let rec loop accu = function
    | [] ->
	accu
    | x1 :: xs ->
	let accu = List.fold_left (fun accu x2 ->
	  (x1, x2) :: accu
        ) accu xs in
	loop accu xs
  in
  loop [] xs

