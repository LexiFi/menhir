type 'a ordering =
    'a -> 'a -> int

let lexicographic2 compare1 compare2 x1 x2 y1 y2 =
  let c = compare1 x1 y1 in
  if c <> 0 then
    c
  else
    compare2 x2 y2

let lexicographic_pair compare1 compare2 (x1, x2) (y1, y2) =
  lexicographic2 compare1 compare2 x1 x2 y1 y2

let lexicographic3 compare1 compare2 compare3 x1 x2 x3 y1 y2 y3 =
  let c = compare1 x1 y1 in
  if c <> 0 then
    c
  else
    let c = compare2 x2 y2 in
    if c <> 0 then
      c
    else
      compare3 x3 y3

let lexicographic_list compare =
  let rec loop xs ys =
    match xs, ys with
    | [], [] ->
	0
    | _ :: _, [] ->
	1
    | [], _ :: _ ->
	-1
    | x :: xs, y :: ys ->
	lexicographic2 compare loop x xs y ys
  in
  loop
