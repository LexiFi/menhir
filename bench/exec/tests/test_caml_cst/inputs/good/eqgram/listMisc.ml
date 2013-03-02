let rec filter p xs =
  match xs with
  | [] ->
      []
  | hd :: tl ->
      let tl' = filter p tl in
      if p hd then
	if tl == tl' then xs else hd :: tl'
      else
	tl'

