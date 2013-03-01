module Q =
  BinomialQueue.Make (struct
    type t = int
    let compare = (-)
  end)

let rec sort l =
  Q.elements (List.fold_right Q.insert l Q.empty)

let rec random_list n =
  if n = 0 then
    []
  else
    Random.int 1000 :: random_list (n - 1)

let check l =
  let l1 = Sort.list (<=) l
  and l2 = sort l in
  List.iter2 (fun x1 x2 ->
    if x1 <> x2 then
      failwith "Failed."
  ) l1 l2

let () =
  for i = 1 to 1000 do
    Printf.printf "%d\n%!" i;
    check (random_list 1000)
  done

