include Stdlib.Array

let rev a =
  let n = length a in
  if n = 0 then [||]
  else
    let r = make n (get a 0) in
    for i = 0 to n - 1 do
      set r i (get a (n - i - 1))
    done ;
    r

let rev_of_list li =
  match li with
  | [] ->
      [||]
  | e :: li ->
      let n = 1 + List.length li in
      let r = make n e in
      List.iteri (fun i e -> set r (n - i - 2) e) li ;
      r

let rev_to_list a = fold_left (fun li e -> e :: li) [] a

let test () =
  assert (rev [|1; 2; 3; 4|] = [|4; 3; 2; 1|]) ;
  assert (rev_of_list [1; 2; 3; 4; 5] = [|5; 4; 3; 2; 1|]) ;
  assert (rev_to_list [|1; 2; 3; 4; 5|] = [5; 4; 3; 2; 1])
