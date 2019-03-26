let const c = fun _ -> c

let group_assoc l =
  let cons k v acc = (k, List.rev v) :: acc in
  let rec aux k v vs acc = function
    | [] -> List.rev (cons k (v :: vs) acc)
    | (k', v') :: xs when compare k k' = 0 ->
      if compare v v' = 0 then
        aux k v vs acc xs
      else
        aux k v' (v :: vs) acc xs
    | (k', v') :: xs ->
      aux k' v' [] (cons k (v :: vs) acc) xs
  in
  match List.sort compare l with
  | [] -> []
  | (k, v) :: xs -> aux k v [] [] xs

(* negation to put nan as the max *)
let compare_float a b = - compare (-.a) (-.b)

let min_float a b =
  if compare_float a b > 0 then b else a

let arg_min_float f a b =
  if compare_float (f a) (f b) <= 0 then a else b

let rec list_last = function
  | [x] -> x
  | _ :: xs -> list_last xs
  | [] -> invalid_arg "list_last"

let pp_list f ppf = function
  | [] -> Format.fprintf ppf "[]"
  | x :: xs ->
     Format.fprintf ppf "[%a" f x;
     List.iter (Format.fprintf ppf "; %a" f) xs;
     Format.fprintf ppf "]"
