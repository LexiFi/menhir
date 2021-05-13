(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

include Array

let empty =
  [||]

let pop a =
  if a = [||] then raise (Invalid_argument "Array.pop")
  else sub a 0 (length a - 1)

let push a x =
  let n = length a in
  init (n + 1) (fun i -> if i = n then x else a.(i))

let truncate k a =
  let n = length a in
  if n <= k then
    a
  else
    sub a (n-k) k

let rev a =
  let n = length a in
  if n = 0 then
    a
  else
    let r = make n a.(0) in
    for i = 0 to n - 2 do
      r.(i) <- a.(n - i - 1)
    done;
    r

let rev_of_list xs =
  match xs with
  | [] ->
      [||]
  | x :: xs ->
      let n = 1 + List.length xs in
      let r = make n x in
      List.iteri (fun i x -> r.(n - i - 2) <- x) xs ;
      r

let rev_to_list a =
  fold_left (fun xs x -> x :: xs) [] a

(* To keep compatibility with OCaml 4.02, we copy [Array.for_all],
   which appeared in 4.03. *)

let for_all p a =
  let n = length a in
  let rec loop i =
    if i = n then true
    else if p (unsafe_get a i) then loop (succ i)
    else false in
  loop 0

(* Similarly, we copy [Array.for_all2], which appeared in 4.11. *)

let for_all2 p l1 l2 =
  let n1 = length l1
  and n2 = length l2 in
  if n1 <> n2 then invalid_arg "Array.for_all2"
  else let rec loop i =
    if i = n1 then true
    else if p (unsafe_get l1 i) (unsafe_get l2 i) then loop (succ i)
    else false in
  loop 0

let fold_left2 f accu a1 a2 =
  let n1 = length a1
  and n2 = length a2 in
  if n1 <> n2 then invalid_arg "Array.fold_left2";
  let accu = ref accu in
  for i = 0 to n1 - 1 do
    accu := f !accu (unsafe_get a1 i) (unsafe_get a2 i)
  done;
  !accu

let test () =
  assert (pop [|1; 2; 3; 4|] = [|1; 2; 3|]) ;
  assert (push [|1; 2; 3|] 4 = [|1; 2; 3; 4|]) ;
  assert (truncate 2 [|1; 2; 3; 4|] = [|3; 4|]) ;
  assert (truncate 4 [|1; 2|] = [|1; 2|]) ;
  assert (rev [|1; 2; 3; 4|] = [|4; 3; 2; 1|]) ;
  assert (rev_of_list [1; 2; 3; 4; 5] = [|5; 4; 3; 2; 1|]) ;
  assert (rev_to_list [|1; 2; 3; 4; 5|] = [5; 4; 3; 2; 1])
