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

let test () =
  assert (pop [|1; 2; 3; 4|] = [|1; 2; 3|]) ;
  assert (push [|1; 2; 3|] 4 = [|1; 2; 3; 4|]) ;
  assert (truncate 2 [|1; 2; 3; 4|] = [|3; 4|]) ;
  assert (truncate 4 [|1; 2|] = [|1; 2|]) ;
  assert (rev [|1; 2; 3; 4|] = [|4; 3; 2; 1|]) ;
  assert (rev_of_list [1; 2; 3; 4; 5] = [|5; 4; 3; 2; 1|]) ;
  assert (rev_to_list [|1; 2; 3; 4; 5|] = [5; 4; 3; 2; 1])
