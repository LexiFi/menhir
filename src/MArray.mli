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

(** This module is an extension of Stdlib.Array *)

include module type of Array

(**[empty] is the empty array. *)
val empty : 'a array

(** [pop a] is [a] with its last element removed.
    [pop [|1; 2; 3; 4|] = [|1; 2; 3|]] *)
val pop : 'a t -> 'a t

(** [push a x] is [a] with [x] added at its end.
    [push [|1; 2; 3|] 4 = [|1; 2; 3; 4|]] *)
val push : 'a t -> 'a -> 'a t

(**If the array [a] has length at least [k], then [truncate k a] is the suffix
   of length [k] of the array [a]. Otherwise, [truncate k a] is [a]. *)
val truncate : int -> 'a array -> 'a array

(** Array reversal.
   [rev [|1; 2; 3; 4|] = [|4; 3; 2; 1|]]*)
val rev : 'a t -> 'a t

(** Convert a list to an array. The list's head becomes the end of the array.
    [rev_of_list [1; 2; 3; 4; 5] = [|5; 4; 3; 2; 1|]] *)
val rev_of_list : 'a list -> 'a t

(** Convert an array to a list. The end of the array becomes the list's head.
    [rev_to_list [|1; 2; 3; 4; 5|] = [5; 4; 3; 2; 1]] *)
val rev_to_list : 'a t -> 'a list

(** Unit tests. *)
val test : unit -> unit
