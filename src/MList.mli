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

(** This module is an extension of Stdlib.List *)

include module type of List

(** A list subject to a condition. (Be careful, though: the list is of course
    constructed even if the condition is false.) *)
val ifn : bool -> 'a list -> 'a list

(** A list subject to a condition. (Be careful, though: the list is of course
    constructed even if the condition is false.) *)
val if1 : bool -> 'a -> 'a list

(** A lazy version of [ifn], where the list is constructed only if the condition
    is true. *)
val ifnlazy : bool -> (unit -> 'a list) -> 'a list

(** The sum of a list of integers. *)
val sum : int list -> int

(** Group equivalent elements of a list.
    [group_by ~compare ~group xs] sorts the list [xs] using [compare] and then
    groups runs of equivalent elements using [group] *)
val group_by :
  compare:('a -> 'a -> int) -> group:('a -> 'a list -> 'b) ->
  'a list -> 'b list

(** [find_map f xs] applies [f] to elements of [xs] in order and the returns
    the first result of the form [Some y], or [None]. *)
val find_map : ('a -> 'b option) -> 'a list -> 'b option

(** [partition_map f xs] classifies elements of list [xs] in a left and a right
    lists according to the result of [f] *)
val partition_map :
  ('a -> [< `L of 'l | `R of 'r ]) -> 'a list -> 'l list * 'r list

(** [compare f l1 l2] compares two list according to the lexicographic
    ordering. Elements are compared using the [f] argument. *)
val compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int
