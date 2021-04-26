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

(* This is the lattice of the natural numbers, completed with [Infinity], and
   ordered towards zero (i.e. [Infinity] is [bottom], [Finite 0] is [top]). *)

type t = Finite of int | Infinity

val bottom : t

val equal : t -> t -> bool

val is_maximal : t -> bool

val zero : t

val one : t

val min : t -> t -> t

val max : t -> t -> t

val add : t -> t -> t

val sub : t -> int -> t

val min_lazy : t -> (unit -> t) -> t

val max_lazy : t -> (unit -> t) -> t

val add_lazy : t -> (unit -> t) -> t

val print : t -> string

val to_int : t -> int

val compare : t -> t -> int

val ( < ) : t -> t -> bool

val ( <= ) : t -> t -> bool

val ( > ) : t -> t -> bool

val ( >= ) : t -> t -> bool
