(**************************************************************************)
(*  Mini, a type inference engine based on constraint solving.            *)
(*  Copyright (C) 2006. François Pottier, Yann Régis-Gianas               *)
(*  and Didier Rémy.                                                      *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation; version 2 of the License.               *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  General Public License for more details.                              *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program; if not, write to the Free Software           *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *)
(*  02110-1301 USA                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: intRank.ml 421 2006-12-22 09:27:42Z regisgia $ *)

(** This module defines integer ranks.
    
    The constraint solver assigns a nonnegative integer rank to
    every type variable that is currently in scope, that is, every
    type variable in [dtv(S)], where [S] is the current stack. The
    rank of a variable bound at the outermost level is 0; the rank
    of a variable bound at the kth [let] frame down the stack is k.
    Type variables that are not currently in scope include the
    universal quantifiers of type schemes that appear in environment
    frames, which by convention have rank [none], as well as type
    variables that are bound inside an external constraint (that is,
    a constraint that has not yet been examined by the solver),
    whose rank is irrelevant. *)
type t = int
    
  (** [compare] is the usual total ordering on integers. *)
let compare = (-)
  
(** [none] is the special value used to identify the universal
    quantifiers of a type scheme in an environment frame. Such
    variables are not part of [dtv(S)], that is, the type
    variables currently in scope, which explains why they do not
    carry a nonnegative integer rank. *)
let none = -1
  
(** [outermost] is the rank assigned to variables that are
    existentially bound at the outermost level. *)
let outermost = 0
    

