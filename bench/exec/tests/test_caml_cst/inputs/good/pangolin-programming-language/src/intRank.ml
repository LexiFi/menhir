(* $Id: intRank.ml 47 2007-10-01 14:39:45Z yann.regisgianas $ *)

(* Pangolin, a functional programming language for correct programs.
   Copyright (C) 2007, Yann Régis-Gianas, François Pottier.
   Contact: yann.regisgianas@gmail.com

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

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
  
(** [gen k] is the special value used to identify the universal
    quantifiers of a type scheme in an environment frame. Such
    variables are not part of [dtv(S)], that is, the type
    variables currently in scope, which explains why they do not
    carry a nonnegative integer rank. *)
let generalize_rank k = 
  assert (k >= 0);
  -k

let as_generalized_rank k =
  if k < 0 then
    Some (-k)
  else 
    None
  
(** [outermost] is the rank assigned to variables that are
    existentially bound at the outermost level. *)
let outermost = 0
    
let is_generalized_rank k = k < 0


