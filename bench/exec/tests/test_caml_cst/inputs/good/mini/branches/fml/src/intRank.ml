(* $Id$ *)

open Sig

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
    

