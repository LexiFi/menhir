(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open StackLang

(* This module provides basic support for code specialization in StackLang.
   It does not know with respect to what information the code is specialized,
   or how it is specialized. It keeps track of the mapping of source labels
   to target labels, of the queue of blocks waiting to be specialized, etc. *)

(* For each block labeled [label] in the source program, zero, one or more
   blocks labeled [(label, data)] are created in the specialized program.

   A specialized block is created only if it is reachable.

   The user chooses the type of [data]. The [default] data is used to
   specialize the program's entry points. *)

(* The user must supply two functions:

   - [spec_label] maps a target label (that is, a pair [(label, data)]) to a
     source label. It determines the labels used in the specialized code. The
     pair [(label, Data.default)] must be mapped to the same string as the
     label [label].

   - [spec_tblock jump (label, data) tblock] specializes the typed block
     [tblock], whose label is [label], with respect to [data]. The [jump]
     function must be used to generate [JUMP] instructions towards target
     labels. *)

module Make
  (Data : sig
     include Order.S
     val default : t
   end)
  (S : sig
     type label' = label * Data.t
     val spec_label : label' -> label
     val spec_tblock :
       (* jump: *) (label' -> block) ->
       label' -> typed_block ->
       typed_block
   end)
  (X : sig val program : program end)
     : sig val program : program end
