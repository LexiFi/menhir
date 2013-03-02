(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/entailment.ml,v 1.7.6.5 1999/02/18 00:06:27 francois Exp $ *)
(*

Here is the simplified (GLUBless) entailment algorithm.

*)

open Types

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The history is a set of pairs of variables.

*)

let compare_goals (vl1, vl2) (vr1, vr2) =
  let result = compare_variables vl1 vr1 in
  if result = 0 then compare_variables vl2 vr2
  else result
;;

let empty_history =
  Set7.empty compare_goals
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The main function. Remember that this function must work on canonical sets, because it does not handle GLUBs
at all. This makes it much simpler, and, hopefully, faster.

The history only stores goals of the form variable < variable. This is enough to prevent looping, since any
looping run must go through propagation steps, and the result of a propagation step is a variable < variable
constraint. The advantages of this decision are that the code is simpler, the history is smaller and accessed
less often, the history set's comparison function is simpler. The drawback is that the algorithm might run
longer before detecting loops (at most, one step deeper).

The history set must be global, not local (i.e. it must not decrease when coming back from recursive calls).
This makes the algorithm quadratic instead of exponential (cf. my PhD thesis). The speed difference is not
noticeable on small examples (the quadratic version might even be slower, maybe because it uses a reference
cell), but it is huge on certain large examples (the quadratic version takes a few seconds, while the exponential
one does not terminate in practice).

*)

let entailment v1 v2 =

  let history = ref empty_history in

  let rec prove_constraint v1 v2 =

    (* Check whether this goal has been seen before. If so, we're done. *)

    let goal = (v1, v2) in
    if not (Set7.mem goal !history) then begin

    (* Check whether these variables are related using reflexivity and transitivity. We take advantage of the
       fact that the constraint graph is closed. *)

      if (v1 != v2) & (not (Set7.mem v1 v2.loset)) then begin

      (* If they are not, replace them with their canonical constructed bounds. *)

      	history := Set7.add goal !history;
      	subconstraints (fun leaf1 leaf2 ->
	  prove_constraint (leaf_to_variable leaf1) (leaf_to_variable leaf2)
        ) v1.hi v2.lo

      end
    end

  in prove_constraint v1 v2
;;
