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

open Grammar

module Run () = struct

(* -------------------------------------------------------------------------- *)

(* Let us write A -> alpha when there exists a production A -> alpha, and let
   us write beta => gamma when the sentential form beta expands (in one step)
   to gamma. *)

(* According to Aho and Ullman ("The Theory of Parsing, Translation, and
   Compiling -- Volume I: Parsing", page 150), a grammar is cycle-free if
   there is no derivation of the form A =>+ A. In other words, there is a
   cycle when a nonterminal symbol A expands, in one more steps, to itself. *)

(* Under the assumption that every nonterminal symbol is reachable and
   generates a nonempty language, the presence of a cycle implies that the
   grammar is infinitely ambiguous: for some inputs, there is an infinite
   number of parse trees. *)

(* We reject such a grammar, on two grounds: first, it seems pathological, and
   is likely the result of a mistake; second, the algorithm that we use to
   speed up closure computations (in the module Item) does not tolerate the
   presence of certain cycles. *)

(* Let us define a relation R as follows: A R B holds if and only if there is
   a production A -> alpha B beta where alpha and beta are nullable. Then, it
   is not difficult to see that the relations =>+ and R+ coincide. That is, to
   check that a grammar is cycle-free, it suffices to check that the relation
   R is acyclic. *)

(* Here is the relation R: *)

let successors (yield : Nonterminal.t -> unit) (nt : Nonterminal.t) : unit =
  Production.iternt nt begin fun prod ->
    let nullable_prefix = ref true in
    Production.rhs prod |> Array.iteri begin fun i symbol ->
      match symbol with
      | Symbol.T _   ->
          nullable_prefix := false
      | Symbol.N nt' ->
          let nullable_suffix, _ = Analysis.nullable_first_prod prod (i + 1) in
          if !nullable_prefix && nullable_suffix then
            yield nt';
          nullable_prefix := !nullable_prefix && Analysis.nullable nt'
    end
  end

(* A detailed explanation of cycles whose length is greater than one. *)

let show_cycle nts nt =
  assert (List.hd nts = nt);
  if List.length nts = 1 then "" else begin
    let nts = Array.of_list (nts @ [nt]) in
    let i = ref 0 in
    let next () = Nonterminal.print false nts.(Misc.postincrement i)
    and finished () = !i = Array.length nts in
    Misc.with_buffer 1024 begin fun b ->
      let out format = Printf.bprintf b format in
      out "%s" (next());
      while not (finished()) do
        out " expands to %s" (next());
        if finished() then out ".\n" else out ",\nwhich"
      done
    end
  end

(* To detect a cycle in a relation, we use the combinator [defensive_fix] that
   is provided by the library Fix. We define a function of type [Nonterminal.t
   -> unit] that computes nothing but calls itself recursively according to the
   pattern defined by the function [successors] above. Then, we evaluate this
   function everywhere. If there is a cycle, it is detected and reported. *)

(* The claim that "a cyclic grammar is ambiguous" implicitly assumes that
   every nonterminal symbol is reachable and inhabited. *)

let () =
  let module M = Fix.Memoize.ForType(Nonterminal) in
  let check = M.defensive_fix successors in
  try
    Nonterminal.iter check
  with M.Cycle (nts, nt) ->
    let positions = List.flatten (List.map Nonterminal.positions nts) in
    Error.error positions
      "the grammar is cyclic:\n\
       the nonterminal symbol %s expands to itself.\n%s\
       A cyclic grammar is ambiguous."
      (Nonterminal.print false nt)
      (show_cycle nts nt)

(* -------------------------------------------------------------------------- *)

end (* Run *)
