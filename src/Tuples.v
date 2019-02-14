(* *********************************************************************)
(*                                                                     *)
(*                              Menhir                                 *)
(*                                                                     *)
(*          Jacques-Henri Jourdan, INRIA Paris-Rocquencourt            *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of     *)
(*  the License, or  (at your option) any later version.               *)
(*                                                                     *)
(* *********************************************************************)

From Coq Require Import List Syntax Equality.

(** A curryfied function with multiple parameters **)
Definition arrows_left: list Type -> Type -> Type :=
  fold_left (fun A B => B -> A).

(** A curryfied function with multiple parameters **)
Definition arrows_right: Type -> list Type -> Type :=
  fold_right (fun A B => A -> B).

(** A tuple is a heterogeneous list. For convenience, we use pairs. **)
Fixpoint tuple (types : list Type) : Type :=
  match types with
  | nil => unit
  | t::q => prod t (tuple q)
  end.

Fixpoint uncurry {args:list Type} {res:Type}:
  arrows_left args res -> tuple args -> res :=
  match args return forall res, arrows_left args res -> tuple args -> res with
    | [] => fun _ f _ => f
    | t::q => fun res f p => let (d, t) := p in
      (@uncurry q _ f t) d
  end res.

Lemma arrows_left_right_map_rev_append A B l1 l2 (f : B -> Type) :
  arrows_left (map f (rev_append l1 l2)) A =
  arrows_left (map f l2) (arrows_right A (map f l1)).
Proof.
  revert l2. induction l1 as [|C l1 IH]; intros l2; [|simpl; rewrite IH]; reflexivity.
Qed.

Lemma JMeq_eqrect:
  forall (U:Type) (a b:U) (P:U -> Type) (x:P a) (e:a=b),
    eq_rect a P x b e ~= x.
Proof.
destruct e.
reflexivity.
Qed.
