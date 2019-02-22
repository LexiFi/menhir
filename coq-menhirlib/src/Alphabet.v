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

From Coq Require Import Omega List Syntax Relations RelationClasses.

Local Obligation Tactic := intros.

(** A comparable type is equiped with a [compare] function, that define an order
   relation. **)
Class Comparable (A:Type) := {
  compare : A -> A -> comparison;
  compare_antisym : forall x y, CompOpp (compare x y) = compare y x;
  compare_trans :  forall x y z c,
    (compare x y) = c -> (compare y z) = c -> (compare x z) = c
}.

Theorem compare_refl {A:Type} (C: Comparable A) :
  forall x, compare x x = Eq.
Proof.
intros.
pose proof (compare_antisym x x).
destruct (compare x x); intuition; try discriminate.
Qed.

(** The corresponding order is a strict order. **)
Definition comparableLt {A:Type} (C: Comparable A) : relation A :=
  fun x y => compare x y = Lt.

Instance ComparableLtStrictOrder {A:Type} (C: Comparable A) :
  StrictOrder (comparableLt C).
Proof.
apply Build_StrictOrder.
unfold Irreflexive, Reflexive, complement, comparableLt.
intros.
pose proof H.
rewrite <- compare_antisym in H.
rewrite H0 in H.
discriminate H.
unfold Transitive, comparableLt.
intros x y z.
apply compare_trans.
Qed.

(** nat is comparable. **)
Program Instance natComparable : Comparable nat :=
  { compare := Nat.compare }.
Next Obligation.
symmetry.
destruct (Nat.compare x y)  as [] eqn:?.
rewrite Nat.compare_eq_iff in Heqc.
destruct Heqc.
rewrite Nat.compare_eq_iff.
trivial.
rewrite <- nat_compare_lt in *.
rewrite <- nat_compare_gt in *.
trivial.
rewrite <- nat_compare_lt in *.
rewrite <- nat_compare_gt in *.
trivial.
Qed.
Next Obligation.
destruct c.
rewrite Nat.compare_eq_iff in *; destruct H; assumption.
rewrite <- nat_compare_lt in *.
apply (Nat.lt_trans _ _ _ H H0).
rewrite <- nat_compare_gt in *.
apply (gt_trans _ _ _ H H0).
Qed.

(** A pair of comparable is comparable. **)
Program Instance PairComparable {A:Type} (CA:Comparable A) {B:Type} (CB:Comparable B) :
  Comparable (A*B) :=
  { compare := fun x y =>
      let (xa, xb) := x in let (ya, yb) := y in
      match compare xa ya return comparison with
        | Eq => compare xb yb
        | x => x
      end }.
Next Obligation.
destruct x, y.
rewrite <- (compare_antisym a a0).
rewrite <- (compare_antisym b b0).
destruct (compare a a0); intuition.
Qed.
Next Obligation.
destruct x, y, z.
destruct (compare a a0) as [] eqn:?, (compare a0 a1) as [] eqn:?;
try (rewrite <- H0 in H; discriminate);
try (destruct (compare a a1) as [] eqn:?;
  try (rewrite <- compare_antisym in Heqc0;
         rewrite CompOpp_iff in Heqc0;
         rewrite (compare_trans _ _ _  _ Heqc0 Heqc2) in Heqc1;
         discriminate);
  try (rewrite <- compare_antisym in Heqc1;
         rewrite CompOpp_iff in Heqc1;
         rewrite (compare_trans _ _ _ _ Heqc2 Heqc1) in Heqc0;
         discriminate);
  assumption);
rewrite (compare_trans _ _ _ _ Heqc0 Heqc1);
try assumption.
apply (compare_trans _ _ _ _ H H0).
Qed.

(** Special case of comparable, where equality is usual equality. **)
Class ComparableUsualEq {A:Type} (C: Comparable A) :=
  compare_eq : forall x y, compare x y = Eq -> x = y.

(** Boolean equality for a [Comparable]. **)
Definition compare_eqb {A:Type} {C:Comparable A} (x y:A) :=
  match compare x y with
    | Eq => true
    | _ => false
  end.

Theorem compare_eqb_iff {A:Type} {C:Comparable A} {U:ComparableUsualEq C} :
  forall x y, compare_eqb x y = true <-> x = y.
Proof.
unfold compare_eqb.
intuition.
apply compare_eq.
destruct (compare x y); intuition; discriminate.
destruct H.
rewrite compare_refl; intuition.
Qed.

Instance NComparableUsualEq : ComparableUsualEq natComparable := Nat.compare_eq.

(** A pair of ComparableUsualEq is ComparableUsualEq **)
Instance PairComparableUsualEq
  {A:Type} {CA:Comparable A} (UA:ComparableUsualEq CA)
  {B:Type} {CB:Comparable B} (UB:ComparableUsualEq CB) :
    ComparableUsualEq (PairComparable CA CB).
Proof.
intros x y; destruct x, y; simpl.
pose proof (compare_eq a a0); pose proof (compare_eq b b0).
destruct (compare a a0); try discriminate.
intuition.
destruct H2, H0.
reflexivity.
Qed.

(** An [Finite] type is a type with the list of all elements. **)
Class Finite (A:Type) := {
  all_list : list A;
  all_list_forall : forall x:A, In x all_list
}.

(** An alphabet is both [ComparableUsualEq] and [Finite]. **)
Class Alphabet (A:Type) := {
  AlphabetComparable :> Comparable A;
  AlphabetComparableUsualEq :> ComparableUsualEq AlphabetComparable;
  AlphabetFinite :> Finite A
}.

(** The [Numbered] class provides a conveniant way to build [Alphabet] instances,
   with a good computationnal complexity. It is mainly a injection from it to
   [positive] **)
Class Numbered (A:Type) := {
  inj : A -> positive;
  surj : positive -> A;
  surj_inj_compat : forall x, surj (inj x) = x;
  inj_bound : positive;
  inj_bound_spec : forall x, (inj x < Pos.succ inj_bound)%positive
}.

Instance NumberedAlphabet {A:Type} (N:Numbered A) : Alphabet A :=
  { AlphabetComparable := {| compare := fun x y => Pos.compare (inj x) (inj y) |};
    AlphabetFinite :=
      {| all_list := fst (Pos.iter
                           (fun '(l, p) => (surj p::l, Pos.succ p))
                           ([], 1%positive) inj_bound) |} }.
Proof.
  - intros ??. now rewrite <- Pos.compare_antisym.
  - intros x y z [].
    + rewrite !Pos.compare_eq_iff. congruence.
    + rewrite !Pos.compare_lt_iff. apply Pos.lt_trans.
    + rewrite !Pos.compare_gt_iff. eauto using Pos.lt_trans.
  - intros x y. unfold compare. rewrite Pos.compare_eq_iff. intros Hxy.
    rewrite <-(surj_inj_compat x), <-(surj_inj_compat y). congruence.
  - intros x. rewrite <-(surj_inj_compat x).
    generalize (inj_bound_spec x). generalize (inj x). clear x. intros x.
    match goal with |- ?Hx -> In ?s (fst ?p) =>
      assert ((Hx -> In s (fst p)) /\ snd p = Pos.succ inj_bound); [|now intuition] end.
    rewrite Pos.lt_succ_r.
    induction inj_bound as [|y [IH1 IH2]] using Pos.peano_ind;
      (split; [intros Hx|]); simpl.
    + rewrite (Pos.le_antisym _ _ Hx); auto using Pos.le_1_l.
    + auto.
    + rewrite Pos.iter_succ. destruct Pos.iter; simpl in *. subst.
      rewrite Pos.le_lteq in Hx. destruct Hx as [?%Pos.lt_succ_r| ->]; now auto.
    + rewrite Pos.iter_succ. destruct Pos.iter. simpl in IH2. subst. reflexivity.
Qed.

(** Previous class instances for [option A] **)
Program Instance OptionComparable {A:Type} (C:Comparable A) : Comparable (option A) :=
  { compare := fun x y =>
      match x, y return comparison with
        | None, None => Eq
        | None, Some _ => Lt
        | Some _, None => Gt
        | Some x, Some y => compare x y
      end }.
Next Obligation. destruct x, y; intuition. apply compare_antisym. Qed.
Next Obligation.
  destruct x, y, z; try now intuition;
    try (rewrite <- H in H0; discriminate).
  apply (compare_trans _ _ _ _ H H0).
Qed.

Instance OptionComparableUsualEq {A:Type} {C:Comparable A} (U:ComparableUsualEq C) :
  ComparableUsualEq (OptionComparable C).
Proof.
  intros [] []; simpl; try reflexivity; try discriminate.
  intros ->%compare_eq. reflexivity.
Qed.

Program Instance OptionFinite {A:Type} (E:Finite A) : Finite (option A) :=
  { all_list := None :: map Some all_list }.
Next Obligation.
destruct x; intuition.
right.
apply in_map.
apply all_list_forall.
Defined.

(** Definitions of [FSet]/[FMap] from [Comparable] **)
Require Import OrderedTypeAlt.
Require FSetAVL.
Require FMapAVL.
Import OrderedType.

Module Type ComparableM.
  Parameter t : Type.
  Declare Instance tComparable : Comparable t.
End ComparableM.

Module OrderedTypeAlt_from_ComparableM (C:ComparableM) <: OrderedTypeAlt.
  Definition t := C.t.
  Definition compare : t -> t -> comparison := compare.

  Infix "?=" := compare (at level 70, no associativity).

  Lemma compare_sym x y : (y?=x) = CompOpp (x?=y).
  Proof. exact (Logic.eq_sym (compare_antisym x y)). Qed.
  Lemma compare_trans c x y z :
    (x?=y) = c -> (y?=z) = c -> (x?=z) = c.
  Proof.
  apply compare_trans.
  Qed.
End OrderedTypeAlt_from_ComparableM.

Module OrderedType_from_ComparableM (C:ComparableM) <: OrderedType.
  Module Alt := OrderedTypeAlt_from_ComparableM C.
  Include (OrderedType_from_Alt Alt).
End OrderedType_from_ComparableM.
