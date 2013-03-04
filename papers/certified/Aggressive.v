(* A tiny example showing how to refine a defensive program performing

   dynamic typechecking on its data into an aggressive program where

   these checks are removed under the precondition that it is safe to

   do it.



   compiled with coq version 8.3pl2. 

*)

Require Import Program.

Open Scope list_scope.



(* Our "dynamic types" with their semantics *)

Inductive type := NAT | BOOL. 



Definition sem (t:type) : Set := if t then nat else bool.



Definition eq_type_dec: forall (x y:type), {x=y} + {x<>y}.

  decide equality. 

Defined.



(* Our typed values *)

Record data: Set := mkdata { dyntype: type; value: sem dyntype }.



(* A "safe" cast function. *)

Program Definition cast {t1:type} (t2:_ | t1=t2): sem t1 -> sem t2 :=

  match proj2_sig t2 in (_ = y) return (sem t1) -> (sem y) with

  | eq_refl => fun x => x

  end.



(* The above "match" destructs a non-informative type (Coq equality). 

   Hence at extraction, "cast" is simply identity. *)

Extraction Inline cast. (* inline identity ! *)



(* Now, we define a binary operation on homogeneous data *)

Definition op {t: type} :=

  if t return (sem t) -> (sem t) -> (sem t)

  then plus

  else orb.



(* Now, a defensive version of "op" on data, raising an error on

   heterogeneous data *)

Program Definition def_op (x y:data): option (sem (dyntype x)) :=

  let tx := (dyntype x) in 

  if (eq_type_dec tx (dyntype y)) then

     Some (op (value x) (cast tx (value y)))

  else

     None. 



(* Our defensive program iterates "def_op" on a list of data.

   If the list is heterogeneous: it raises a dynamic error. *)

Fixpoint defensive (l:list data)

    : forall (acc: data), option (sem (dyntype acc)) :=

  match l with

  | [] => fun acc => Some (value acc)

  | x::l => fun acc => 

    match def_op acc x with

    | Some res => defensive l (mkdata _ res)

    | None => None

    end

  end.



(* Hence, the extracted code contains many defensive checks...*)

Extraction Inline def_op op eq_type_dec.

Extraction defensive.



(* Now, we define "aggressive" as refinement of "defensive" (see types

   below), under the precondition that defensive does not raise an

   error.



   First, we define "aggressive_rec" the recursive part of the

   program, that iterates "l" over "agg_op", which is a monomorphic 

   implementation of "op" selected by "aggressive".



  N.B: aggressive_rec code does perform tests neither on types, nor on

  errors.  It replaces "dynamic typechecking" by "(proved) safe

  cast".*)



Program Fixpoint aggressive_rec {t:type} 

  (agg_op: sem t -> sem t -> sem t | forall x y, agg_op x y = op x y) 

  (l: list data) 

  (acc:sem t | defensive l (mkdata _ acc) <> None) 

    : { res | defensive l (mkdata _ acc) = Some res } :=

  match l with

  | [] => acc

  | x::l => aggressive_rec agg_op l (agg_op acc (cast t (value x)))

  end.



(* Proof obligations of aggressive_rec. *)

Transparent def_op_obligation_1.



(* a generic script solving the OP *)

Ltac aggressive_rec_OP x t H H0 :=

  generalize H; clear H; simpl; unfold def_op; simpl ;

  case (eq_type_dec t (dyntype x)) ; [

  (* case: t=dyntype x *)

  unfold def_op_obligation_1;

  generalize (dyntype x) (value x); clear x ;

  intros tx vx e; generalize vx; clear vx ;

  case e; intros vx; (try rewrite H0); auto |

  (* absurd case *)

  intros H H1; case H1; auto ].



Obligation 2. (* safety of the cast *)

  aggressive_rec_OP x t H H0.

Defined.



Obligation 3. (* precondition of the recursive call *)

  aggressive_rec_OP x t H H0.

Qed.



Obligation 4. (* postcondition of the result *)

  aggressive_rec_OP x t H H0.

Defined.



(* The main program "aggressive" *) 

Program Definition aggressive 

  (l: list data) 

  (acc: data | defensive l acc <> None)

    : { res | defensive l acc = Some res } := 

  (if dyntype acc as t return 

      forall (acc:sem t | defensive l (mkdata _ acc) <> None), 

      { res | defensive l (mkdata _ acc) = Some res }

   then fun acc => aggressive_rec (t:=NAT) plus l acc

   else fun acc => aggressive_rec (t:=BOOL) orb l acc

  ) (value acc).



Obligation 3. (* precondition of internal if *)

  generalize H; case acc; simpl; auto.

Qed.



(* a trivial technical lemma *)

Lemma unfold_Somedefbysig A (x:option A) (p: { res | x = Some res }):

  x = Some (`p).

Proof. case p; simpl; auto. Qed.



Obligation 4. (* postcondition of internal if *)

  generalize H; case acc; simpl; clear acc H.

  intros tacc; case tacc; simpl; intros; apply unfold_Somedefbysig.

Qed.



(* See the extracted code now ! *)

Extraction "aggressive.ml" aggressive.
