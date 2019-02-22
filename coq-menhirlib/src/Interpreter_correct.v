(****************************************************************************)
(*                                                                          *)
(*                                   Menhir                                 *)
(*                                                                          *)
(*           Jacques-Henri Jourdan, CNRS, LRI, Université Paris Sud         *)
(*                                                                          *)
(*  Copyright Inria. All rights reserved. This file is distributed under    *)
(*  the terms of the GNU Lesser General Public License as published by the  *)
(*  Free Software Foundation, either version 3 of the License, or (at your  *)
(*  option) any later version, as described in the file LICENSE.            *)
(*                                                                          *)
(****************************************************************************)

From Coq Require Import Streams List Syntax.
Require Import Alphabet.
Require Grammar Automaton Interpreter.
From Coq.ssr Require Import ssreflect.

Module Make(Import A:Automaton.T) (Import Inter:Interpreter.T A).

(** * Correctness of the interpreter **)

(** We prove that, in any case, if the interpreter accepts returning a
    semantic value, then this is a semantic value of the input **)

Section Init.

Variable init:initstate.

(** [word_has_stack_semantics] relates a word with a stack, stating that the
    word is a concatenation of words that have the semantic values stored in
    the stack. **)
Inductive word_has_stack_semantics:
  forall (word:list token) (stack:stack), Prop :=
  | Nil_stack_whss: word_has_stack_semantics [] []
  | Cons_stack_whss:
    forall (wordq:list token) (stackq:stack),
      word_has_stack_semantics wordq stackq ->

    forall (wordt:list token) (s:noninitstate)
           (pt:parse_tree (last_symb_of_non_init_state s) wordt),

    word_has_stack_semantics
       (wordq++wordt) (existT noninitstate_type s (pt_sem pt)::stackq).

(** [pop] preserves the invariant **)
Lemma pop_spec_ptl A symbols_to_pop action word_stk stk (res : A) stk' :
  pop_spec symbols_to_pop stk action stk' res ->
  word_has_stack_semantics word_stk stk ->
  exists word_stk' word_res (ptl:parse_tree_list symbols_to_pop word_res),
    (word_stk' ++ word_res = word_stk)%list /\
    word_has_stack_semantics word_stk' stk' /\
    ptl_sem ptl action = res.
Proof.
  intros Hspec. revert word_stk.
  induction Hspec as [stk sem|symbols_to_pop st stk action sem stk' res Hspec IH];
    intros word_stk Hword_stk.
  - exists word_stk, [], Nil_ptl. rewrite -app_nil_end. eauto.
  - inversion Hword_stk. subst_existT.
    edestruct IH as (word_stk' & word_res & ptl & ? & Hword_stk'' & ?); [eassumption|].
    subst. eexists word_stk', (word_res ++ _)%list, (Cons_ptl ptl _).
    split; [|split]=>//. rewrite app_assoc //.
Qed.

(** [reduce_step] preserves the invariant **)
Lemma reduce_step_invariant (stk:stack) (prod:production) Hv Hi word buffer :
  word_has_stack_semantics word stk ->
  match reduce_step init stk prod buffer Hv Hi with
  | Accept_sr sem buffer_new =>
    exists pt : parse_tree (NT (start_nt init)) word,
    buffer = buffer_new /\ pt_sem pt = sem
  | Progress_sr stk' buffer_new =>
    buffer = buffer_new /\ word_has_stack_semantics word stk'
  | Fail_sr => True
  end.
Proof.
  intros Hword_stk. unfold reduce_step.
  match goal with
  | |- context [pop_state_valid init ?stp stk ?x1 ?x2 ?x3 ?x4 ?x5] =>
    generalize (pop_state_valid init stp stk x1 x2 x3 x4 x5)
  end.
  destruct pop as [stk' sem] eqn:Hpop=>/= Hv'.
  apply pop_spec_ok in Hpop. apply pop_spec_ptl with (word_stk := word) in Hpop=>//.
  destruct Hpop as (word1 & word2 & ptl & <- & Hword1 & <-).
  match goal with | |- context [proj2 (Hv I) ?x1 ?x2] => generalize (proj2 (Hv I) x1 x2) end.
  destruct goto_table as [[st' EQ]|].
  - intros _. split=>//.
    change (ptl_sem ptl (prod_action prod)) with (pt_sem (Non_terminal_pt prod ptl)).
    generalize (Non_terminal_pt prod ptl). rewrite ->EQ. intros pt. by constructor.
  - intros Hstk'. destruct Hword1; [|by destruct Hstk'].
    generalize (reduce_step_subproof0 init prod [] (ptl_sem ptl (prod_action prod))
                                      (fun _ : True => Hstk')).
    simpl in Hstk'. rewrite -Hstk' // => EQ. rewrite cast_eq.
    exists (Non_terminal_pt prod ptl). by split.
Qed.

(** [step] preserves the invariant **)
Lemma step_invariant stk word (buffer:Stream token) safe Hi :
  word_has_stack_semantics word stk ->
  match step safe init stk buffer Hi with
  | Accept_sr sem buffer_new =>
    exists word_new (pt:parse_tree (NT (start_nt init)) word_new),
      word ++ buffer = word_new ++ buffer_new /\
      pt_sem pt = sem
  | Progress_sr stk_new buffer_new =>
    exists word_new,
      word ++ buffer = word_new ++ buffer_new /\
      word_has_stack_semantics word_new stk_new
  | Fail_sr => True
  end.
Proof.
  intros Hword_stk. unfold step.
  generalize (reduce_ok safe (state_of_stack init stk)).
  destruct action_table as [prod|awt].
  - intros Hv.
    apply (reduce_step_invariant stk prod (fun _ => Hv) Hi word buffer) in Hword_stk.
    destruct reduce_step=>//.
    + destruct Hword_stk as (pt & <- & <-); eauto.
    + destruct Hword_stk as [<- ?]; eauto.
  - destruct buffer as [[term sem] buffer]=>/=.
    move=> /(_ term) Hv. destruct (awt term) as [st EQ|prod|]=>//.
    + eexists _. split; [by apply app_str_app_assoc with (l2 := [_])|].
      change sem with (pt_sem (Terminal_pt term sem)) at 2.
      generalize (Terminal_pt term sem).
      unfold token.
      generalize [existT (fun t => symbol_semantic_type (T t)) term sem].
      rewrite -> EQ=>word' pt /=. by constructor.
    + apply (reduce_step_invariant stk prod (fun _ => Hv) Hi word
                 (Cons (existT _ term sem) buffer)) in Hword_stk.
      destruct reduce_step=>//.
      * destruct Hword_stk as (pt & <- & <-); eauto.
      * destruct Hword_stk as [<- ?]; eauto.
Qed.

(** The interpreter is correct : if it returns a semantic value, then the input
    word has this semantic value.
**)
Theorem parse_correct safe buffer n_steps:
  match parse safe init buffer n_steps with
  | Parsed_pr sem buffer_new =>
    exists word_new (pt:parse_tree (NT (start_nt init)) word_new),
      buffer = word_new ++ buffer_new /\
      pt_sem pt = sem
  | _ => True
  end.
Proof.
  unfold parse.
  change buffer with ([] ++ buffer) at 2. revert buffer. generalize Nil_stack_whss.
  generalize (parse_subproof init).
  generalize (@nil token). generalize (@nil (sigT noninitstate_type)).
  induction n_steps as [|n_steps IH]=>//= stk word Hi Hword_stk buffer.
  apply step_invariant with (buffer := buffer) (safe := safe) (Hi := Hi) in Hword_stk.
  generalize (step_stack_invariant_preserved safe init stk (buffer) Hi).
  destruct step as [| |stk' buffer']=>//. clear Hi. move=> /(_ _ _ eq_refl) Hi.
  destruct Hword_stk as (word' & -> & Hword'). by eapply IH.
Qed.

End Init.

End Make.
