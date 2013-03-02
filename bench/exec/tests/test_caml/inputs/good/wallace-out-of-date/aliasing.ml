(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/aliasing.ml,v 1.2.6.1 1998/08/31 17:21:18 fpottier Exp $ *)

open Interpreter

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The purpose of this code is to entirely eliminate PAlias patterns from the code, by rewriting them. We work with
patterns as input. The rewriting code generates only internal (VFun/VMatcher) constructs.

Rewriting PAlias patterns is important in constructs that can potentially filter out values, i.e. in functions
(because they can be multi-line) as opposed to let constructs. In a let construct, there is always only one line
so the PAlias pattern can be left in and will be typed in a classical way. To illustrate this distinction, here
are two examples: in
  function (A _) as x -> x | _ -> bottom
it is important to give a fine type to x, so we shall rewrite the PAlias pattern as explained below. On the other
hand, in
  let (A _) as x = e in ...
the type of e is necessarily of the form [ A : ... ], so we don't need to be smart about typing x.

*)
(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Handling PAlias patterns in a fine way isn't easy. Consider the following code:
  match x with
    (A, A) as y -> y
  | _ -> (A, A)
We want this code to have type [ A ] * [ A ]. This means that we cannot lift the definition of y out of the match
construct, like this:
  let y = x in
  match x with
    (A, A) -> y
  | _ -> (A, A)
because the type would then be wider (it would be the union of the desired type and of x's type).

In general terms, here is our solution. When we want to typecheck
  match x with
    P as y -> y
  | ...
we need a filter_P function that filters its argument according to pattern P. Given a few primitives, we can
generate these filter_P functions. For instance, we have
  filter_(P1, P2) = function (x1, x2) -> (filter_P1 x1, filter_P2 x2)
and so on. Of course, generating code in this way means that there is a performance loss (we rebuild a new, identical
value instead of directly re-using the existing value). We don't really care about that, since our main point of
interest is typechecking (and we can choose to execute the original code rather than the rewritten code).

The code below generates filter_P, given a pattern P. As an optimization, it returns None when the filter is trivial
(filter_P is trivial iff it is equivalent to the identity iff P doesn't contain any constructor selector). If the
filter is trivial, no call to it shall be generated. This allows us, for instance, to generate no code for
filter_(x,y) rather than generating a call to function (x1, x2) -> ((function x -> x) x1, (function x -> x) x2)

*)

let rec create_pattern_filter = function

  (* Being successfully filtered by a wildcard or variable gives no information. Same goes for constants. *)

    PWildcard
  | PVar _
  | PConstant _ ->
      None

  (* We have the equation filter_(P1, P2) = fun (x1, x2) -> (filter_P1 x1, filter_P2 x2) *)

  | PPair(pat1, pat2) -> (

      let filter1 = create_pattern_filter pat1
      and filter2 = create_pattern_filter pat2 in
      match filter1, filter2 with
	None, None ->
	  None
      |	_ ->
	  Some (VFun (
	    PPair(PVar "x1", PVar "x2"),
            VPair(apply_filter filter1 (VVar "x1"), apply_filter filter2 (VVar "x2"))
	  ))
    )

  (* For records, we use the equation
       filter_{ l_i = P_i } = fun x -> x + { l_i = filter_P_i x.l_i }
     where + is the record update primitive operation. *)

  | PRecord lplist -> (

      (* Create the expression "x + { l_i = filter_P_i x.l_i }", while optimizing away trivial filters. *)

      let updated_record = List.fold_right (fun (label, pattern) updated_record ->
	match create_pattern_filter pattern with
	  None ->
	    updated_record
	| Some filter ->
	    VApp (VApp(VRecordUpdate label, updated_record), VApp(filter, VRecordAccess(VVar "x", label)))
      ) lplist (VVar "x") in

      (* If all filters were trivial, this filter is trivial too. Otherwise, wrap the above expression. *)

      match updated_record with
	VVar _ ->
	  None
      |	_ ->
	  Some (VFun (PVar "x", updated_record))

    )

  (* This is the case where the output filter is always non-trivial. We use the equation
       filter_{A P} = function
                        A y -> A (filter_P y)
                      | _ -> bottom
  *)

  | PConstruct (label, pattern) ->

      let arg = apply_filter (create_pattern_filter pattern) (VVar "y") in
      let yes_code = VFun(PVar "y", VConstruct (label, arg)) in
      let no_code = VFun(PWildcard, VBottom) in
      Some (VApp(VApp(VMatcher label, yes_code), no_code))

  (* An alias pattern filters nothing out, so it adds nothing to the filter generated below it. *)

  | PAlias (pattern, _) ->
      create_pattern_filter pattern

  (* Filtering with a POr pattern is equivalent to filtering with each of its argument and taking the union of 
     the results. Hence the equation
       filter_(P1 | P2) = fun x -> if true then filter_P1 x else filter_P2 x
     As far as optimization is concerned, if filter_P1 or filter_P2 is trivial, then so is filter_(P1 | P2). This
     should rarely happen though, since no sane programmer would use an irrefutable pattern inside a POr pattern. *)
  
  | POr (pat1, pat2) -> (

      match create_pattern_filter pat1, create_pattern_filter pat2 with
	Some filter1, Some filter2 ->
	  Some (VFun (
		  PVar "x",
		  VIf (VConstant (ConstBool true), VApp (filter1, VVar "x"), VApp (filter2, VVar "x"))
               ))
      |	_ ->
	  None

    )

  | PRef pattern -> (

      match create_pattern_filter pattern with
	None ->
	  None
      |	Some filter ->
	  Some (VFun (
		  PRef (PVar "x"),
		  VApp(VVar "ref", VApp(filter, VVar "x"))
               ))

    )

and apply_filter filter expr =
  match filter with
    None ->
      expr
  | Some filter ->
      VApp(filter, expr)
;;

