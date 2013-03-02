(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/types.ml,v 1.52.2.34 1999/04/08 12:43:46 francois Exp $ *)
(*

Basic and advanced utilities to deal with types.

*)

open Errors
open Rowsig

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Type definitions.

*)

type variable_sign =
    Neutral
  | Positive
  | Negative
  | Bipolar

and type_variable = {
    stamp: int;

    mutable sign: variable_sign;

    mutable lo: small_term;
    mutable loset: variable_set;
    mutable hiset: variable_set;
    mutable hi: small_term;

    mutable guards: guard_set;

    mutable traverse_id: int;

    span: span;
    mutable link: row option;

    mutable representative: type_variable
  }

and kind =
    KRegular
  | KVariant
  | KRecord

and span =
    Field
  | Remainder of row_set

and variable_set = type_variable Set7.t

and guard = head_constructor * type_variable * type_variable

and guard_set = guard Set7.t

and leaf_term =
    TVar of type_variable
  | TLUB of variable_set
  | TGLB of variable_set

and small_term =
    TBottom
  | TAbstract of int * (leaf_term list)
  | TTop

  | TRBottom
  | TRMissing
  | TRPresent of leaf_term
  | TRMaybe of leaf_term
  | TRTop

  | TVAbsent
  | TVPresent of leaf_term
  | TVTop

and row = {
    entries: leaf_term row_map;
    remainder: leaf_term
  }

and head_constructor =
    HBottom
  | HAbstract of int
  | HTop

  | HRBottom
  | HRMissing
  | HRPresent
  | HRMaybe
  | HRTop

  | HVAbsent
  | HVPresent
  | HVTop

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

More type definitions.

*)

type identifier = string * int
 and context = (identifier * type_variable) Set7.t
 and type_scheme = Scheme of context * type_variable * type_variable
 and constrained_type = Ctype of type_variable * type_variable

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Facilities to traverse type schemes.

To avoid looping, we must mark variables as we go. A simple boolean flag is enough, but using such a flag forces us
to first traverse the whole set to set each flag to false; to do this, we need to maintain a (doubly) linked list of
all variables. We prefer to avoid this, both because maintaining the list is a chore, and because not having a list
allows the garbage collector to throw away unused constraints automatically.

So, instead of using a boolean, the idea is to use an integer mark. We don't need to initialize the mark, provided
that the value that means "already visited" is incremented by one for each traversal. This works as long as we perform
less than 4 billion traversals in a single program run. Assuming we perform one million traversals a second (which is
very optimistic), this allows us to run without problems for an hour. It should be more than enough. If better security
is desired, time stamps can be added (they were found in previous versions of this code).

*)

module Traverse = struct

  let current_traverse_id = ref min_int

  let start () =
    incr current_traverse_id
  ;;

  let mark v =
    v.traverse_id <- !current_traverse_id
  ;;

  let is_marked v =
    (v.traverse_id == !current_traverse_id)
  ;;

end

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Comparison functions and empty sets.

To compare variables, we must compare the stamps only. Comparing the mutable fields would be incorrect - the order
must not change with time. Note that calls to compare_variables are inlined by hand in several places.

In contexts, we have (label, leaf term) pairs where labels are unique, so the comparison function only takes labels
into account. Beware though, the second element of the pair must be taken into account when comparing elements coming
from distinct sets!

*)

let compare_variables v1 v2 =
  v1.stamp - v2.stamp

let empty_variable_set =
  Set7.empty compare_variables

let compare_guards (head1, v1, w1) (head2, v2, w2) =
  let result = Pervasives.compare head1 head2 in
  if result <> 0 then result
  else let result = compare_variables v1 v2 in
  if result <> 0 then result
  else compare_variables w1 w2

let empty_guard_set =
  Set7.empty compare_guards

let compare_labeled_pairs (label1, _) (label2, _) =
  Pervasives.compare label1 label2

let compare_label_to_labeled_pair label (label', _) =
  Pervasives.compare label label'

let empty_label_set =
  Set7.empty Pervasives.compare

let empty_context =
  Set7.empty compare_labeled_pairs

let empty_row_map =
  RowMap.empty()

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Determining a term's kind.

*)

let kind_of_term = function
    TBottom
  | TTop
  | TAbstract _ ->
      KRegular
  | TRBottom
  | TRMissing
  | TRPresent _
  | TRMaybe _
  | TRTop ->
      KRecord
  | TVAbsent
  | TVPresent _
  | TVTop ->
      KVariant

let kind_of_variable v =
  kind_of_term v.lo

let kind_of_leaf = function
    TVar v ->
      kind_of_variable v
  | TLUB vset ->
      kind_of_variable (Set7.choose vset)
  | TGLB vset ->
      kind_of_variable (Set7.choose vset)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Creating fresh variables.

*)

let tautologous_bounds_kind = function
    KRegular ->
      TBottom, TTop
  | KRecord ->
      TRBottom, TRTop
  | KVariant ->
      TVAbsent, TVTop

let counter = ref 0

let freshSK span kind =
  incr counter;
  let lo, hi = tautologous_bounds_kind kind in
  let rec v = {
    stamp = !counter; sign = Neutral;
    lo = lo; loset = empty_variable_set; hiset = empty_variable_set; hi = hi; guards = empty_guard_set;
    traverse_id = min_int;
    span = span; link = None;
    representative = v
  } in
  v

let freshV v =
  freshSK v.span (kind_of_variable v)

let fresh () =
  freshSK Field KRegular

let leaf_to_variable = function
  TVar v ->
    v
| _ ->
    raise (CantHappen "Types.leaf_to_variable expects type variables only!")

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Utilities to deal with signs.

*)

let is_positive v =
  match v.sign with
    Bipolar
  | Positive ->
      true
  | Negative
  | Neutral ->
      false

let is_negative v =
  match v.sign with
    Bipolar
  | Negative ->
      true
  | Positive
  | Neutral ->
      false

let set_sign v sign =
  match (v.sign, sign) with
    Positive, false
  | Negative, true ->
      v.sign <- Bipolar
  | Neutral, true ->
      v.sign <- Positive
  | Neutral, false ->
      v.sign <- Negative
  | _ ->
      ()

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Head constructors.

*)

let head_constructor_of_term = function
  TTop ->
    HTop
| TBottom ->
    HBottom
| TAbstract (stamp, _) ->
    HAbstract stamp
| TRPresent _ ->
    HRPresent
| TVPresent _ ->
    HVPresent
| TRMaybe _ ->
    HRMaybe
| TRTop ->
    HRTop
| TRMissing ->
    HRMissing
| TVAbsent ->
    HVAbsent
| TRBottom ->
    HRBottom
| TVTop ->
    HVTop

let head_constructors_ordered head1 head2 =
  match (head1, head2) with

    (* KRegular. *)
    
    HBottom, _
  | _, HTop ->
      true
  | HAbstract stamp1, HAbstract stamp2 ->
      stamp1 = stamp2

  (* KRecord. *)

  | HRBottom, _
  | _, HRTop
  | HRMissing, HRMissing
  | HRPresent, HRPresent
  | (HRMissing | HRPresent | HRMaybe), HRMaybe ->
      true

  (* KVariant. *)

  | HVAbsent, _
  | HVPresent, HVPresent
  | _, HVTop ->
      true

  | _, _ ->
      false

(* ----------------------------------------------------------------------------------------------------------------- *)
(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Primitives to deal with destructive expansion.

*)
(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Given a row, return the set of its explicit fields. The row must be normalized.

*)

let row_signature row =
  RowMap.domain row.entries

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

These auxiliary functions add the fields carried by a variable/leaf term/small term, if it is expanded, to the
accumulator.

*)

let rec accumulate_variable v accu =
  match normalize_link v with
    None ->
      accu
  | Some row ->
      RowSet.union (row_signature row) accu

and accumulate_leaf leaf accu =
  match normalize_leaf leaf with
    TVar v ->
      accumulate_variable v accu
  | TLUB _
  | TGLB _ ->
      accu

and accumulate_small_term term accu =
  match term with
    TBottom | TTop | TRBottom | TRMissing | TRTop | TVAbsent | TVTop ->
      accu
  | TAbstract(stamp, _) when stamp = Abstract.get_stamp "{}" ->
      accu
  | TAbstract(stamp, _) when stamp = Abstract.get_stamp "[]" ->
      accu
  | TAbstract (_, args) ->
      List.fold_right accumulate_leaf args accu
  | TRPresent arg ->
      accumulate_leaf arg accu
  | TRMaybe arg ->
      accumulate_leaf arg accu
  | TVPresent arg ->
      accumulate_leaf arg accu

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Given a set of variables, make sure that all of them carry the same set of explicit fields. Return this set as
a result.

*)

and normalize_vset vset =
  Errors.trace "Types.normalize_vset" (fun () ->

    (* Gather the set of fields explicitly carried by our variables. *)

    let signature = Set7.fold accumulate_variable vset RowSet.empty in

    (* If it is empty, we're done; otherwise, force each variable to carry these fields. *)

    if not (RowSet.is_empty signature) then
      Set7.iter (expand_variable signature) vset;

    signature

  )

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Given a leaf term, normalize it so that it is either a variable (possibly linked to a clean row), or a GLUB of clean
(i.e. unlinked) variables.

*)

and normalize_leaf leaf =
  Errors.trace "Types.normalize_leaf" (fun () ->

    let auxiliary operator vset =

      (* Force all variables to carry the same fields. *)

      let signature = normalize_vset vset in
      if RowSet.is_empty signature then leaf
      else begin

	(* This auxiliary function returns the row to which a variable points. The variable must have a
	   non-empty link field. *)

	let get_row v = Standard.option (normalize_link v) in

	(* Compute the union/intersection of all the rows pointed to by the variables. There is at least one
	   variable in the set. *)

	let v, vset = Set7.isolate vset in
	let row = Set7.fold (fun v row -> operator (get_row v) row) vset (get_row v) in

	(* Create a dummy variable to stand for the newly computed row. *)

	let dummy = freshV v in
	dummy.link <- Some row;
	TVar dummy

      end in

    (* The main function's body. *)

    match leaf with
      TVar v ->
	let _ = normalize_link v in
	leaf
    | TLUB vset ->
	auxiliary uniform_row_LUB vset
    | TGLB vset ->
	auxiliary uniform_row_GLB vset

  )

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Given a row, normalize it so that its remainder is either a clean variable, or a GLUB of clean variables.

*)
	
and normalize_row row =
  Errors.trace "Types.normalize_row" (fun () ->
    match normalize_leaf row.remainder with
      TVar { link = Some row' } ->
	{ entries = RowMap.union row.entries row'.entries; remainder = row'.remainder }
    | _ ->
	row
  )

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Given a variable, normalize its link field, perform path compression, and return its value.

*)

and normalize_link v = match v.link with
  None ->
    None
| Some row ->
    let row' = normalize_row row in
    if row != row' then
      v.link <- Some row';
    v.link

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

A few utilities for use by the following functions. 'row_iter2' simultaneously iterates over two rows with the same
set of explicit fields, and applies the specified action to each pair of corresponding leaf terms. 'set_upper_link'
(resp. 'set_lower_link') accepts two variables, packaged as leaf terms, and creates a subtyping link from the first
one up to (resp. down to) the second one. 'set_upper_bound' (resp. 'set_lower_bound') does a similar job, with a small
term as second argument, and with order of arguments reversed. 'set_guard' does a similar job, but sets up a guarded
constraint.

*)

and row_iter2 action row1 row2 =
  RowMap.iter2 action row1.entries row2.entries;
  action row1.remainder row2.remainder

and set_upper_link leaf1 leaf2 =
  match (leaf1, leaf2) with
    TVar v1, TVar v2 ->
      v1.hiset <- Set7.add v2 v1.hiset
  | _, _ ->
      raise (CantHappen "Types.set_upper_link.")

and set_lower_link leaf1 leaf2 =
  match (leaf1, leaf2) with
    TVar v1, TVar v2 ->
      v1.loset <- Set7.add v2 v1.loset
  | _, _ ->
      raise (CantHappen "Types.set_lower_link.")

and set_upper_bound term2 leaf1 =
  match leaf1 with
    TVar v1 ->
      v1.hi <- term2
  | _ ->
      raise (CantHappen "Types.set_upper_bound.")

and set_lower_bound term2 leaf1 =
  match leaf1 with
    TVar v1 ->
      v1.lo <- term2
  | _ ->
      raise (CantHappen "Types.set_lower_bound.")

and set_guard head leaf1 leaf2 leaf3 =
  match (leaf1, leaf2, leaf3) with
    TVar v1, TVar v2, TVar v3 ->
      v1.guards <- Set7.add (head, v2, v3) v1.guards
  | _ ->
      raise (CantHappen "Types.set_guard.")

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Accepts a set of field names and a type variable. Expands the variable so that it explicitly carries these fields.
The fields must be disjoint from the variable's span, and there must be at least one element in the set of requested
fields. The variable must be clean, i.e. not already expanded.

*)

and expand_clean_variable requested_fields v =
  Errors.trace "Types.expand_clean_variable" (function () ->

    (* Sanity check. *)

    if v.link <> None then
      raise (CantHappen "Variable is already expanded.");

    (* Compute the span of the remainder variable which we shall create. At the same time, perform another
       sanity check. *)

    let remainder_span = match v.span with
      Remainder signature ->
	Remainder (RowSet.union signature requested_fields)
    | Field ->
	raise (CantHappen "Variable has illegal span.") in

    let kind = kind_of_variable v in

    (* Create fresh variables. We immediately give them correct signs, because expansion might be called transparently
       in places where signs need to be preserved. Happily, they all have the same sign as the original variable,
       since they occur at the same places in the constraint graph. *)

    let make span =
      let v' = freshSK span kind in
      v'.sign <- v.sign;
      TVar v' in

    let entries = RowSet.lift (fun _ -> make Field) requested_fields in
    let remainder = make remainder_span in

    (* Expand the original variable, by setting its link. *)

    let row = { entries = entries; remainder = remainder } in
    v.link <- Some row;

    (* The variable is related, via subtyping constraints, to other variables and to its constructed bounds.
       These must be expanded as well. *)

    expand_small_bound requested_fields set_upper_bound row remainder_span v.hi;
    expand_small_bound requested_fields set_lower_bound row remainder_span v.lo;

    Set7.iter (expand_variable_bound requested_fields set_upper_link row) v.hiset;
    Set7.iter (expand_variable_bound requested_fields set_lower_link row) v.loset;

    (* Guards on the newly expanded variable must also be expanded. *)

    Set7.iter (fun (head, w1, w2) ->

      (* Expand w1 and w2. *)

      let row1 = isolate requested_fields remainder_span (TVar w1)
      and row2 = isolate requested_fields remainder_span (TVar w2) in

      (* Create the new guarded constraints. *)

      RowMap.iter3 (set_guard head) entries row1.entries row2.entries;
      set_guard head remainder row1.remainder row2.remainder

    ) v.guards;

    (* We're done expanding variables, return the result. *)

    row
  )

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

An auxiliary function of 'expand_clean_variable', in charge of expanding a small term. A difference with the case of
'expand_clean_variable' is that the (parameters of) small term may be partially expanded already.

Rather than return a row a small terms (which would incur some temporary memory overhead), the function expects a row
leaf terms as arguments, carrying exactly the requested fields, and associates each of them to the correct small term,
using the supplied association function.

Finally, to save some computations, the caller must supply the span of the row's remainder.

*)

and expand_small_bound requested_fields associate row span term =
  Errors.trace "Types.expand_small_bound" (fun () ->
    match term with

      (TBottom | TTop | TRBottom | TRMissing | TRTop | TVAbsent | TVTop) as constant ->

	(* To expand a constant small term, simply duplicate it. *)

	RowMap.iter (associate constant) row.entries;
	associate constant row.remainder

    | TAbstract(stamp, args) ->

	let argrows = List.map (isolate requested_fields span) args in

	RowMap.fold (fun label rowentry () ->
	  associate (TAbstract(stamp, List.map (fun row -> RowMap.find label row.entries) argrows)) rowentry
	) row.entries ();
	associate (TAbstract(stamp, List.map (fun row -> row.remainder) argrows)) row.remainder

    | TRPresent arg ->

	(* To expand a small term of arity 1, expand its argument, then use distributivity. *)

	let argrow = isolate requested_fields span arg in
	row_iter2 (fun leaf1 leaf2 -> associate (TRPresent leaf1) leaf2) argrow row

    | TRMaybe arg ->

	let argrow = isolate requested_fields span arg in
	row_iter2 (fun leaf1 leaf2 -> associate (TRMaybe leaf1) leaf2) argrow row

    | TVPresent arg ->

	let argrow = isolate requested_fields span arg in
	row_iter2 (fun leaf1 leaf2 -> associate (TVPresent leaf1) leaf2) argrow row

  )

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

An auxiliary function of 'expand_clean_variable', in charge of expanding a related variable. A difference with the
case of 'expand_clean_variable' is that the variable may be expanded already -- but if so, then it must carry the
same explicit fields.

The function expects a row leaf terms as arguments, carrying exactly the requested fields, and associates each of them
to the correct leaf term, using the supplied association function.

*)

and expand_variable_bound requested_fields associate row v' =
  Errors.trace "Types.expand_variable_bound" (fun () ->

    (* Expand the variable. If it is already expanded, then it must carry the correct set of fields. Indeed,
       two variables related by a constraint always carry the same set of explicit fields. If this invariant
       is false at this point, then it means that we are currently in the process of expanding both. *)

    let row' = match normalize_link v' with
      None ->
	expand_clean_variable requested_fields v'
    | Some row' ->
	row' in

    (* Replicate the subtyping constraints at the level of the newly created variables. *)

    row_iter2 associate row row'

  )

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Given an arbitrary leaf term, isolates a set of fields out of it, i.e. returns a row carrying exactly those fields.
The requested set of fields must be non-empty. Because the term might be partially expanded already, the returned
row's remainder might be expanded.

To save some computations, the caller must supply the span of the returned row's remainder.

*)

and isolate requested_fields span leaf =
  Errors.trace "Types.isolate" (fun () ->

    (* Expand the term to make the requested fields appear. The term might already be partially expanded.
       Because at least one field is requested, this call must return a single, expanded variable. *)

    let row = Standard.option (leaf_to_variable (expand_leaf requested_fields leaf)).link in

    (* Isolate the requested fields. *)

    let entries, extra = RowMap.split row.entries requested_fields in

    (* If there are no extra fields, then the row itself satisfies the specification. Otherwise, we must create
       a new row term to stand for the extra fields plus the remainder. To save computations, we expect our caller
       to supply its span. *)

    if RowMap.cardinality extra = 0 then row
    else begin

      let dummy = freshSK span (kind_of_leaf row.remainder) in
      dummy.link <- Some { entries = extra; remainder = row.remainder };
      { entries = entries; remainder = TVar dummy }

    end

  )

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Given a clean row (i.e. whose remainder is either a clean variable, or a GLUB of clean variables), force it to carry
all requested fields. The function returns a normalized row. The set of requested fields may be empty, and need not
be disjoint with the row's initial signature.

*)

and expand_clean_row requested_fields row =
  Errors.trace "Types.expand_clean_row" (function () ->

    (* Determine which fields are missing. *)

    let missing_fields = RowSet.diff requested_fields (row_signature row) in

    (* If any are actually missing, expand the remainder. *)

    if RowSet.is_empty missing_fields then row
    else begin
      let _ = expand_leaf missing_fields row.remainder in
      normalize_row row
    end

  )

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Given a variable, force it to carry all requested fields. The function returns a normalized row -- the new value of
the variable's link.

*)

and expand_variable requested_fields v =
  Errors.trace "Types.expand_variable" (fun () ->
    match normalize_link v with
      None ->
	expand_clean_variable requested_fields v
    | Some row ->
	let row = expand_clean_row requested_fields row in
	v.link <- Some row;
	row
  )

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Given a leaf term, force it to carry all requested fields. The function returns a normalized leaf: either a variable
(possibly linked to a clean row) or a GLUB of clean variables.

*)	

and expand_leaf requested_fields leaf =
  Errors.trace "Types.expand_leaf" (fun () ->
    match leaf with
      TVar v ->
	let _ = expand_variable requested_fields v in
	leaf
    | TLUB vset ->
	let _ = expand_variable requested_fields (Set7.choose vset) in
	normalize_leaf leaf
    | TGLB vset ->
	let _ = expand_variable requested_fields (Set7.choose vset) in
	normalize_leaf leaf
  )

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Computing GLBs and LUBs.

*)

and terms_LUB ty1 ty2 =
  Errors.trace "Types.terms_LUB" (fun () ->
    match (ty1, ty2) with

      (* KRegular. *)

      TBottom, _ ->
    	ty2
    | _, TBottom ->
    	ty1
    | _, TTop
    | TTop, _ ->
	TTop
    | TAbstract(stamp1, args1), TAbstract(stamp2, args2) when stamp1 = stamp2 ->
	TAbstract(stamp1,
		  Standard.list_fold_right3 (fun sign arg1 arg2 accu ->
		    ((if sign then vars_LUB else vars_GLB) arg1 arg2) :: accu
		  ) (Abstract.get_variance stamp1) args1 args2 []
                 )
    | TAbstract _, TAbstract _ ->
	TTop

    (* KRecord. *)

    | TRBottom, _ ->
	ty2
    | _, TRBottom ->
	ty1
    | TRTop, _
    | _, TRTop ->
	TRTop
    | TRMissing, TRMissing ->
	TRMissing
    | TRPresent ty1, TRPresent ty2 ->
    	TRPresent (vars_LUB ty1 ty2)
    | TRMaybe ty1, TRMaybe ty2 ->
	TRMaybe (vars_LUB ty1 ty2)
    | TRMissing, TRPresent ty ->
	TRMaybe ty
    | TRPresent ty, TRMissing ->
	TRMaybe ty
    | TRMissing, TRMaybe _ ->
	ty2
    | TRMaybe _, TRMissing ->
	ty1
    | TRPresent ty1, TRMaybe ty2 ->
	TRMaybe (vars_LUB ty1 ty2)
    | TRMaybe ty1, TRPresent ty2 ->
	TRMaybe (vars_LUB ty1 ty2)

    (* KVariant. *)

    | _, TVTop
    | TVTop, _ ->
	TVTop
    | TVAbsent, _ ->
    	ty2
    | _, TVAbsent ->
    	ty1
    | TVPresent ty1, TVPresent ty2 ->
    	TVPresent (vars_LUB ty1 ty2)

    (* Some, but not all, kinding errors are detected here. *)

    | _, _ ->
    	raise (CantHappen "Kind inconsistency.")

  )

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Taking the union of two rows. The rows must carry the same set of explicit fields.

*)

and uniform_row_LUB row1 row2 =
  Errors.trace "Types.uniform_row_LUB" (fun () ->

    (* Compute a LUB on each entry. *)

    let entries = RowMap.fold2 (fun label ty1 ty2 entries ->
      RowMap.add label (vars_LUB ty1 ty2) entries
    ) row1.entries row2.entries empty_row_map

    and remainder = vars_LUB row1.remainder row2.remainder in

    { entries = entries; remainder = remainder }

  )

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Taking the union of two leaf terms. The terms need not be normalized.

*)

and vars_LUB ty1 ty2 =
  Errors.trace "Types.vars_LUB" (fun () ->
    match (ty1, ty2) with
      TVar v1, TVar v2 ->
    	if v1 == v2 then ty1
    	else TLUB (Set7.add v1 (Set7.add v2 empty_variable_set))
    | TVar v, TLUB vset ->
    	if Set7.mem v vset then ty2
    	else TLUB (Set7.add v vset)
    | TLUB vset, TVar v ->
    	if Set7.mem v vset then ty1
    	else TLUB (Set7.add v vset)
    | TLUB vset1, TLUB vset2 ->
    	TLUB (Set7.union vset1 vset2)
    | _, _ ->
    	raise (CantHappen "Variables or LUBs only.")
  )

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The same routines, this time for GLBs. Factoring the code wouldn't be easy here because of pattern matching, and
it would be slower anyway.

*)

and terms_GLB ty1 ty2 =
  Errors.trace "Types.terms_GLB" (fun () ->
    match (ty1, ty2) with
      TTop, _ ->
    	ty2
    | _, TTop ->
    	ty1
    | _, TBottom
    | TBottom, _ ->
	TBottom
    | TAbstract(stamp1, args1), TAbstract(stamp2, args2) when stamp1 = stamp2 ->
	TAbstract(stamp1,
		  Standard.list_fold_right3 (fun sign arg1 arg2 accu ->
		    ((if sign then vars_GLB else vars_LUB) arg1 arg2) :: accu
		  ) (Abstract.get_variance stamp1) args1 args2 []
                 )
    | TAbstract _, TAbstract _ ->
	TBottom

    | TRBottom, _
    | _, TRBottom
    | TRMissing, TRPresent _
    | TRPresent _, TRMissing ->
	TRBottom
    | TRTop, _ ->
    	ty2
    | _, TRTop ->
    	ty1
    | TRMissing, TRMissing
    | TRMissing, TRMaybe _
    | TRMaybe _, TRMissing ->
	TRMissing
    | TRPresent ty1, TRPresent ty2 ->
    	TRPresent (vars_GLB ty1 ty2)
    | TRMaybe ty1, TRMaybe ty2 ->
	TRMaybe (vars_GLB ty1 ty2)
    | TRPresent ty1, TRMaybe ty2 ->
	TRPresent (vars_GLB ty1 ty2)
    | TRMaybe ty1, TRPresent ty2 ->
	TRPresent (vars_GLB ty1 ty2)

    | TVTop, _ ->
	ty2
    | _, TVTop ->
	ty1
    | TVAbsent, _
    | _, TVAbsent ->
    	TVAbsent
    | TVPresent ty1, TVPresent ty2 ->
    	TVPresent (vars_GLB ty1 ty2)

    | _, _ ->
    	raise (CantHappen "Kind inconsistency.")

  )

and uniform_row_GLB row1 row2 =
  Errors.trace "Types.uniform_row_GLB" (fun () ->

    let entries = RowMap.fold2 (fun label ty1 ty2 entries ->
      RowMap.add label (vars_GLB ty1 ty2) entries
    ) row1.entries row2.entries empty_row_map

    and remainder = vars_GLB row1.remainder row2.remainder in

    { entries = entries; remainder = remainder }
  )

and vars_GLB ty1 ty2 =
  Errors.trace "Types.vars_GLB" (fun () ->
    match (ty1, ty2) with
      TVar v1, TVar v2 ->
    	if v1 == v2 then ty1
    	else TGLB (Set7.add v1 (Set7.add v2 empty_variable_set))
    | TVar v, TGLB vset ->
    	if Set7.mem v vset then ty2
    	else TGLB (Set7.add v vset)
    | TGLB vset, TVar v ->
    	if Set7.mem v vset then ty1
    	else TGLB (Set7.add v vset)
    | TGLB vset1, TGLB vset2 ->
    	TGLB (Set7.union vset1 vset2)
    | _, _ ->
    	raise (CantHappen "Variables or GLBs only.")
  )

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

These functions perform propagation over row terms. The first one is only a special case of the second one, optimized
for speed. It accepts two action functions, instead of one, again for speed in the most common case.

*)

let match_variables vaction action v1 v2 =
  Errors.trace "Types.match_variables" (fun () ->
    match normalize_link v1, normalize_link v2 with
      None, None ->
	vaction v1 v2
    | Some row1, None ->
	let row2 = expand_clean_variable (row_signature row1) v2 in
	row_iter2 action row1 row2
    | None, Some row2 ->
	let row1 = expand_clean_variable (row_signature row2) v1 in
	row_iter2 action row1 row2
    | Some row1, Some row2 ->
	let sig1 = row_signature row1
	and sig2 = row_signature row2 in
	let row1 = expand_clean_row sig2 row1
	and row2 = expand_clean_row sig1 row2 in
	row_iter2 action row1 row2
  )

let match_leaves action leaf1 leaf2 =
  Errors.trace "Types.match_leaves" (fun () ->
    
    let fields1 = accumulate_leaf leaf1 RowSet.empty
    and fields2 = accumulate_leaf leaf2 RowSet.empty in

    let leaf1 = if RowSet.is_empty fields2 then normalize_leaf leaf1 else expand_leaf fields2 leaf1
    and leaf2 = if RowSet.is_empty fields1 then normalize_leaf leaf2 else expand_leaf fields1 leaf2 in

    match leaf1, leaf2 with
      TVar v1, TVar v2 -> (
	match v1.link, v2.link with
	  None, None ->
	    action leaf1 leaf2
	| Some row1, Some row2 ->
	    row_iter2 action row1 row2
	| _, _ ->
	    raise (CantHappen "Both variables should now carry the same fields.")
      )
    | _, _ ->
	action leaf1 leaf2
  )

(* ----------------------------------------------------------------------------------------------------------------- *)
(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Maps from variables to arbitrary data.

*)

module VarMap = Map.Make (struct
    type t = type_variable
    let compare = compare_variables
  end)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This exception is raised whenever an inconsistent constraint is encountered.

*)

type any_term =
    LeafTerm of leaf_term
  | SmallTerm of small_term

exception Inconsistent of (any_term * any_term) list

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This is the only function in charge of performing propagation. It accepts a constraint between two small terms and
calls the action function for each of its subconstraints. The action function is passed a sign, which indicates
whether the subconstraint at hand is the result of a covariant or contravariant propagation. Exception Inconsistent is
raised if the head constructors are incompatible.

*)

let signed_subconstraints action ty1 ty2 =
  Errors.trace "Types.signed_subconstraints" (fun () ->

    try

      match (ty1, ty2) with

      (* KRegular. *)

	TBottom, _
      | _, TTop ->
	  ()
      | TAbstract(stamp1, args1), TAbstract(stamp2, args2) when stamp1 = stamp2 ->
	  Standard.list_iter3 (fun sign type1 type2 ->
	    if sign then action sign type1 type2
	    else action sign type2 type1
	  ) (Abstract.get_variance stamp1) args1 args2

      (* KRecord. *)

      | TRBottom, _
      | _, TRTop
      | TRMissing, TRMissing
      | TRMissing, TRMaybe _ ->
	  ()
      | TRPresent type1, TRPresent type2 ->
	  action true type1 type2
      | TRMaybe type1, TRMaybe type2 ->
	  action true type1 type2
      | TRPresent type1, TRMaybe type2 ->
	  action true type1 type2

      (* KVariant. *)

      | TVAbsent, _
      | _, TVTop ->
	  ()
      | TVPresent type1, TVPresent type2 ->
	  action true type1 type2

      (* If we reach this point, we have two incompatible types. Kinding errors are not detected. *)

      | _ ->
	  raise (Inconsistent [])

    with Inconsistent constraint_list ->
      raise (Inconsistent ((SmallTerm ty1, SmallTerm ty2) :: constraint_list))

  )

let subconstraints action ty1 ty2 =
  signed_subconstraints (fun _ ty1 ty2 -> action ty1 ty2) ty1 ty2

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Working with paths. These calls are used during minimization.

for_each_leaf reads a small term. It looks for the leaves and calls an action function for each (index, variable)
combination. It is basically equivalent to building a list of (index, leaf) pairs, but it can be more efficient
depending on what the caller intends to do with the data.

If for_each_leaf ranges over an interval [0..r[, then arity returns r.

Records and variants are dealt with as follows. If the argument of unary abstract type is an expanded variable, then
we consider the elements of the row to be the arguments. The remainder of the row is always argument number 0; the
row's entries follow.

*)

let for_each_leaf action term =
  match term with
    TBottom
  | TTop
  | TRTop
  | TRMissing
  | TVAbsent
  | TRBottom
  | TVTop ->
      ()
  | TAbstract (_, [ (TVar v) as arg ]) -> (
      match normalize_link v with
	Some row ->
	  action 0 row.remainder;
	  let _ = RowMap.fold (fun _ leaf index ->
	    action index leaf;
	    succ index
	  ) row.entries 1 in
	  ()
      |	None ->
	  action 0 arg
    )
  | TAbstract (_, args) ->
      let _ = List.fold_left (fun index arg -> action index arg; succ index) 0 args in
      ()
  | TRPresent ty ->
      action 0 ty
  | TRMaybe ty ->
      action 0 ty
  | TVPresent ty ->
      action 0 ty

let arity = function
    TBottom
  | TTop
  | TRTop
  | TRMissing
  | TVAbsent
  | TRBottom
  | TVTop ->
      0
  | TAbstract (_, [ (TVar v) as arg ]) -> (
      match normalize_link v with
	Some row ->
	  1 + RowMap.cardinality row.entries
      |	None ->
	  1
    )
  | TAbstract (_, args) ->
      List.length args
  | TRPresent _
  | TRMaybe _
  | TVPresent _ ->
      1

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Most types of kind KReguar can be viewed as abstract types, thus eliminating all of the code necessary to handle
them. All we have to do is declare their names, arities and variances here.

Ref types have two parameters: the input type and the output type. The first one is the type of things that can be
written into the ref; the second one is the type obtained when reading from the ref.

*)

let make_tarrow =
  let stamp = Abstract.create "->" [ false; true; true ] in
  fun arg1 arg2 arg3 ->
    TAbstract(stamp, [ arg1; arg2; arg3 ])

let make_tproduct =
  let stamp = Abstract.create "*" [ true; true ] in
  fun arg1 arg2 ->
    TAbstract(stamp, [ arg1; arg2 ])

let make_tref =
  let stamp = Abstract.create "ref" [ false; true ] in
  fun arg1 arg2 ->
    TAbstract(stamp, [ arg1; arg2 ])

let make_tvect =
  let stamp = Abstract.create "vect" [ false; true ] in
  fun arg1 arg2 ->
    TAbstract(stamp, [ arg1; arg2 ])

let atom_int =
  let stamp = Abstract.create "int" [] in
  TAbstract(stamp, [])

let atom_unit =
  let stamp = Abstract.create "unit" [] in
  TAbstract(stamp, [])

let atom_bool =
  let stamp = Abstract.create "bool" [] in
  TAbstract(stamp, [])

let atom_float =
  let stamp = Abstract.create "float" [] in
  TAbstract(stamp, [])

let atom_char =
  let stamp = Abstract.create "char" [] in
  TAbstract(stamp, [])

let atom_string =
  let stamp = Abstract.create "string" [] in
  TAbstract(stamp, [])

let make_trecord =
  let stamp = Abstract.create "{}" [ true ] in
  fun entries remainder ->
    let leaf =
      if RowMap.is_empty entries then
	remainder
      else begin
	let dummy = freshSK (Remainder RowSet.empty) KRecord in
	dummy.link <- Some { entries = entries; remainder = remainder };
	TVar dummy
      end in
    TAbstract(stamp, [ leaf ])

let make_tvariant =
  let stamp = Abstract.create "[]" [ true ] in
  fun entries remainder ->
    let leaf =
      if RowMap.is_empty entries then
	remainder
      else begin
	let dummy = freshSK (Remainder RowSet.empty) KVariant in
	dummy.link <- Some { entries = entries; remainder = remainder };
	TVar dummy
      end in
    TAbstract(stamp, [ leaf ])

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Comparing variable spans.

*)

let compare_spans span1 span2 =
  match (span1, span2) with
    Field, Field ->
      0
  | Field, Remainder _ ->
      -1
  | Remainder _, Field ->
      1
  | Remainder sig1, Remainder sig2 ->
      RowSet.compare sig1 sig2

