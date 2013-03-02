(* $Id$ *)

(** This module provides a simple pretty-printer for the terms
    maintained by a unifier.

    We follow the convention that types and type schemes are represented
    using the same data structure. In the case of type schemes, universally
    quantified type variables are distinguished by the fact that their rank
    is [none]. The pretty-printer binds these variables locally, while other
    (non-quantified) type variables are considered part of a global namespace. 
*)

open Sig
open Misc

module Make (MultiEquation : MultiEquation)
: TermPrinter with type variable = MultiEquation.variable
	  and type term = MultiEquation.crterm

= struct

  open MultiEquation
  open MultiEquation.Algebra

  (** The things that we print are [variable]s, that is, entry points
      for types or type schemes. *)
  type variable = MultiEquation.variable

  type term = MultiEquation.crterm

  (** [name_from_int i] turns the integer [i] into a type variable name. *)
  let rec name_from_int i =
    if i < 26 then
      String.make 1 (Char.chr (0x61 + i))
    else
      name_from_int (i / 26 - 1) ^ name_from_int (i mod 26)

  (** [gi] is the last consumed number. *)
  let gi =
     ref (-1)

  (** [ghistory] is a mapping from variables to variable names. *)
  let ghistory =
    ref []

  (** [reset()] clears the global namespace, which is implemented
     by [gi] and [ghistory]. *)
  let reset () =
    gi := -1;
    ghistory := []

  type arg = 
      Arg of (MultiEquation.variable * string * arg list 
	      * bool * associativity * bool)
    
  let paren b e = if b then "("^^e^^")" else e


  (** [print is_type_scheme v] returns a printable representation of
      the type or type scheme whose entry point is [v]. The parameter
      [is_type_scheme] tells whether [v] should be interpreted as a
      type or as a type scheme. Recursive types are dealt with by
      printing inline equations. Consecutive calls to [print] share
      the same variable naming conventions, unless [reset] is called
      in between. *)
  let printer ?user_name_from_int is_type_scheme =
    let name_from_int = default name_from_int user_name_from_int in

    (** Create marks to deal with cycles. *)
    let visiting = Mark.fresh()
    and hit = Mark.fresh() in

    (** Create a local namespace for this type scheme. *)
    let i = ref (-1)
    and history = ref [] in

    (** [name v] looks up or assigns a name to the variable [v]. When
	dealing with a type scheme, then the local or global namespace
	is used, depending on whether [v] is universally quantified or
	not. When dealing with a type, only the global namespace is
	used. *)

    (* FIXME: necessite du prefixe ? *)
    let rec var_name hits visited v =
      let desc = UnionFind.find v in
      let autoname () = 
	let prefix, c, h =
	  if is_type_scheme && desc.rank = Rank.none 
	  then
	    "", i, history
	  else
	    "_", gi, ghistory
	in
	  try
	    Misc.assocp (UnionFind.equivalent v) !h 
	  with Not_found ->
	    incr c;
	    let result = prefix ^ name_from_int !c in
	      desc.name <- Some result;
	      h := (v, result) :: !h;
	      result
      in
	(match desc.name with
	  | Some name -> 
	      if desc.kind <> Constant then
		try
		  Misc.assocp (UnionFind.equivalent v) !history
		with Not_found ->
		  history := (v, name) :: !history;
		  name
	      else name
	  | _ -> autoname ())
(*	  ^ ("["^string_of_int (Obj.magic desc.rank : int)^"]") *)
(*	  ^ (match desc.kind with Constant -> "#" | Rigid -> "!" | _ -> "?") *)
    in

    (* Term traversal. *)
    let rec print_variable ?use_user_def hits visited v = 

      let is_hit v =
	Mark.same (UnionFind.find v).mark hit
      and is_visited v = 
	Mark.same (UnionFind.find v).mark visiting
      in
      let var_or_sym v = 
	(match variable_name v with
	  | Some name ->
	      (match as_symbol name with
		 | Some sym ->
		     (v, name, [], infix sym, associativity sym, false)
		 | None -> 
		     (v, var_name hits visited v, [], false, NonAssoc, false))
	  | None -> 
	      (v, var_name hits visited v, [], false, NonAssoc, false))
      in
      let desc = UnionFind.find v in
	
      (* If this variable was visited already, we mark it as ``hit
	 again'', so as to record the fact that we need to print an
	 equation at that node when going back up. *)

	if is_hit v || is_visited v then
	  begin
	    desc.mark <- hit;
	    var_or_sym v 
	  end

      (* If this variable was never visited, we mark it as ``being
	 visited'' before processing it, so as to detect cycles.
	 If, when we are done with this variable, its mark has
	 changed to ``hit again'', then it must be part of a cycle,
	 and we annotate it with an inline equation. *)

      else begin

	desc.mark <- visiting;
	match desc.structure with
	  | None -> var_or_sym v
	  | Some t -> 
	      let (v', name, args, infix, assoc, p) as r = 
		print_term hits visited t in
		if is_hit v then 
     		  (v, var_name hits visited v^" =", 
		   [ Arg (v', name, args, infix, assoc, p) ],
		   false, assoc, true) 
		else (desc.mark <- Mark.none; r)
      end

    and print_term ?use_user_def hits visited t = 
      let at_left  = function [] -> true | [ x ] -> false | _ -> assert false
      and at_right = function [] -> true | [ x ] -> false | _ -> assert false
      in
      let rec print = function
	      
	| App (t1, t2) -> 
	    
	    let (op1, name1, args1, infix1, assoc1, force_paren1) = 
	      print_variable hits visited t1
	    and (op2, name2, args2, infix2, assoc2, force_paren2) =
	      print_variable hits visited t2
	    in
	    let priority name = 
	      match as_symbol name with
		| Some sym -> Algebra.priority sym
		| None     -> -1
	    in
	    let paren_t2 = force_paren2 || 
	      if are_equivalent op1 op2 then
		(assoc2 = AssocLeft && at_right args1) 
		|| (assoc2 = AssocRight && at_left args1) 
	      else
		(priority name2 > priority name1)
	    in
	      (op1, name1,
	       (args1 @ [ Arg (op2, name2, args2, infix2, assoc2, paren_t2)]), 
	       infix1, assoc1, force_paren1)
		
	| Var v -> print_variable hits visited v

	| RowCons (label, typ, r) -> 
	    let typv = print_variable hits visited typ in
	    (variable Flexible (), ";", 
	     [
	       Arg (variable Flexible (), RowLabel.export label^":", 
		    [ Arg typv ], false, NonAssoc, false);
	       Arg (print_variable hits visited r)],
	       true, NonAssoc, false)

	| RowUniform typ ->
	    (variable Flexible (), "\\", 
	     [ Arg (print_variable hits visited typ) ],
	     false, NonAssoc, false)

      in print t
    in
    let prefix hits visited () = 
      if is_type_scheme then
	match !history with
	  | [] ->
	      ""
	  | history ->
	      List.fold_left 
		(fun quantifiers (v, _) -> 
		   quantifiers ^ " " ^ (var_name hits visited v)) 
		"forall" (List.rev history) ^ ". " 
      else ""

    in let as_string f r = 
	let rec loop (Arg (_, name, args, infix, assoc, is_paren)) =
	  if args = [] then name 
	  else 
	    paren is_paren
	      (if infix then 
		 print_separated_list (" "^^name^^" ") loop args
	       else 
		 (match assoc with EnclosedBy (t, _) -> t | _ -> name) ^^
		   (if args <> [] then " " else "")^^
		   (print_separated_list " " loop args)^^
		   (match assoc with EnclosedBy (_,t) -> " "^t | _ -> "")
	      )
	in
	let hits, visited = ref [], ref [] in
	let (op, name, args, infix, assoc, _) = f hits visited r in
	  prefix hits visited () 
	  ^ loop (Arg (op, name, args, infix, assoc, false))
    in
      (as_string print_variable, as_string print_term)

  let print_term ?user_name_from_int b t =
    let t = explode t in
    (snd (printer ?user_name_from_int:user_name_from_int b)) t

  let print_variable ?user_name_from_int b v =
    (fst (printer ?user_name_from_int:user_name_from_int b)) v

end

