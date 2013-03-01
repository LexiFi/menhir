(* $Header: /home/pauillac/cristal5/remy/repository/home/tex/mlrow/code/print.ml,v 1.5 2004/03/30 09:18:58 fpottier Exp $ *)

(** This module provides a simple pretty-printer for the terms
    maintained by a unifier.

    We follow the convention that types and type schemes are represented
    using the same data structure. In the case of type schemes, universally
    quantified type variables are distinguished by the fact that their rank
    is [none]. The pretty-printer binds these variables locally, while other
    (non-quantified) type variables are considered part of a global namespace. *)

open Sig

module Make
    (U : Unifier)
= struct

  open U

  (** The things that we print are [variable]s, that is, entry points
      for types or type schemes. *)
  type printable = variable

  (** [name i] turns the integer [i] into a type variable name. *)
  let rec name i =
    if i < 26 then
      String.make 1 (Char.chr (0x61 + i))
    else
      name (i / 26 - 1) ^ name (i mod 26)

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

  type symbol =
    | SVariable
    | SEquation
    | STerm of U.A.symbol

  let parenthesize label = function
    | SVariable ->
	false
    | SEquation ->
	true
    | STerm symbol ->
	U.A.parenthesize label symbol

  type context = bool

  (** [print is_type_scheme v] returns a printable representation of
      the type or type scheme whose entry point is [v]. The parameter
      [is_type_scheme] tells whether [v] should be interpreted as a
      type or as a type scheme. Recursive types are dealt with by
      printing inline equations. Consecutive calls to [print] share
      the same variable naming conventions, unless [reset] is called
      in between. *)
  let print is_type_scheme v =

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
    let name v =
      let prefix, c, h =
	if is_type_scheme && (UnionFind.find v).rank = Rank.none then
	  "'", i, history
	else
	  "'_", gi, ghistory
      in
      try
	Misc.assocp (UnionFind.equivalent v) !h
      with Not_found ->
	incr c;
	let result = prefix ^ name !c in
	h := (v, result) :: !h;
	result
    in

    (** The traversal. *)
    let rec print v =
      let desc = UnionFind.find v in

      (* If this variable was visited already, we mark it as ``hit
	 again'', so as to record the fact that we need to print an
	 equation at that node when going back up. *)

      if (Mark.same desc.mark visiting) || (Mark.same desc.mark hit) then begin

	desc.mark <- hit;
	SVariable, name v

      end

      (* If this variable was never visited, we mark it as ``being
	 visited'' before processing it, so as to detect cycles.
	 If, when we are done with this variable, its mark has
	 changed to ``hit again'', then it must be part of a cycle,
	 and we annotate it with an inline equation. *)

      else begin

	desc.mark <- visiting;

	let symbol, string = 
	  match desc.structure with
	  | None ->
	      SVariable, name v
	  | Some term ->
	      let symbol, string =
		U.A.print (fun label son ->
		  let symbol, string = print son in
		  if parenthesize label symbol then
		    "(" ^ string ^ ")"
		  else
		    string
                ) term in
	      STerm symbol, string in

	if desc.mark == hit then
	  SEquation, name v ^ " = " ^ string
	else begin
	  (* ``hit'' marks are not removed, so this node keeps its name.
	     This avoids printing the equation again if the node is later
	     found again. *)
	  desc.mark <- Mark.none;
	  symbol, string
	end

      end in

    (* Initiate the traversal. *)
    let _, body = print v in

    (* Build the list of universal quantifiers and tack it in front of the body. *)
    match !history with
    | [] ->
	body
    | history ->
	List.fold_left (fun quantifiers (v, _) -> quantifiers ^ " " ^ (name v)) "forall" history ^ " . " ^ body

end

