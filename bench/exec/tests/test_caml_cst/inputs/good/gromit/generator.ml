(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/gromit/generator.ml,v 1.24 2000/02/11 16:15:35 fpottier Exp $ *)

(* Gromit's code generator. *)

(*i*)

open Syntax
open Signature
open Ordering

(*i*)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Dealing with output channels} *)

let channel =
  ref None

let create_file name =
  channel := Some (open_out name)

let emit data =
  match !channel with
    Some channel ->
      output_string channel data
  | None ->
      assert false

let close_file () =
  match !channel with
    Some channel ->
      close_out channel
  | None ->
      assert false

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Auxiliary output functions} *)

(* This auxiliary function prints a set of strings. How each element, and separator, should be printed is defined
   by the caller, which allows very flexible uses. How the empty set should be printed is also definable. Below is
   a variant of this function, which expects a list instead of a set. *)

let print_set print_empty print_element print_separator elements =
  try
    let first = StringSet.choose elements in
    let rest = StringSet.remove first elements in
    print_element first;
    StringSet.iter (fun element ->
      print_separator();
      print_element element
    ) rest
  with Not_found ->
    print_empty()

let print_list print_empty print_element print_separator = function
  | [] ->
      print_empty()
  | first :: rest ->
      print_element first;
      List.iter (fun element ->
	print_separator();
	print_element element
      ) rest

(* These functions define the way symbol names are turned into Objective Caml constructor names. *)

let caml_term symbol_name =
  "T" ^ symbol_name

let caml_symbol symbol_name =
  "H" ^ symbol_name

let caml_kind lattice =
  "K" ^ lattice.lattice_name

let caml_token token =
  "Token" ^ token

(* This auxiliary function accepts a symbol (defined by its number and information structure). It emits a pattern
   for a term whose head constructor is this symbol. If [transformer] is [None], then any arguments are replaced with a
   wildcard pattern. Otherwise, label names, possibly modified by the transformer function, are used to bind the
   term's arguments. *)

let emit_term_pattern transformer num info =
  emit (caml_term info.symbol_name);
  if not (StringSet.is_empty info.symbol_arity) then
    match transformer with
      None ->
	emit " _"
    | Some transformer ->
	emit " (";
	print_set
	  (fun () -> ())
	  (fun label -> emit (transformer label))
	  (fun () -> emit ", ")
	  info.symbol_arity;
	emit ")"

(* This auxiliary function accepts a lattice. It emits a pattern block which matches all terms (resp. symbols)
   belonging to this lattice, if [what] is [MatchTerms] (resp. [MatchSymbols]), or the lattice's kind, if
   [what] is [MatchKinds]. *)

type what =
  | MatchTerms
  | MatchSymbols
  | MatchKinds

let emit_lattice_pattern what lattice =
  match what with
  | MatchTerms
  | MatchSymbols ->
      Array.iteri (fun num info ->
	emit "    | ";
	if what = MatchTerms then
	  emit_term_pattern None num info
	else
	  emit (caml_symbol info.symbol_name);
	emit "\n"
      ) lattice.lattice_symbol
  | MatchKinds ->
      emit (Printf.sprintf "    | %s\n" (caml_kind lattice))

(* This auxiliary function emits a function from lattices into constant values. More precisely, [what] tells whether
   the function should expect terms, symbols or kinds. [name] is the compiled function's name, and [live] is the
   function from lattices into strings which defines the function to be emitted. *)

let emit_lattice_function what name live signature =
  emit (Printf.sprintf "  let %s = function\n" name);
  List.iter (fun lattice ->
    emit_lattice_pattern what lattice;
    emit (Printf.sprintf "    -> %s\n" (live lattice))
  ) signature;
  emit "\n"

(* This auxiliary function emits a clause which catches kinding errors in a binary function. These can occur if the
   signature contains more than one lattice. *)

let emit_catch_kinding signature =
  if List.length signature > 1 then begin
    emit "    | (_, _) ->\n";
    emit "        assert false\n\n"
  end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Emitting code} *)

(* Define the type of terms. It is not a recursive type; it is parameterized over a variable 'a, which represents the
   type of leaves. *)

let emit_term_declaration signature =
  emit "  type 'a t =\n\n";
  List.iter (fun lattice ->

    Array.iter (fun symbol_info ->
      emit "    | ";
      emit (caml_term symbol_info.symbol_name);
      let arity = symbol_info.symbol_arity in
      if not (StringSet.is_empty arity) then begin
	emit " of ";
	print_set (fun () -> ()) (fun _ -> emit "'a") (fun () -> emit " * ") arity
      end;
      emit "\n"
    ) lattice.lattice_symbol;
    emit "\n"

  ) signature

(* Define the type of term expressions. This is the structure of abstract syntax trees produced by the type expression
   parser. *)

let emit_term_expression_decl signature =
  emit "  type 'a expression = {\n";
  emit "      actual: 'a actual_expression;\n";
  emit "      mutable info: 'a option\n";
  emit "    }\n\n";
  emit "  and 'a actual_expression =\n";
  emit "    | Variable of string\n";
  emit "    | Term of ('a expression) Term.t\n";
  emit "    | RowExtension of string * 'a expression * 'a expression\n";
  emit "    | RowUniform of 'a expression\n\n";
  emit "  type 'a coercion =\n";
  emit "    | Coercion of 'a expression * 'a expression\n";
  emit "    | Conditional of Symbol.t * 'a expression";
  emit          " * 'a expression * 'a expression\n\n"

(* Define the type of symbols (i.e. head constructors of terms). *)

let emit_symbol_declaration signature =
  emit "  type t =\n\n";
  List.iter (fun lattice ->

    Array.iter (fun symbol_info ->
      emit (Printf.sprintf "    | %s\n" (caml_symbol symbol_info.symbol_name))
    ) lattice.lattice_symbol;
    emit "\n"

  ) signature

(* Define the type of kinds (i.e. lattices). *)

let emit_kind_declaration signature =
  emit "  type t =\n";
  List.iter (fun lattice ->
    emit (Printf.sprintf "    | %s\n" (caml_kind lattice))
  ) signature;
  emit "\n"

(* Define the ordering on symbols. The function in fact compares a symbol and a term. *)

let emit_symbol_comparison signature =
  emit "  let ordered symbol term =\n";
  emit "    match (symbol, term) with\n\n";
  List.iter (fun lattice ->

    let emit_patterns reverse value =
      let at_least_one = ref false in

      Array.iteri (fun num1 info1 ->
	Array.iteri (fun num2 info2 ->
	  if lattice.lattice_lattice.lattice_matrix.(num1).(num2) <> reverse then begin
	    emit "    | (";
	    emit (caml_symbol info1.symbol_name);
	    emit ", ";
	    emit_term_pattern None num2 info2;
	    emit ")\n";
	    at_least_one := true
	  end
	) lattice.lattice_symbol
      ) lattice.lattice_symbol;

      if !at_least_one then
	emit (Printf.sprintf "    -> %s\n" value) in

    emit_patterns false "true";
    emit_patterns true "false";
    emit "\n"

  ) signature;
  emit_catch_kinding signature

(* Mapping terms and symbols to kinds. *)

let emit_kind_of_term signature =
  emit_lattice_function MatchTerms "of_term" caml_kind signature
let emit_kind_of_symbol signature =
  emit_lattice_function MatchSymbols "of_symbol" caml_kind signature

(* Mapping terms/kinds to the kind's bottom/top element. *)

let bottom_of_lattice lattice =
  caml_term lattice.lattice_symbol.(lattice.lattice_lattice.lattice_bot).symbol_name

let top_of_lattice lattice =
  caml_term lattice.lattice_symbol.(lattice.lattice_lattice.lattice_top).symbol_name

(* Mapping a term to its head constructor and to its arity. *)

let emit_head_of_term signature =
  emit "  let of_term = function\n";
  List.iter (fun lattice ->
    Array.iteri (fun num info ->
      emit "    | ";
      emit_term_pattern None num info;
      emit " ->\n        ";
      emit (caml_symbol info.symbol_name);
      emit "\n"
    ) lattice.lattice_symbol
  ) signature;
  emit "\n"

let emit_arity signature =
  emit "  let arity = function\n";
  List.iter (fun lattice ->
    Array.iteri (fun num info ->
      emit "    | ";
      emit_term_pattern None num info;
      emit " ->\n        ";
      emit (Printf.sprintf "%d\n" (StringSet.cardinal info.symbol_arity))
    ) lattice.lattice_symbol
  ) signature;
  emit "\n"

(* Propagation. There are two variants of this function; one which keeps track of signs, and one which doesn't. *)

let emit_clash () =
  emit "  exception Clash\n\n"

let emit_propagate track_signs signature =
  emit (Printf.sprintf
    "  let %s action term1 term2 =\n"
    (if track_signs then "sbreak" else "break"));
  emit "    match (term1, term2) with\n";
  List.iter (fun lattice ->

    (* Iterate over all possible pairs of arguments. *)

    Array.iteri (fun num1 info1 ->
      Array.iteri (fun num2 info2 ->

	(* Check whether this pair is in the subtyping relation. *)

	if lattice.lattice_lattice.lattice_matrix.(num1).(num2) then begin

	  (* Emit this pair's pattern, with names for the term's arguments. *)

	  emit "    | (";
	  emit_term_pattern (Some (fun label -> label ^ "1")) num1 info1;
	  emit ", ";
	  emit_term_pattern (Some (fun label -> label ^ "2")) num2 info2;
	  emit ") ->\n";

	  (* For each label which is common to both arguments, call the action function. If the label is
	     contravariant, reverse the order of the arguments. *)

	  print_set
	    (fun () -> emit "        ()")
	    (fun label ->
	      let printer =
		match (track_signs, Hashtbl.find lattice.lattice_label label) with
		| false, (Covariant, _, _) ->
		    Printf.sprintf "        action %s1 %s2"
		| false, (Contravariant, _, _) ->
		    Printf.sprintf "        action %s2 %s1"
		| true, (Covariant, _, _) ->
		    Printf.sprintf "        action true %s1 %s2"
		| true, (Contravariant, _, _) ->
		    Printf.sprintf "        action false %s2 %s1" in
	      emit (printer label label))
	    (fun () -> emit ";\n")
	    (StringSet.inter info1.symbol_arity info2.symbol_arity);
	  emit "\n"
	  
	end
	  
      ) lattice.lattice_symbol
    ) lattice.lattice_symbol;

    (* Now, iterate over all inconsistent pairs of arguments. *)

    let trivial = ref true in

    Array.iteri (fun num1 info1 ->
      Array.iteri (fun num2 info2 ->
	if not lattice.lattice_lattice.lattice_matrix.(num1).(num2) then begin

	  emit "    | (";
	  emit_term_pattern None num1 info1;
	  emit ", ";
	  emit_term_pattern None num2 info2;
	  emit ")\n";

	  trivial := false
	end
      ) lattice.lattice_symbol
    ) lattice.lattice_symbol;

    if not !trivial then
      emit "    -> raise Clash\n";
    emit "\n"

  ) signature;
  emit_catch_kinding signature

(* Iterating over a term's leaves. There are four variants of this function: with or without signs, and with or
   without an accumulator. A fifth variant uses kinds instead of signs. *)

let emit_fold_leaves use_signs signature =
  emit (Printf.sprintf
    "  let %sfold action %sterm accu =\n"
    (if use_signs then "s" else "")
    (if use_signs then "sign " else ""));
  emit "    match term with\n";
  List.iter (fun lattice ->
    Array.iteri (fun num info ->

      emit "    | ";
      emit_term_pattern (Some (fun label -> label)) num info;
      emit " ->\n";

      StringSet.iter (fun label ->
	emit "        let accu = action ";
	if use_signs then begin
	  match Hashtbl.find lattice.lattice_label label with
	  | Covariant, _, _ ->
	      emit "sign "
	  | Contravariant, _, _ ->
	      emit "(not sign) "
	end;
	emit label;
	emit " accu in\n"
      ) info.symbol_arity;
      
      emit "        accu\n";

    ) lattice.lattice_symbol;
    emit "\n"
  ) signature

let emit_iter_leaves use_signs signature =
  emit (Printf.sprintf
    "  let %siter action %sterm =\n"
    (if use_signs then "s" else "")
    (if use_signs then "sign " else ""));
  emit "    match term with\n";
  List.iter (fun lattice ->
    Array.iteri (fun num info ->

      emit "    | ";
      emit_term_pattern (Some (fun label -> label)) num info;
      emit " ->\n        ";

      print_set
	(fun () ->
	  emit "()")
	(fun label ->
	  emit "action ";
	  if use_signs then begin
	    match Hashtbl.find lattice.lattice_label label with
	    | Covariant, _, _ ->
		emit "sign "
	    | Contravariant, _, _ ->
		emit "(not sign) "
	  end;
	  emit label)
	(fun () -> emit ";\n        ")
	info.symbol_arity;
      
      emit "\n"

    ) lattice.lattice_symbol;
    emit "\n"
  ) signature

let emit_iter_kind signature =
  emit "  let iter action term =\n";
  emit "    match term with\n";
  List.iter (fun lattice ->
    Array.iteri (fun num info ->

      emit "    | ";
      emit_term_pattern (Some (fun label -> label)) num info;
      emit " ->\n        ";

      print_set
	(fun () ->
	  emit "()")
	(fun label ->
	  emit "action ";
	  let _, _, kind = Hashtbl.find lattice.lattice_label label in
	  emit ("K" ^ kind ^ " ");
	  emit label)
	(fun () -> emit ";\n        ")
	info.symbol_arity;
      
      emit "\n"

    ) lattice.lattice_symbol;
    emit "\n"
  ) signature

(* Applying of function over leaves to a term. There are two variants of this function; a general-purpose one, and one
   which attempts to preserve sharing. *)

let emit_map_term signature =
  emit "  let map f = function\n";
  List.iter (fun lattice ->
    Array.iteri (fun num info ->

      emit "    | ";
      emit_term_pattern (Some (fun label -> label)) num info;
      emit " ->\n        ";
      emit_term_pattern (Some (fun label -> "f " ^ label)) num info;
      emit "\n"

    ) lattice.lattice_symbol
  ) signature;
  emit "\n"

let emit_endo_map_term signature =
  emit "  let endo_map f term =\n";
  emit "    match term with\n";
  List.iter (fun lattice ->
    Array.iteri (fun num info ->

      emit "    | ";
      emit_term_pattern (Some (fun label -> label)) num info;
      emit " ->\n";

      StringSet.iter (fun label ->
	emit "        let ";
	emit label;
	emit "' = f ";
	emit label;
	emit " in\n"
      ) info.symbol_arity;

      if not (StringSet.is_empty info.symbol_arity) then begin

	emit "        if ";
	print_set
	  (fun () -> ())
	  (fun label -> emit (Printf.sprintf "(%s' == %s)" label label))
	  (fun () -> emit " & ")
	  info.symbol_arity;
	emit " then\n";
	emit "          term\n";
	emit "        else\n  "

      end;

      emit "        ";
      emit_term_pattern (Some (fun label -> label ^ "'")) num info;
      emit "\n"

    ) lattice.lattice_symbol
  ) signature;
  emit "\n"

let emit_smap_term signature =
  emit "  let smap f sign = function\n";
  List.iter (fun lattice ->
    Array.iteri (fun num info ->

      emit "    | ";
      emit_term_pattern (Some (fun label -> label)) num info;
      emit " ->\n        ";
      emit_term_pattern (Some (fun label ->
	match Hashtbl.find lattice.lattice_label label with
	| Covariant, _, _ ->
	    "f sign " ^ label
	| Contravariant, _, _ ->
	    "f (not sign) " ^ label)) num info;
      emit "\n"

    ) lattice.lattice_symbol
  ) signature;
  emit "\n"

(* Computing a LUB or GLB of terms. *)

let emit_glub is_lub signature =
  let nameplus, nameminus = if is_lub then "lub", "glb" else "glb", "lub" in
  emit (Printf.sprintf
    "  let %s leaf_glb leaf_lub term1 term2 =\n"
  nameplus);
  emit "    match (term1, term2) with\n\n";
  List.iter (fun lattice ->
    Array.iteri (fun num1 info1 ->
      Array.iteri (fun num2 info2 ->

	(* Emit this pair's pattern, with names for the term's arguments. *)

	emit "    | (";
	emit_term_pattern (Some (fun label -> label ^ "1")) num1 info1;
	emit ", ";
	emit_term_pattern (Some (fun label -> label ^ "2")) num2 info2;
	emit ") ->\n        ";

	(* Deal with the special case where the symbols are ordered, one way or another, and the intersection of
	   their arities is empty. In such a case, we may prove sharing by returning one of the terms unchanged. *)

	let disjoint = StringSet.is_empty
	    (StringSet.inter info1.symbol_arity info2.symbol_arity) in
	let ordered12 = lattice.lattice_lattice.lattice_matrix.(num1).(num2)
	and ordered21 = lattice.lattice_lattice.lattice_matrix.(num2).(num1) in
	
	if disjoint & ordered12 then
	  emit (if is_lub then "term2" else "term1")
	else if disjoint & ordered21 then
	  emit (if is_lub then "term1" else "term2")
	else begin

	  (* Find out which symbol is the meet/join of these two. *)

	  let table =
	    if is_lub then lattice.lattice_lattice.lattice_lub
	    else lattice.lattice_lattice.lattice_glb in
	  let num = table.(num1).(num2) in
	  let info = lattice.lattice_symbol.(num) in

	  (* Emit a term. Thanks to the lattice's ``sensibility'' assumption, each label which appears in the result
	     symbol's arity must appear in at least one of the original symbols' arities. *)

	  let transform label =
	    match (StringSet.mem label info1.symbol_arity,
		   StringSet.mem label info2.symbol_arity) with
	    | true, true ->
		let name = match Hashtbl.find lattice.lattice_label label with
		| Covariant, _, _ ->
		    nameplus
		| Contravariant, _, _ ->
		    nameminus in
		Printf.sprintf "leaf_%s %s1 %s2" name label label
	    | true, false ->
		label ^ "1"
	    | false, true ->
		label ^ "2"
	    | false, false ->
		assert false in

	  emit_term_pattern (Some transform) num info

	end;
	emit "\n"
	
      ) lattice.lattice_symbol
    ) lattice.lattice_symbol;
    emit "\n"
  ) signature;
  emit_catch_kinding signature

(* Determining whether a term constructor can be lifted to a row constructor. *)

let label_sort lattice label =
  let _, sort, _ = Hashtbl.find lattice.lattice_label label in
  sort

let emit_expands signature =
  emit "  let expands = function\n";
  List.iter (fun lattice ->
    Array.iteri (fun num info ->
      emit "    | ";
      emit_term_pattern None num info;
      emit " ->\n        ";
      emit (
        if StringSet.is_empty info.symbol_arity then "false\n"
	else if List.for_all (fun label ->
	  label_sort lattice label = SortRegular
        ) (StringSet.elements info.symbol_arity) then "true\n"
	else "false\n"
      )
    ) lattice.lattice_symbol
  ) signature;
  emit "\n"

(* Enumerating leaves, together with their sort. *)

let emit_sort_iter signature =
  emit "  let iter action = function\n";
  List.iter (fun lattice ->
    Array.iteri (fun num info ->
      emit "    | ";
      emit_term_pattern (Some (fun label -> label)) num info;
      emit " ->\n        ";

      print_set
	(fun () ->
	  emit "()")
	(fun label ->
	  emit (Printf.sprintf "action %s %s"
		  (if label_sort lattice label = SortRegular then "false" else "true")
		  label))
	(fun () -> emit ";\n        ")
	info.symbol_arity;
      
      emit "\n"

    ) lattice.lattice_symbol;
    emit "\n"
  ) signature

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* \mysection{Dealing with printed appearance}

   Build a list of all tokens which appear in the signature. *)

let tokens lattices =
  List.fold_right (fun lattice tokens ->
    Array.fold_right (fun info tokens ->
      let term, symbol = info.symbol_appearance in
      List.fold_right (fun item tokens ->
	match item with
	| Token token ->
	    StringSet.add token tokens
	| _ ->
	    tokens
      ) term (StringSet.add symbol tokens)
    ) lattice.lattice_symbol tokens
  ) lattices StringSet.empty

(* Define the type of tokens. *)

let emit_token_declaration signature =
  emit "  type token =\n";
  StringSet.iter (fun token ->
    emit (Printf.sprintf "    | %s\n" (caml_token token))
  ) (tokens signature);
  emit "\n"

(* Emit the priority function. *)

let emit_priority signature =
  emit "  let priority = function\n";
  List.iter (fun lattice ->
    Array.iteri (fun num info ->
      emit "    | ";
      emit_term_pattern None num info;
      emit " ->\n        ";
      emit (Printf.sprintf "%d\n" info.symbol_priority)
    ) lattice.lattice_symbol
  ) signature;
  emit "\n"

(* This auxiliary function determines whether a label is surroundered by two tokens in an appearance list.
   If so, then this term is unambiguously delimited, a fact which we take into account when producing
   the pretty-printer and the grammar's productions. *)

let rec unambiguous label appearance =
  match appearance with
  | (Token _) :: (Label (label', _)) :: (Token _) :: _ when label = label' ->
      true
  | _ :: rest ->
      unambiguous label rest
  | [] ->
      false

(* Emit the printing functions. *)

let emit_term_printer signature =
  emit "  let term print_token is_row print_leaf term =\n";
  emit "    match term with\n";
  List.iter (fun lattice ->
    Array.iteri (fun num info ->

      let term_expands = List.for_all (fun label ->
	label_sort lattice label = SortRegular
      ) (StringSet.elements info.symbol_arity) in

      emit "    | ";
      emit_term_pattern (Some (fun label -> label)) num info;
      emit " ->\n        ";

      let appearance, _ = info.symbol_appearance in

      print_list
	(fun () ->
	  emit "()")
	(fun item ->
	  match item with
	  | Label (label, shield) ->
	      emit (Printf.sprintf
		      "print_leaf (fun term -> %s) %s %s"
		      (if unambiguous label appearance then "false"
		      else Printf.sprintf "priority term %s %d"
			  (if shield then ">=" else ">")
			  info.symbol_priority)
		      (if term_expands then "is_row"
		      else if label_sort lattice label = SortRegular then "false"
		      else "true")
		      label
                   )
	  | Token token ->
	      emit "print_token ";
	      emit (caml_token token)
        )
	(fun () -> emit ";\n        ")
	appearance;
      
      emit "\n"

    ) lattice.lattice_symbol;
    emit "\n"
  ) signature

let emit_symbol_printer signature =
  emit "  let symbol print_token = function\n";
  List.iter (fun lattice ->
    Array.iteri (fun num info ->

      emit "    | ";
      emit (caml_symbol info.symbol_name);
      emit " ->\n        ";
      emit "print_token ";
      emit (caml_token (snd info.symbol_appearance));
      emit "\n"

    ) lattice.lattice_symbol;
    emit "\n"
  ) signature

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Parsing} *)

let entry level =
  if level = -1 then "term_floor"
  else "term" ^ (string_of_int level)

let emit_grammar basename signature =

  (* Preamble. *)

  emit (Printf.sprintf
    "%%{\n\nopen %s.Term\nopen %s.Symbol\nopen %s.Abstract\n\n%%}\n\n"
  basename basename basename);
  emit "%start one_single_term\n";
  emit (Printf.sprintf
    "%%type <'a %s.Abstract.expression> one_single_term\n\n"
  basename);
  emit "%start one_single_coercion\n";
  emit (Printf.sprintf
    "%%type <'a %s.Abstract.coercion> one_single_coercion\n\n"
  basename);

  (* Define all tokens. Tokens whose name begins with "Spaced" are pseudo-tokens, used for pretty-printing only.
     As far as parsing is concerned, the "Spaced" prefix can be removed. *)

  let is_spaced token =
    let len = String.length token in
    if len < 6 then None
    else if String.sub token 0 6 = "Spaced" then
      Some (String.sub token 6 (len - 6))
    else
      None in

  let print_token token =
    match is_spaced token with
    | Some token ->
	token
    | None ->
	token in

  emit "%token If\n";
  emit "%token Colon\n";
  emit "%token EOF\n";
  emit "%token Less\n";
  emit "%token LParen\n";
  emit "%token RParen\n";
  emit "%token Then\n";
  emit "%token Quote\n";
  emit "%token Semi\n";
  emit "%token Urow\n";
  emit "%token <string> Ident\n\n";

  StringSet.iter (fun token ->
    match is_spaced token with
    | None ->
	emit (Printf.sprintf "%%token %s\n" token)
    | Some _ ->
	()
  ) (tokens signature);
  emit "\n";
  emit "%%\n\n";

  (* Define the floor entry. *)

  emit (entry (-1));
  emit ":\n";
  emit "    LParen row RParen\n    { $2 }\n";
  emit "  | Quote Ident\n    { { actual = Variable $2; info = None } }\n";
  emit ";\n";

  (* Define each term level. *)

  let max_priority = ref (-1) in

  List.iter (fun lattice ->
    Array.iteri (fun num info ->

      let level = info.symbol_priority in

      if level > !max_priority then
	max_priority := level;

      emit (entry level);
      emit ":\n    ";

      let indices = Hashtbl.create 149 in
      let index = ref 0 in
      
      let appearance, _ = info.symbol_appearance in
      print_list
	(fun () -> emit "/* epsilon */")
	(function
	  | Label (label, parenthesized) ->
	      incr index;
	      Hashtbl.add indices label !index;
	      emit (
	        if label_sort lattice label = SortRow then "row"
                else if unambiguous label appearance then "term"
		else entry (
                  if parenthesized then level - 1 else level
                )
              )
	  | Token token ->
	      incr index;
	      emit (print_token token))
	(fun () -> emit " ")
	appearance;
      emit "\n    { { actual = Term (";
      emit (caml_term info.symbol_name);
      if not (StringSet.is_empty info.symbol_arity) then begin
	emit " (";
	print_set
	  (fun () -> ())
	  (fun label ->
	    let index = Hashtbl.find indices label in
	    emit ("$" ^ (string_of_int index)))
	  (fun () -> emit ", ")
	  info.symbol_arity;
	emit ")"
      end;

      emit "); info = None } } \n  | ";
      emit (entry (level - 1));
      emit "\n    { $1 }\n;\n"

    ) lattice.lattice_symbol
  ) signature;

  (* Define symbols, for use in conditional constraints. *)

  emit "symbol:\n";
  List.iter (fun lattice ->
    Array.iteri (fun num info ->
      let _, symbol_token = info.symbol_appearance in
      emit (Printf.sprintf "  | %s\n    { %s }\n"
	      (print_token symbol_token)
	      (caml_symbol info.symbol_name))
    ) lattice.lattice_symbol
  ) signature;
  emit ";\n";

  (* Define rows. *)

  emit "row:\n";
  emit "    Ident Colon term Semi row\n";
  emit "    { { actual = RowExtension ($1, $3, $5); info = None } }\n";
  emit "  | Urow term_floor\n";
  emit "    { { actual = RowUniform $2; info = None } }\n";
  emit "  | term\n";
  emit "    { $1 }\n;\n";

  (* Define the top entries. We require an extra backquote in front of conditional constraints, otherwise the grammar
     wouldn't be $LL(k)$ for any $k$. *)

  emit "term:\n    ";
  emit (entry !max_priority);
  emit "\n    { $1 }\n;\n";

  emit "coercion:\n";
  emit "  | row Less row\n";
  emit "    { Coercion ($1, $3) }\n";
  emit "  | If symbol Less row Then row Less row\n";
  emit "    { Conditional($2, $4, $6, $8) }\n";
  emit ";\n";

  (* Define user-visible entries. *)

  emit "one_single_term:\n    ";
  emit "term EOF";
  emit "\n    { $1 }\n;\n";

  emit "one_single_coercion:\n    ";
  emit "coercion EOF";
  emit "\n    { $1 }\n;\n"

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Putting it all together} *)

let generate filename signature =

  let basename =
    if Filename.check_suffix filename ".grm"
    then Filename.chop_suffix filename ".grm"
    else filename in

  create_file (basename ^ ".ml");

  emit "(* This file was automatically generated by Gromit, *)\n";
  emit "(* the type algebra compiler. Do not edit.          *)\n\n";

  emit "module Term = struct\n\n";

  emit_term_declaration signature;
  emit_lattice_function MatchTerms "bottom" bottom_of_lattice signature;
  emit_lattice_function MatchTerms "top" top_of_lattice signature;
  emit_arity signature;
  emit_clash();
  emit_propagate false signature;
  emit_propagate true signature;
  emit_iter_leaves false signature;
  emit_fold_leaves false signature;
  emit_iter_leaves true signature;
  emit_fold_leaves true signature;
  emit_map_term signature;
  emit_endo_map_term signature;
  emit_smap_term signature;
  emit_glub false signature;
  emit_glub true signature;
  emit_expands signature;

  emit "end\n\n";
  emit "module Symbol = struct\n\n";

  emit_symbol_declaration signature;

  emit "  open Term\n\n";

  emit_head_of_term signature;
  emit_symbol_comparison signature;

  emit "end\n\n";
  emit "module Kind = struct\n\n";

  emit_kind_declaration signature;

  emit "  open Term\n";
  emit "  open Symbol\n\n";

  emit_kind_of_term signature;
  emit_kind_of_symbol signature;
  emit_lattice_function MatchKinds "bottom" bottom_of_lattice signature;
  emit_lattice_function MatchKinds "top" top_of_lattice signature;
  emit_iter_kind signature;

  emit "end\n\n";
  emit "module Sort = struct\n\n";

  emit "  open Term\n\n";

  emit_sort_iter signature;

  emit "end\n\n";
  emit "module Print = struct\n\n";

  emit_token_declaration signature;

  emit "  open Term\n\n";

  emit_priority signature;
  emit_term_printer signature;

  emit "  open Symbol\n\n";

  emit_symbol_printer signature;

  emit "end\n\n";
  emit "module Abstract = struct\n\n";

  emit_term_expression_decl signature;

  emit "end\n\n";

  close_file();

  create_file (basename ^ "_parser.mly");

  emit "/* This file was automatically generated by Gromit, */\n";
  emit "/* the type algebra compiler. Do not edit.          */\n\n";

  emit_grammar (String.capitalize basename) signature;

  close_file()

