(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/gromit/gromit.ml,v 1.10 2000/02/11 16:15:35 fpottier Exp $ *)

(*i*)

open Syntax
open Signature
open Ordering

(*i*)

(* This is the body of Gromit, the type algebra compiler. The purpose of this tool is to accept a type algebra
   definition, written in a human-readable way, and to output Objective Caml type definitions and auxiliary
   functions which allow working with this algebra. Such a tool allows writing a very flexible and maintainable
   program analysis tool, without paying the (admittedly slight) performance penalty associated with
   \emph{interpreting} a type algebra definition at run-time. *)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Reporting errors} *)

let filename =
  ref ""

let failure message =
  Printf.printf "%s: %s.\n" !filename message;
  flush stdout;
  exit(1)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Analyzing the user's input}
   
   After the user's input description has been parsed into an abstract syntax tree, we must convert it further to
   a nicer internal form. We also check its consistency in the process. The function [digest] takes an abstract
   syntax tree, containing a lattice definition, and produces a [Signature.lattice] object. *)

let digest lattice_names (Lattice(lattice_name, components)) =

  (* We begin by creating a table which lists all labels, together with their variance, their sort and their kind. We
     check, of course, that no label is multiply defined. *)

  let labels = Hashtbl.create 149 in
  List.iter (function
    | ComponentLabel (variance, sort, kind, label) -> (
	try
	  let _ = Hashtbl.find labels label in
	  failure (Printf.sprintf
		     "label %s is multiply defined in lattice %s"
		   label lattice_name)
	with Not_found ->
	  try
	    let () = Hashtbl.find lattice_names kind in
	    Hashtbl.add labels label (variance, sort, kind)
	  with Not_found ->
	    failure (Printf.sprintf
		       "%s (the kind of label %s) is undefined"
		     kind label)
      )
    | _ ->
	()
  ) components;

  (* Then, we create a table which lists all symbols, together with their arity, their appearance, and a unique
     number. We assign symbol numbers counting from 0 and up. Each symbol's arity is stored as a set of strings. *)

  let symbols = Hashtbl.create 149
  and symbol_count = ref 0 in

  List.iter (function
    | ComponentConstructor (symbol, arity, appearance) -> (
	try
	  let _ = Hashtbl.find symbols symbol in
	  failure (Printf.sprintf
	    "symbol %s is multiply defined in lattice %s"
          symbol lattice_name)
	with Not_found ->

	  let symbol_labels = ref StringSet.empty in

	  List.iter (fun label ->
	    try
	      let _ = Hashtbl.find labels label in

	      if StringSet.mem label !symbol_labels then
		failure (Printf.sprintf
		  "label %s appears twice in arity of symbol %s"
		label symbol);

	      symbol_labels := StringSet.add label !symbol_labels
	    with Not_found ->
	      failure (Printf.sprintf
		"label %s is undefined in arity of symbol %s"
	      label symbol)
          ) arity;

	  (* While we're here, make sure every label mentioned in the appearance description is indeed
	     defined for this constructor. *)

	  List.iter (fun item ->
	    match item with
	    | Label (label, _) ->
		if not (StringSet.mem label !symbol_labels) then
		  failure (Printf.sprintf
		    "label %s is undefined in appearance of symbol %s"
		    label symbol)
	    | _ ->
		()
          ) (fst appearance);

	  Hashtbl.add symbols symbol (!symbol_labels, appearance, !symbol_count);
	  incr symbol_count
        )
    | _ ->
	()
  ) components;

  (* We may now build a symbol information table. *)

  let symbol_table = Array.init !symbol_count (fun _ -> {
    symbol_name = "";
    symbol_arity = StringSet.empty;
    symbol_appearance = [], "";
    symbol_priority = -1
  }) in

  Hashtbl.iter (fun symbol (arity, appearance, num) ->
    let info = symbol_table.(num) in
    info.symbol_name <- symbol;
    info.symbol_arity <- arity;
    info.symbol_appearance <- appearance
  ) symbols;

  (* Then, we parse the ordering declarations. *)

  let m = Array.make_matrix !symbol_count !symbol_count false in
  List.iter (function
    | ComponentOrdering (symbol1, symbol2) -> (
	try
	  let _, _, num1 = Hashtbl.find symbols symbol1 in
	  try
	    let _, _, num2 = Hashtbl.find symbols symbol2 in
	    m.(num1).(num2) <- true;
	  with Not_found ->
	    failure (Printf.sprintf
		       "symbol %s is undefined in lattice %s"
		     symbol2 lattice_name)
	with Not_found ->
	  failure (Printf.sprintf
		     "symbol %s is undefined in lattice %s"
		   symbol1 lattice_name)
      )
    | _ ->
	()
  ) components;

  (* Next, we make sure these declarations make up a lattice. *)

  let o = {
    ordering_n = !symbol_count;
    ordering_matrix = m
  } in

  let lattice = try
    Ordering.ordering o;
    Ordering.lattice o
  with
  | Ordering.Cycle(num1, num2) ->
      failure (Printf.sprintf
	"symbols %s and %s form a loop in lattice %s"
	symbol_table.(num1).symbol_name
	symbol_table.(num2).symbol_name
        lattice_name)
  | Ordering.Empty ->
      failure (Printf.sprintf
	"lattice %s is empty"
	lattice_name)
  | Ordering.NoLUB(num1, num2) ->
      failure (Printf.sprintf
	"symbols %s and %s do not have a least upper bound in lattice %s"
	symbol_table.(num1).symbol_name
        symbol_table.(num2).symbol_name
        lattice_name)
  | Ordering.NoGLB(num1, num2) ->
      failure (Printf.sprintf
	"symbols %s and %s do not have a greatest lower bound in lattice %s"
	symbol_table.(num1).symbol_name
        symbol_table.(num2).symbol_name
        lattice_name) in

  (* We're now able to produce a [Signature.lattice] structure. *)

  {
    lattice_name = lattice_name;
    lattice_count = !symbol_count;
    lattice_symbol = symbol_table;
    lattice_label = labels;
    lattice_lattice = lattice
  } 

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* The function [consistent] accepts a [Signature.lattice] structure, as produced by the above code. First, it makes
   sure its arity function $a$ is ``pointwise convex'', i.e.
   \newcommand{\ground}{{\mathcal{S}}}
   \newcommand{\groundleq}{\leq_\ground}
   \def\latticeunion{\sqcup}
   \def\latticeinter{\sqcap}
   \newcommand{\groundbot}{\bot_\ground}
   \newcommand{\groundtop}{\top_\ground}
   \newcommand{\groundlatticeunion}{\latticeunion_\ground}
   \newcommand{\groundlatticeinter}{\latticeinter_\ground}
   $$\forall r, s, t\in\ground \quad r\groundleq s\groundleq t \Rightarrow a(s)\supseteq a(r)\cap a(t)$$
   Indeed, this condition is necessary to ensure that this signature defines an ordering on ground terms.
   Then, it makes sure that it is also ``sensible'', i.e.
   \newcommand{\symset}{S}
   \begin{align*}
   \forall\symset\subseteq\ground\quad a(\groundlatticeunion\symset) & \subseteq\cup\, a(\symset) \\
   \forall\symset\subseteq\ground\quad a(\groundlatticeinter\symset) & \subseteq\cup\, a(\symset)
   \end{align*}
   %
   (Note that this implies $a(\groundbot)=a(\groundtop)=\varnothing$.) Indeed, this condition is necessary
   to allow enforcing the ``small terms invariant''. *)

let consistent lattice =

  (* Check for pointwise convexity. We use a naïve algorithm, which runs in time $O(n^3)$. A quicker test might
     be possible, but definitely not worth the trouble. *)

  Array.iteri (fun r info_r ->
    Array.iteri (fun t info_t ->

      let arity_r = info_r.symbol_arity
      and arity_t = info_t.symbol_arity in

      if (r <> t) & lattice.lattice_lattice.lattice_matrix.(r).(t) then

	let common_rt = StringSet.inter arity_r arity_t in
	Array.iteri (fun s info_s ->

	  let arity_s = info_s.symbol_arity in

	  if (r <> s) & lattice.lattice_lattice.lattice_matrix.(r).(s)
	   & (s <> t) & lattice.lattice_lattice.lattice_matrix.(s).(t) then

	    if not (StringSet.subset common_rt arity_s) then
	      let line1 = Printf.sprintf
		  "lattice %s does not enjoy ``pointwise convexity''.\n"
		  lattice.lattice_name in
	      let line2 = Printf.sprintf
		  "Labels carried by %s and %s should also be carried by %s"
		  info_r.symbol_name
		  info_t.symbol_name
		  info_s.symbol_name in
	      failure (line1 ^ line2)

        ) lattice.lattice_symbol

    ) lattice.lattice_symbol
  ) lattice.lattice_symbol;

  (* Then, check for ``sensibility''. We begin by making sure that $\bot$ and $\top$ have arity $\varnothing$. *)

  let line1 = Printf.sprintf
      "lattice %s does not enjoy ``sensibility''.\n"
      lattice.lattice_name in

  let bot_num = lattice.lattice_lattice.lattice_bot in
  let bot_sym = lattice.lattice_symbol.(bot_num) in
  let bot_name = bot_sym.symbol_name in

  if not (StringSet.is_empty bot_sym.symbol_arity) then
    let line2 = Printf.sprintf
	"Its bottom element, %s, should have empty arity"
	bot_name in
    failure (line1 ^ line2);

  let top_num = lattice.lattice_lattice.lattice_top in
  let top_sym = lattice.lattice_symbol.(top_num) in
  let top_name = top_sym.symbol_name in
  
  if not (StringSet.is_empty top_sym.symbol_arity) then
    let line2 = Printf.sprintf
	"Its top element, %s, should have empty arity"
	top_name in
    failure (line1 ^ line2);

  (* We continue by making sure that the arity of a binary meet (or join) is less than, or equal to, the union of
     the arities. *)

  let line3 =
    "so its arity should be a subset of the union of their arities" in

  Array.iteri (fun r info_r ->
    Array.iteri (fun t info_t ->

      let arity_r = info_r.symbol_arity
      and arity_t = info_t.symbol_arity in
      let union_rt = StringSet.union arity_r arity_t in

      let meet_num = lattice.lattice_lattice.lattice_glb.(r).(t) in
      let meet_sym = lattice.lattice_symbol.(meet_num) in
      if not (StringSet.subset meet_sym.symbol_arity union_rt) then
	let line2 = Printf.sprintf
	    "%s is the meet of %s and %s,\n"
	    meet_sym.symbol_name
            info_r.symbol_name
            info_t.symbol_name in
	failure (line1 ^ line2 ^ line3);

      let join_num = lattice.lattice_lattice.lattice_lub.(r).(t) in
      let join_sym = lattice.lattice_symbol.(join_num) in
      if not (StringSet.subset join_sym.symbol_arity union_rt) then
	let line2 = Printf.sprintf
	    "%s is the join of %s and %s,\n"
	    join_sym.symbol_name
            info_r.symbol_name
            info_t.symbol_name in
	failure (line1 ^ line2 ^ line3)

    ) lattice.lattice_symbol
  ) lattice.lattice_symbol

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Dealing with priorities}
  
   Given the priority list, this function assigns a numeric priority level to each constructor. *)

let do_priorities lattices plist =

  (* First, create a global map which associates each constructor name with its information record. *)

  let ctors = Hashtbl.create 149 in

  List.iter (fun lattice ->
    Array.iter (fun info ->
      let name = info.symbol_name in
      try
	let _ = Hashtbl.find ctors name in
	failure (Printf.sprintf
		   "constructor %s is multiply defined"
		   name)
      with Not_found ->
	Hashtbl.add ctors name info
    ) lattice.lattice_symbol
  ) lattices;

  (* Then, walk the priority list and assign numbers. *)

  let _ = List.fold_left (fun index name ->
    let info = Hashtbl.find ctors name in
    info.symbol_priority <- index;
    index + 1
  ) 0 plist in

  (* Lastly, make sure all symbols appear in the priority list. *)

  Hashtbl.iter (fun name info ->
    if info.symbol_priority = -1 then
      failure (Printf.sprintf
		 "constructor %s does not appear in the priority list"
		 info.symbol_name)
  ) ctors

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{The program's entry point} *)

(* The function [compile] is called once per file specified on the command line. The global variable [filename]
   contains the file's name. *)

let compile () =
  let channel = open_in !filename in
  let lexbuf = Lexing.from_channel channel in
  try

    (* We parse the file, which yields an abstract syntax tree. We further process this tree, so as to obtain
       a list of [Signature.lattice] structures, and check their consistency. Then, we pass them to the code
       generator. *)

    let lattices, plist = Parser.signature Lexer.token lexbuf in

    let lattice_names = Hashtbl.create 149 in
    List.iter (function Lattice(name, _) ->
      try
	let () = Hashtbl.find lattice_names name in
	failure (Printf.sprintf "lattice %s is multiply defined" name)
      with Not_found ->
	Hashtbl.add lattice_names name ()
    ) lattices;

    let lattices = List.map (fun lattice ->
      let lattice = digest lattice_names lattice in
      consistent lattice;
      lattice
    ) lattices in
    do_priorities lattices plist;
    Generator.generate !filename lattices

  with
  | Lexer.Error (error, start_loc, end_loc) ->
      failure (Printf.sprintf
	"%s at characters %d-%d"
	(Lexer.report_error error)
        start_loc
        end_loc)
  | SyntaxError message ->
      failure message

let _ =
  Arg.parse [] (fun name ->
    filename := name;
    compile()
  ) "Gromit, the type algebra compiler."

