open Printf
open Syntax
open Strings
open Front
open Kind

(* ---------------------------------------------------------------------------- *)
(* Open the two output channels. *)

(* The implementation file. *)

let ml = open_out (basename ^ ".ml")

(* The interface file. *)

let mli = open_out (basename ^ ".mli")

(* Switching between the two output channels. Dirty, but convenient. *)

let output =
  ref ml

let printf format =
  fprintf !output format

(* ---------------------------------------------------------------------------- *)

(* A single nominal type is turned into three O'Caml types: (i) a raw
   type, with transparent atoms and abstractions; (ii) an internal
   type, with opaque atoms and abstractions; (iii) a flat internal
   type, with opaque atoms and transparent abstractions. *)

type variety =
  | VRaw
  | VInternal
  | VFlat

let has_opaque_atoms = function
  | VRaw ->
      false
  | VFlat
  | VInternal ->
      true

let has_opaque_abstractions = function
  | VRaw
  | VFlat ->
      false
  | VInternal ->
      true

let prefix = function
  | VRaw ->
      "Raw."
  | VFlat ->
      "Flat."
  | VInternal ->
      ""

let explain = function
  | VRaw ->
      "raw"
  | VFlat ->
      "flat internal"
  | VInternal ->
      "internal"

(* ---------------------------------------------------------------------------- *)
(* Auxiliary functions for code generation. *)

let nuple =
  StringAux.show_list "()" "(" ", " ")"    (* for regular tuples *)

let duple =
  StringAux.show_list "" " (" ", " ")"     (* for the arguments of a data constructor *)

let ruple =
  StringAux.show_list "{}" "{ " "; " " }"  (* for records *)

let attach variety label separator t =
  match label with
  | None ->
      t
  | Some (_, _, label) ->
      prefix variety ^ label ^ separator ^ t

(* ---------------------------------------------------------------------------- *)
(* Classes that help generate code. *)

type 'factor pf =
    string * locid option * 'factor

class virtual basic_generator (impl : bool) = object(self)

  (* The flag [impl] tells whether we are generating an implementation
     or interface file. *)

  (* These methods are in charge of generating an entire function
     or method. *)
 
  method virtual separator: string         (* a keyword that should be printed first *)
  method virtual indentation: string       (* some amount of whitespace *)
  method virtual description: string       (* "Function:" or "Method:" *)
  method virtual name: string              (* the name of the function or method *)
  method virtual header: string            (* its type *)
  method virtual formals: string           (* its formal parameters besides the tree that is examined *)
  method virtual summary: string           (* a summary of the function's semantics *)
  method virtual in_comments: string list  (* a description of the input parameters *)
  method virtual out_comments: string list (* a description of the output parameters *)
  method virtual rhs: unit                 (* generates code for the right-hand side *)
 
  method body =

    (* Generate a comment. *)
    
    if not impl then begin

      let p =
	StringAux.show_list "" "" ("\n" ^ self#indentation ^ " *             ") "" in
      printf "
%s(*
%s * %s   %s
%s * Summary:    %s
%s * Parameters: %s
%s * Results:    %s
%s *)
"
	self#indentation
	self#indentation self#description self#name
	self#indentation self#summary
	self#indentation (p self#in_comments)
	self#indentation (p self#out_comments)
	self#indentation

    end;

    (* Generate the header. *)

    printf "\n%s%s %s%s%s"
      self#indentation self#separator self#name (if self#header = "" then "" else " : ") self#header;

    (* Generate the right-hand side of the definition. *)

    if impl then begin
      printf " = ";
      let formals = self#formals in
      if formals <> "" then
	printf "fun %s -> " formals;
      self#rhs
    end
    else
      printf "\n"

end

class notexported = object

  method header =
    ""

  method summary : string =
    assert false

  method in_comments : string list =
    assert false

  method out_comments : string list =
    assert false

end

class virtual ['factor] generator impl variety summands = object(self)

  (* The type variable ['factor] tells whether we are dealing with
     expressions or patterns. The [variety] parameter tells what kind
     of data we are *examining*. *)

  inherit basic_generator impl

  (* The method [pat] chooses a name for the variable that shall be
     bound to a factor. Its parameters are an integer counter
     (incremented with every factor) and the factor itself.

     We could make things simpler by choosing arbitrary distinct names
     for these variables. Instead, we use a basename that is
     reminiscent of the factor's nature, for clarity. *)

  method virtual pat: int -> 'factor -> string

  method pats factors =
    let _, ps, pfs = List.fold_right (fun (label, factor) (i, ps, pfs) ->
      let x = self#pat i factor in
      i+1, (attach variety label " = " x) :: ps, (x, label, factor) :: pfs
    ) factors (0, [], []) in
    ps, pfs

  (* These methods are in charge of generating right-hand side code
     for an individual summand. *)

  method virtual record: 'factor pf list -> unit

  method virtual tuple: 'factor pf list -> unit

  method virtual data: string -> 'factor pf list -> unit

  (* This method generates code for an individual summand. We choose
     names for the bound variables, generate the left-hand side that
     binds these names, and call one of the above methods to finish
     the job. *)

  method summand = function
    | Summand (dataid, factors) ->
	let ps, pfs = self#pats factors in
	match dataid, factors with
	| None, (Some _, _) :: _ ->
	    (* This is a record type. *)
	    printf "  %s ->\n" (ruple ps);
	    self#record pfs
	| None, _ ->
	    (* This is a tuple type. *)
	    printf "  %s ->\n" (nuple ps);
	    self#tuple pfs
	| Some (_, _, data), _ ->
	    (* This is a sum type. *)
	    printf "  | %s%s%s ->\n" (prefix variety) data (duple ps);
	    self#data data pfs

  method rhs =
    printf "function\n";
    List.iter self#summand summands

end

class virtual ['factor] map_generator impl variety summands = object(self)

  inherit ['factor] generator impl variety summands

  method virtual factor : 'factor pf -> string

  method record_factor =
    self#factor

  method record pfs =
    printf "    %s\n" (ruple (List.map self#record_factor pfs))

  method tuple_factor =
    self#factor

  method tuple pfs =
    printf "    %s\n" (nuple (List.map self#tuple_factor pfs))

  method data_factor =
   self#factor

  method virtual prefix : string -> string

  method data data pfs =
   printf "      %s%s\n" (self#prefix data) (duple (List.map self#data_factor pfs))

end

class virtual ['factor] fold_generator impl variety summands = object(self)

  inherit ['factor] generator impl variety summands

  method virtual factor: 'factor pf -> unit

  method record_factor =
    self#factor

  method tuple_factor =
    self#factor

  method data_factor =
    self#factor

  method trailer =
    printf "      %s\n" self#formals

  method record pfs =
    List.iter self#record_factor pfs;
    self#trailer

  method tuple pfs =
    List.iter self#tuple_factor pfs;
    self#trailer

  method data data pfs =
    List.iter self#data_factor pfs;
    self#trailer

end

class virtual ['factor] fold2_generator impl variety summands = object(self)

  inherit basic_generator impl

  method virtual pat: int -> 'factor -> string

  method virtual factor2: 'factor pf -> 'factor pf -> unit

  method record_factor2 =
    self#factor2

  method tuple_factor2 =
    self#factor2

  method data_factor2 =
    self#factor2

  method trailer =
    printf "      %s\n" self#formals

  method record2 pfs1 pfs2 =
    List.iter2 self#record_factor2 pfs1 pfs2;
    self#trailer

  method tuple2 pfs1 pfs2 =
    List.iter2 self#tuple_factor2 pfs1 pfs2;
    self#trailer

  method data2 data pfs1 pfs2 =
    List.iter2 self#data_factor2 pfs1 pfs2;
    self#trailer

  method pats2 factors i =
    List.fold_right (fun (label, factor) (i, ps, pfs) ->
      let x = self#pat i factor in
      i+1, (attach variety label " = " x) :: ps, (x, label, factor) :: pfs
    ) factors (i, [], []) 

  method summand = function
    | Summand (dataid, factors) ->
	let i, ps1, pfs1 = self#pats2 factors 0 in
	let _, ps2, pfs2 = self#pats2 factors i in
	match dataid, factors with
	| None, (Some _, _) :: _ ->
	    (* This is a record type. *)
	    printf "  | %s, %s ->\n" (ruple ps1) (ruple ps2);
	    self#record2 pfs1 pfs2

	| None, _ ->
	    (* This is a tuple type. *)
	    printf "  | %s, %s ->\n" (nuple ps1) (nuple ps2);
	    self#tuple2 pfs1 pfs2

	| Some (_, _, data), _ ->
	    (* This is a sum type. *)
	    printf "  | %s%s%s, %s%s%s ->\n" 
	      (prefix variety) data (duple ps1)
	      (prefix variety) data (duple ps2);
	    self#data2 data pfs1 pfs2

  method rhs =
    let unlikely = "unlikely__arg" in
    printf "\n  fun %s1 %s2 ->\n  match %s1, %s2 with\n"
      unlikely unlikely unlikely unlikely;
    List.iter self#summand summands;
    if List.length summands > 1 then
      printf "  | _, _ ->\n      raise (Invalid_argument \"%s\")\n" self#name

end

(* Choosing names for the bound variables. We could choose arbitrary
   distinct names, but we use a basename that is reminiscent of the
   type for clarity. *)

class exppat = object

  method pat i = function
    | EAtom (_, _, sort) ->
	sprintf "_%s%d" sort i
    | EEscape _ ->
	sprintf "_x%d" i
    | EAbstraction (_, (_, _, t)) ->
	sprintf "_%s%d" t i
    | ETypRef (container, _, (_, _, t)) ->
	sprintf "_%s%s%d" t (Container.suffix container) i

end

class patpat = object

  method pat i = function
    | PAtom (_, _, sort) ->
	sprintf "_%s%d" sort i
    | PEscape _ ->
	sprintf "_x%d" i
    | PTypRef (_, container, _, (_, _, t)) ->
	sprintf "_%s%s%d" t (Container.suffix container) i

end

(* Implementations of the [prefix] method. *)

class prefix variety = object

  method prefix data =
    prefix variety ^ data

end

(* Implementations of the [separator], [description], and [indentation] methods. *)

class function_separator (impl : bool) = object

  method separator =
    if impl then "and" else "val"

  method description =
    "Function:"

  method indentation =
    ""

end

class method_separator = object

  method separator =
    "method"

  method description =
    "Method:  "

  method indentation =
    "  "
end

(* ---------------------------------------------------------------------------- *)
(* Lots of naming conventions and printing helpers. *)

(* Name of the [Atom] module for a given sort. *)

let atom sort =
  String.capitalize sort

(* Name of the opaque abstraction type and of its fields and operations. *)

let abstraction_type t =
  "opaque_" ^ t

let delayed_field t = (* also applied to sorts *)
  t ^ "_delayed"

let idelayed_field i t =
  sprintf "%s%d" (delayed_field t) i

let body_field t =
  t

let create t =
  "create_" ^ t

let apply t =
  "apply_" ^ t

let openup t =
  "open_" ^ t

let freshen2 t =
  "freshen2_" ^ t

let openup2 t =
  "open2_" ^ t

let openup2i t =
  "open2i_" ^ t

let aeq t =
  "aeq_" ^ t

let equality t =
  "equal_" ^ t

(* Names for substitution parameters. *)

let inner_or_outer f1 f2 sorts sort =
  if StringSet.mem sort sorts then
    f1 sort
  else
    f2 sort

let m sort =
  sort ^ "_m"

let m_type sort =
  atom sort ^ ".AtomIdMap.t"

let inner_m sort =
  sort ^ "_im"

let outer_m sort =
  sort ^ "_om"

let inner_or_outer_m =
  inner_or_outer inner_m outer_m

let bvar sort =
  sort ^ "_bvars"

let fvar sort =
  sort ^ "_fvars"

let inner_fvar sort =
  sort ^ "_ifvars"

let outer_fvar sort =
  sort ^ "_ofvars"

let inner_or_outer_fvar =
  inner_or_outer inner_fvar outer_fvar

let subst sort =
  sort ^ "_env"

let inner_subst sort =
  sort ^ "_ienv"

let outer_subst sort =
  sort ^ "_oenv"

let inner_or_outer_subst =
  inner_or_outer inner_subst outer_subst

(* Same thing for tuples of substitution parameters. *)

let show_set empty opening separator closing f sorts =
  StringAux.show_list empty opening separator closing (List.map f (StringSet.elements sorts))

let tuple =
  show_set "()" "(" ", " ")"

let tuple_type =
  show_set "unit" "" " * " ""

let ms =
  tuple m

let ms_type =
  tuple_type m_type

let inner_ms =
  tuple inner_m

let outer_ms =
  tuple outer_m

let inner_or_outer_ms bound =
  tuple (inner_or_outer_m bound)

let bvars =
  tuple bvar

let fvars =
  tuple fvar

let inner_or_outer_fvars bound =
  tuple (inner_or_outer_fvar bound)

let substs =
  tuple subst

let inner_substs =
  tuple inner_subst

let outer_substs =
  tuple outer_subst

let inner_or_outer_substs bound =
  tuple (inner_or_outer_subst bound)

(* Types. *)

let subst_type sort =
  atom sort ^ ".Subst.t"

let subst_comment sort =
  sprintf "a substitution of atoms for atoms at sort %s" sort

let substs_type =
  tuple_type subst_type

let cuple1 f1 s1 =
  List.map f1 (StringSet.elements s1)

let substs_comment =
  cuple1 subst_comment

(* Empty sets of atoms. *)

let noatom sort =
  sprintf "%s.AtomSet.empty" (atom sort)

let noatoms =
  tuple noatom

(* A tuple of identity substitutions. *)

let nosubst sort =
  atom sort ^ ".Subst.id"

let subst_id =
  tuple nosubst

(* A condition that identifies a tuple of identity substitutions. *)

let is_subst_id field =
  show_set "false" "(" " && " ")" (fun sort ->
    atom sort ^ ".Subst.is_id " ^ (field sort)
  )

(* A tuple of delayed substitutions. *)

let delayeds =
  tuple delayed_field

let idelayeds i =
  tuple (idelayed_field i)

(* Create a single tuple out of two sets of atom sorts. Used to
   define parameters for the generated function [substitute] and
   [export] over patterns. *)

let tuple2 f1 s1 f2 s2 =
  nuple (
    (List.map f1 (StringSet.elements s1)) @
    (List.map f2 (StringSet.elements s2))
  )

(* Create a single tuple out of three sets of atom sorts. Used to
   define an accumulator for the generated function [bvfvaccu]. *)

let tuple3 f1 s1 f2 s2 f3 s3 =
  nuple (
    (List.map f1 (StringSet.elements s1)) @
    (List.map f2 (StringSet.elements s2)) @
    (List.map f3 (StringSet.elements s3))
  )

let bifofvars bound innerlive outerlive =
  tuple3 bvar bound inner_fvar innerlive outer_fvar outerlive

let biffvars bound innerlive outerlive =
  tuple3 bvar bound inner_fvar innerlive fvar outerlive

let nonofvars bound innerlive outerlive =
  tuple3 noatom bound noatom innerlive fvar outerlive

let nonono bound innerlive outerlive =
  tuple3 noatom bound noatom innerlive noatom outerlive

let m_comment sort =
  sprintf "a mapping of atoms of sort %s to identifiers" sort

let ms_comment =
  cuple1 m_comment

let import_subst_type sort =
  sort ^ " Identifier.Map.t"

let import_subst_comment sort =
  sprintf "a mapping of identifiers to atoms of sort %s" sort

let import_substs_comment =
  cuple1 import_subst_comment

let import_substs_type =
  tuple_type import_subst_type

let var_type sort =
  atom sort ^ ".AtomSet.t"

let vars_type =
  tuple_type var_type

let tuple_type2 f s1 s2 =
  StringAux.show_list "unit" "" " * " "" (
    (List.map f (StringSet.elements s1)) @
    (List.map f (StringSet.elements s2))
  )

let substs_type2 =
  tuple_type2 subst_type

let import_substs_type2 =
  tuple_type2 import_subst_type

let ms_type2 =
  tuple_type2 m_type

let import_substs_type2 =
  tuple_type2 import_subst_type

let vars_type3 s1 s2 s3 =
  StringAux.show_list "unit" "" " * " "" (
    (List.map var_type (StringSet.elements s1)) @
    (List.map var_type (StringSet.elements s2)) @
    (List.map var_type (StringSet.elements s3))
  )

(* Names for the operations that are available at every type. *)

let export t =
  "export_" ^ t

let flatten t =
  "flatten_" ^ t

let unflatten t =
  "unflatten_" ^ t

let fv t =
  "free_" ^ t

let fvaccu t =
  "free_accu_" ^ t

let bvfv t =
  "bound_free_" ^ t

let bvfvaccu t =
  "bound_free_accu_" ^ t

let inner_or_outer_live t =
  StringSet.union (getouterlive t) (getinnerlive t)

(* Comments. *)

let free_var_comment sort =
  sprintf "the set of all atoms of sort %s that occur free in the input expression" sort

let free_vars_comment =
  cuple1 free_var_comment

let bound_var_comment sort =
  sprintf "the set of all atoms of sort %s that occur in a binding position in the input pattern" sort

let bound_vars_comment =
  cuple1 bound_var_comment

let outer_subst_comment t sort =
  let binds = getbinds t in
  [ sprintf "a substitution of atoms for atoms at sort %s" sort;
    if StringSet.mem sort binds then
      "  (to be applied to sub-expressions in outer scope)"
    else
      "  (to be applied to sub-expressions in outer, neutral, or inner scope)" ]

let inner_subst_comment sort =
  [ sprintf "a substitution of atoms for atoms at sort %s" sort;
      "  (to be applied to atoms in binding position and to sub-expressions in inner scope)" ]

let comments f sorts =
  List.flatten (cuple1 f sorts)

let outer_substs_comment t =
  comments (outer_subst_comment t)

let inner_substs_comment =
  comments inner_subst_comment

let free_inner_var_comment sort =
  [ sprintf "the set of all atoms of sort %s that occur free in a" sort;
            "  sub-expression that lies in inner scope in the input pattern " ]

let free_inner_vars_comment =
  comments free_inner_var_comment

let free_outer_var_comment t sort =
  let binds = getbinds t in
  [ sprintf "the set of all atoms of sort %s that occur free in a" sort;
    if StringSet.mem sort binds then
      "  sub-expression that lies in outer scope in the input pattern "
    else
      "  sub-expression that lies in outer, neutral, or inner scope in the input pattern " ]

let free_outer_vars_comment t =
  comments (free_outer_var_comment t)

(* ---------------------------------------------------------------------------- *)
(* Turning nominal type declarations into O'Caml type declarations. *)

let strip params =
  List.map (function (_, _, x) -> x) params

let ido2string = function
  | None ->
      assert false
  | Some (_, _, x) ->
      x

class param_translation = object(self)

  method param v =
    "'" ^ v

  method params params =
    match params with
    | [ param ] -> (* special case to avoid superfluous parentheses *)
	(self#param param) ^ " "
    | _ ->
	StringAux.show_list "" "(" ", " ") " (List.map self#param params)

end

class virtual ['factor] type_translation = object(self)

  inherit param_translation

  method virtual nude_factor: 'factor -> string

  method factor (label, factor) =
    attach VInternal label ": " (self#nude_factor factor)

  method summands summands =
    match summands with
    | [ Summand (None, ((Some _, _) :: _ as factors)) ] ->
	(* This is a record type. *)
	printf "%s" (StringAux.show_list "" "  { " "; " " }\n" (List.map self#factor factors))
    | [ Summand (None, ((None, _) :: _ as factors)) ] ->
	(* This is a tuple type. *)
	printf "%s" (StringAux.show_list "" "  " " * " "\n" (List.map self#factor factors))
    | _ ->
	(* This is a sum type. *)
	List.iter (function
	  | Summand (dataid, factors) ->
	      printf "  | %s%s\n" (ido2string dataid)
		(StringAux.show_list "" " of " " * " "" (List.map self#factor factors))
	) summands

end

class exp_type_translation variety = object(self)

  inherit [ expfactor ] type_translation

  method nude_factor = function
    | EAtom (_, _, sort) ->
	sort
    | EEscape st ->
	sprintf "(%s)" st
    | ETypRef (container, params, (_, _, t)) ->
	let params = self#params (strip params) in
	sprintf "%s%s%s" params t (Container.print container)
    | EAbstraction (params, (_, _, t)) ->
	let params = self#params (strip params) in
	sprintf "%s%s" params (if has_opaque_abstractions variety then abstraction_type t else t)

end

class pat_type_translation = object(self)

  inherit [ patfactor ] type_translation

  method nude_factor = function
    | PAtom (_, _, sort) ->
	sort
    | PEscape st ->
	sprintf "(%s)" st
    | PTypRef (modifier, container, params, (_, _, t)) ->
	let params = self#params (strip params) in
	sprintf "%s%s%s" params t (Container.print container)

end

let translate_type_declarations impl variety =
  let element, finished = StringAux.new_list_manager !output "" "type " "\n and " "" in
  List.iter (function
    | DeclExpType (params, (_, _, t), summands) ->
	element();
	let translate = new exp_type_translation variety in
	let params = translate#params (strip params) in
	printf "%s%s = \n" params t;
	translate#summands summands
    | DeclPatType (params, (_, _, t), _, summands) ->
	element();
	let translate = new pat_type_translation in
	let params = translate#params (strip params) in
	printf "%s%s = \n" params t;
	translate#summands summands;
	if (has_opaque_abstractions variety) && (StringSet.mem t abstractions) then begin
	  element();
	  printf "%s%s" params (abstraction_type t);
	  if impl then begin

	    (* Generate declarations for the abstraction types. Each such type
	       consists of a tuple of delayed substitutions and a body. Both
	       fields are mutable in order to allow memoization. *)

	    let live = inner_or_outer_live t in
	    printf " = {\n";
	    printf "    mutable %s: %s;\n" (delayed_field t) (substs_type live);
	    printf "    mutable %s: %s%s\n" (body_field t) params t;
	    printf "  }"

	  end;
	  printf "\n"
	end
    | DeclSort (_, _, sort) ->
	element();
	if has_opaque_atoms variety then
	  printf "%s =\n  %s.Atom.t\n" sort (atom sort)
	else
	  printf "%s =\n  Identifier.t\n" sort
    | DeclContainer _ 
    | DeclIdentifier _ ->
	()
  ) declarations;
  finished()

let translate_sort_declarations () =
  StringSet.iter (fun sort ->
    printf "module %s = AlphaLib.Atom.Make(Identifier)\n\n" (atom sort)
  ) sorts

(* ---------------------------------------------------------------------------- *)
(* Generating atom substitution code. *)

(* This auxiliary function tells which sorts of atoms are bound by the
   pattern type [t] inside an inner, outer, or neutral hole. *)

let bound_in_hole t = function
  | MRef ->
      assert false
  | MInner ->
      (* All atom sorts that are declared to be bound by the current
	 pattern type are considered bound inside an inner hole. *)
      getbinds t
  | MOuter
  | MNeutral ->
      (* No atoms are bound by the current abstraction inside an outer
	 or neutral hole. *)
      StringSet.empty 

class virtual substitute_expr impl variety summands = object(self)

  inherit exppat
  inherit [ expfactor ] map_generator impl variety summands
  inherit function_separator impl
  inherit prefix VInternal

  (* Name of the function that should be used for applying
     a substitution to an atom, parameterized by an atom sort. *)

  method virtual apply_atom: string -> string

  (* Name of the (generated) function that substitutes through an
     data structure, parameterized by a type name. *)

  method virtual substitute: string -> string

  (* How to generate code for applying a substitution to an
     abstraction. The parameters are [x], the name of the
     abstraction value, and [t], its type. The function can
     generate bindings via [printf] and should return the
     final result as a string. *)

  method virtual apply_abstraction: string -> string -> string

  (* Generate code for a function that applies a substitution to an
     expression factor. The function expects one substitution
     parameter for every atom sort that is live inside the current
     type. *)

  method factor ((x, label, factor) : expfactor pf) =
    attach VInternal label " = " (
      match factor with
      | EAtom (_, _, sort) ->
	  sprintf "%s %s %s" (self#apply_atom sort) x (subst sort)
      | EEscape _ ->
	  x
      | ETypRef (container, _, (_, _, t)) ->
	  let live = getlive t in
	  sprintf "%s(%s %s) %s" (Container.map container) (self#substitute t) (substs live) x
      | EAbstraction (_, (_, _, t)) ->
	  self#apply_abstraction x t
    )

end

class virtual bvaccu_pat impl variety summands = object(self)

  inherit patpat
  inherit [ patfactor ] fold_generator impl variety summands
  inherit function_separator impl

  (* How to generate code for extending the current set of bound
     atoms with a newly discovered one. The parameters are [sort],
     the sort of the bound atom, and [x], the bound atom. The
     function must generate a binding for the appropriate accumulator
     via [printf]. *)

  method virtual extend: string -> string -> unit

  (* Name of the (generated) function that returns a pattern's
     bound variables, parameterized by a type name. *)

  method virtual bvaccu: string -> string

  (* Generate code for a function that examines a pattern factor and
     returns sets of its bound variables. Accumulators are used to
     hold the sets as they are built. The function expects and returns
     one accumulator for every atom sort that is bound live inside the
     current type. *)

  method factor (x, _, factor) =
    match factor with
    | PAtom (_, _, sort) ->
	self#extend sort x
    | PTypRef (MRef, container, _, (_, _, t)) ->
	let boundlive = getboundlive t in
	let accu = bvars boundlive in
	printf "      let %s = %s%s %s %s in\n" accu (Container.fold container) (self#bvaccu t) accu x
    | PTypRef ((MInner | MOuter | MNeutral), _, _, _)
    | PEscape _ ->
	()

end

class virtual substitute_pat impl variety ty summands = object(self)

  inherit patpat
  inherit [ patfactor ] map_generator impl variety summands
  inherit function_separator impl
  inherit prefix VInternal

  (* Name of the function that should be used for applying
     a substitution to an atom, parameterized by an atom sort. *)

  method virtual apply_atom: string -> string

  (* Name of the (generated) function that substitutes through an
     data structure, parameterized by a type name. *)

  method virtual substitute: string -> string

  (* Generate code for a function that applies two substitutions to
     (each sort of atoms inside) a pattern. An inner substitution
     applies to atoms in binding position and to sub-terms in inner
     scope, while an outer substitution applies to sub-terms in outer
     scope. The function expects one outer substitution for every atom
     sort that is outerlive within the current type and one inner
     substitution for every atom sort that is either boundlive or
     innerlive within the current type. *)

  method factor (x, label, factor) =
    attach VInternal label " = " (
      match factor with
      | PAtom (_, _, sort) ->
	  sprintf "%s %s %s" (self#apply_atom sort) x (inner_subst sort)
      | PEscape _ ->
	  x
      | PTypRef (MRef, container, _, (_, _, t)) ->
	  let outerlive = getouterlive t
	  and bound = StringSet.union (getboundlive t) (getinnerlive t) in
	  sprintf "%s(%s %s) %s"
	    (Container.map container) (self#substitute t) (tuple2 outer_subst outerlive inner_subst bound) x
      | PTypRef ((MInner | MOuter | MNeutral) as modifier, container, _, (_, _, t)) ->
	  let live = getlive t in
	  let bound = bound_in_hole ty modifier in
	  sprintf "%s(%s %s) %s"
	    (Container.map container) (self#substitute t) (inner_or_outer_substs bound live) x
    )

end

class import = object

  method apply_atom sort =
    sprintf "%s.find" (atom sort)

  method substitute t =
    "import_" ^ t

  method bv t =
    "bvi_" ^ t

  method bvaccu t =
    "bvi_accu_" ^ t

end

class internal = object

  method apply_atom sort =
    sprintf "%s.Subst.lookup" (atom sort)

  method substitute t =
    "subst_" ^ t

  method bv t =
    "bound_" ^ t

  method bvaccu t =
    "bound_accu_" ^ t

end

let unbound_excs sorts =
  StringSet.fold (fun sort accu ->
    sprintf "or a [%s.UnboundIdentifier] exception" (atom sort) :: accu
  ) sorts []

class import_expr impl params ty summands = object(self)

  val live =
    getlive ty

  inherit substitute_expr impl VRaw summands
  inherit import

  method name =
    self#substitute ty

  method header =
    sprintf "%s -> %sRaw.%s -> %s%s"
      (import_substs_type live)
      params ty
      params ty

  method formals =
    substs live

  method summary =
    "converts raw forms into internal forms"

  method in_comments =
    (import_substs_comment live) @ [ "an expression in raw form" ]

  method out_comments =
    [ "an expression in internal form";
      "  (semantically equivalent to the input expression)" ] @
    unbound_excs live

  method apply_abstraction x t =
    let outerlive = getouterlive t in
    let binds = getbinds t in
    assert (StringSet.equal (getboundlive t) binds);
    assert (StringSet.equal (getinnerlive t) binds);
    (* Compute which variables are bound by this abstraction.
       We exploit the fact that [boundlive] and [binds] coincide. *)
    printf "      let %s = %s %s in \n" (bvars binds) (self#bv t) x;
    (* Compute the substitutions that should be applied in inner scope.
       We exploit the fact that [boundlive U innerlive] and [binds] coincide. *)
    StringSet.iter (fun sort ->
      printf "      let %s = %s.Atom.mfreshb %s %s in\n"
	(inner_subst sort) (atom sort) (bvar sort) (subst sort)
    ) binds;
    (* Perform the recursive call. *)
    printf "      let %s = %s %s %s in\n" x (self#substitute t) (tuple2 subst outerlive inner_subst binds) x;
    (* Wrap the result into a new abstraction. *)
    sprintf "%s %s" (create t) x

end

class internal_expr impl params ty summands = object (self)

  val live =
    getlive ty

  inherit substitute_expr impl VInternal summands
  inherit internal

  method name =
    self#substitute ty

  method header =
    sprintf "%s -> %s%s -> %s%s"
      (substs_type live)
      params ty
      params ty

  method formals =
    substs live

  method summary =
    "substitutes atoms for atoms"

  method in_comments =
    (substs_comment live) @ [ "an expression in internal form" ]

  method out_comments =
    [ "an expression in internal form";
      "  (the result of applying these substitutions to the input expression)" ]

  method apply_abstraction x t =
    sprintf "%s %s %s" (apply t) (substs (inner_or_outer_live t)) x

end

class bviaccu params ty summands = object(self)

  val boundlive =
    getboundlive ty

  inherit bvaccu_pat true VRaw summands
  inherit notexported
  inherit import

  method name =
    self#bvaccu ty

  method formals =
    bvars boundlive

  method extend sort x =
    let accu = bvar sort in
    printf "      let %s = Identifier.Map.add %s () %s in\n" accu x accu

end

class bvaccu params ty summands = object (self)

  val boundlive =
    getboundlive ty

  inherit bvaccu_pat true VInternal summands
  inherit notexported
  inherit internal

  method name =
    self#bvaccu ty

  method formals =
    bvars boundlive

  method extend sort x =
    let accu = bvar sort in
    printf "      let %s = %s.AtomSet.add %s %s in\n" accu (atom sort) x accu

end

class bvi ty = object(self)

  val boundlive =
    getboundlive ty

  inherit basic_generator true
  inherit function_separator true
  inherit import
  inherit notexported

  method name  =
    self#bv ty

  method formals =
    ""

  method rhs =
    printf "\n  function %s -> %s %s %s\n" ty (self#bvaccu ty) (tuple (fun _ -> "Identifier.Map.empty") boundlive) ty

end

class bv impl params ty = object(self)

  val boundlive =
    getboundlive ty

  inherit basic_generator impl
  inherit function_separator impl
  inherit internal

  method name =
    self#bv ty

  method formals =
    ""

  method header =
    sprintf "%s%s -> %s" params ty (vars_type boundlive)

  method summary =
    "collects atoms in a binding position"

  method in_comments =
    [ "a pattern in internal form" ]

  method out_comments =
    bound_vars_comment boundlive

  method rhs =
    printf "\n  function %s -> %s %s %s\n" ty (self#bvaccu ty) (noatoms boundlive) ty

end    

class import_pat params ty summands = object(self)

  val outerlive =
    getouterlive ty

  val boundinnerlive = 
    StringSet.union (getboundlive ty) (getinnerlive ty)

  inherit substitute_pat true VRaw ty summands
  inherit import
  inherit notexported

  method name =
    self#substitute ty

  method formals =
    tuple2 outer_subst outerlive inner_subst boundinnerlive

end

class internal_pat impl params ty summands = object(self)

  val outerlive =
    getouterlive ty

  val boundinnerlive = 
    StringSet.union (getboundlive ty) (getinnerlive ty)

  inherit substitute_pat impl VInternal ty summands
  inherit internal

  method name =
    self#substitute ty

  method header =
    sprintf "%s -> %s%s -> %s%s" (substs_type2 outerlive boundinnerlive) params ty params ty

  method formals =
    tuple2 outer_subst outerlive inner_subst boundinnerlive

  method summary =
    "substitutes atoms for atoms"

  method in_comments =
    (outer_substs_comment ty outerlive) @
    (inner_substs_comment boundinnerlive) @
    [ "a pattern in internal form" ]

  method out_comments =
    [ "a pattern in internal form";
      "  (the result of applying these substitutions to the input pattern)" ]

end

(* Generate code to turn an expression or pattern back into raw form. *)

class export_expr impl params ty summands = object

  val live =
    getlive ty

  inherit exppat
  inherit [ expfactor ] map_generator impl VInternal summands
  inherit prefix VRaw
  inherit function_separator impl

  method name =
    export ty

  method header =
    sprintf "%s -> %s%s -> %sRaw.%s" (ms_type live) params ty params ty

  method formals =
    ms live

  method summary =
    "converts internal forms to raw forms"

  method in_comments =
    (ms_comment live) @ [ "an expression in internal form" ]

  method out_comments =
    [ "an expression in raw form";
      "  (semantically equivalent to the input expression)";
      "  (raises [Foo.Atom.Unknown] if an unknown atom of sort \"foo\" is encountered" ]

  method factor (x, label, factor) =
    attach VRaw label " = " (
      match factor with
      | EAtom (_, _, sort) ->
	  sprintf "%s.AtomIdMap.lookup %s %s" (atom sort) x (m sort)
      | EEscape _ ->
	  x
      | ETypRef (container, _, (_, _, t)) ->
	  let live = getlive t in
	  sprintf "%s(%s %s) %s" (Container.map container) (export t) (ms live) x
      | EAbstraction (_, (_, _, t)) ->
	  let outerlive = getouterlive t in
	  let binds = getbinds t in
	  assert (StringSet.equal (getboundlive t) binds);
	  assert (StringSet.equal (getinnerlive t) binds);
	  (* Open the abstraction. *)
	  printf "      let %s = %s %s in\n" t (openup t) x;
	  (* Compute which variables are bound by this abstraction.
	     We exploit the fact that [boundlive] and [binds] coincide. *)
	  printf "      let %s = %s %s in\n" (bvars binds) ((new internal)#bv t) t;
	  (* Compute the mappings that should be applied in inner scope.
	     We exploit the fact that [boundlive U innerlive] and [binds] coincide. *)
	  StringSet.iter (fun sort ->
	    printf "      let %s = %s.AtomIdMap.add_set %s %s in\n"
	      (inner_m sort) (atom sort) (bvar sort) (m sort)
	  ) binds;
	  (* Perform the recursive call. *)
	  sprintf "%s %s %s" (export t) (tuple2 m outerlive inner_m binds) t
      )

end

class export_pat params ty summands = object

  val outerlive =
    getouterlive ty

  val boundinnerlive =
    StringSet.union (getboundlive ty) (getinnerlive ty)

  inherit patpat
  inherit [ patfactor ] map_generator true VInternal summands
  inherit prefix VRaw
  inherit function_separator true
  inherit notexported

  method name =
    export ty

  method header =
    sprintf "%s -> %s%s -> %sRaw.%s" (ms_type2 outerlive boundinnerlive) params ty params ty

  method formals =
    tuple2 outer_m outerlive inner_m boundinnerlive

  method factor (x, label, factor) =
    attach VRaw label " = " (
      match factor with
      | PAtom (_, _, sort) ->
	  sprintf "%s.AtomIdMap.lookup %s %s" (atom sort) x (inner_m sort)
      | PEscape _ ->
	  x
      | PTypRef (MRef, container, _, (_, _, t)) ->
	  let outerlive = getouterlive t in
	  let bound = StringSet.union (getboundlive t) (getinnerlive t) in
	  sprintf "%s(%s %s) %s" (Container.map container) (export t) (tuple2 outer_m outerlive inner_m bound) x
      | PTypRef ((MInner | MOuter | MNeutral) as modifier, container, _, (_, _, t)) ->
	  let live = getlive t in
	  let bound = bound_in_hole ty modifier in
	  sprintf "%s(%s %s) %s" (Container.map container) (export t) (inner_or_outer_ms bound live) x
      )

end

(* Generate code to turn an internal form into flat internal form and back. *)

class flatten = object

  method source =
    VInternal

  method target =
    VFlat

  method name =
    flatten

  method comment =
    "  (obtained by opening up every abstraction in the input expression)"

  method abstraction x t =
    (* Open up the abstraction. *)
    printf "      let %s = %s %s in\n" t (openup t) x;
    (* Perform the recursive call. *)
    sprintf "%s %s" (flatten t) t

end

class unflatten = object

  method source =
    VFlat

  method target =
    VInternal

  method name =
    unflatten

  method comment =
    "  (obtained by creating abstractions where needed)"

  method abstraction x t =
    (* Perform the recursive call. *)
    printf "      let %s = %s %s in\n" t (unflatten t) x;
    (* Create an abstraction. *)
    sprintf "%s %s" (create t) t

end

class flatten_or_unflatten_expr d impl params ty summands = object

  inherit exppat
  inherit [ expfactor ] map_generator impl d#source summands
  inherit prefix d#target
  inherit function_separator impl

  method name =
    d#name ty

  method header =
    sprintf "%s%s%s -> %s%s%s" params (prefix d#source) ty params (prefix d#target) ty

  method formals =
    ""

  method summary =
    sprintf "converts %s forms to %s forms" (explain d#source) (explain d#target)

  method in_comments =
    [ sprintf "an expression in %s form" (explain d#source) ]

  method out_comments =
    [ sprintf "an expression in %s form" (explain d#target); d#comment ]

  method factor (x, label, factor) =
    attach d#target label " = " (
      match factor with
      | EAtom _ ->
	  (* Identity on atoms. *)
	  x
      | EEscape _ ->
	  (* Identity on foreign data. *)
	  x
      | ETypRef (container, _, (_, _, t)) ->
	  (* Recursive call on named data. *)
	  sprintf "%s%s %s" (Container.map container) (d#name t) x
      | EAbstraction (_, (_, _, t)) ->
	  d#abstraction x t
    )

end

class flatten_or_unflatten_pat d params ty summands = object

  inherit patpat
  inherit [ patfactor ] map_generator true d#source summands
  inherit prefix d#target
  inherit function_separator true
  inherit notexported

  method name =
    d#name ty

  method header =
    sprintf "%s%s%s -> %s%s%s" params (prefix d#source) ty params (prefix d#target) ty

  method formals =
    ""

  method factor (x, label, factor) =
    attach d#target label " = " (
      match factor with
      | PAtom _ ->
	  (* Identity on atoms. *)
	  x
      | PEscape _ ->
	  (* Identity on foreign data. *)
	  x
      | PTypRef (_, container, _, (_, _, t)) ->
	  (* Recursive call on named data, regardless of the modifier. *)
	  sprintf "%s%s %s" (Container.map container) (d#name t) x
    )

end

(* Generate code to compute the free atoms of a (cooked) expression
   or pattern. *)

class fvaccu ty summands = object

  val live =
    getlive ty

  inherit exppat
  inherit [ expfactor ] fold_generator true VInternal summands
  inherit function_separator true
  inherit notexported

  method name =
    fvaccu ty

  method formals =
    fvars live

  method factor (x, _, factor) =
    match factor with
    | EAtom (_, _, sort) ->
	let accu = fvar sort in
	printf "      let %s = %s.AtomSet.add %s %s in\n" accu (atom sort) x accu
    | EEscape _ ->
	()
    | ETypRef (container, _, (_, _, t)) ->
	let live = getlive t in
	let accu = fvars live in
	printf "      let %s = %s%s %s %s in \n" accu (Container.fold container) (fvaccu t) accu x
    | EAbstraction (_, (_, _, t)) ->
	let outerlive = getouterlive t in
	let binds = getbinds t in
	assert (StringSet.equal (getboundlive t) binds);
	assert (StringSet.equal (getinnerlive t) binds);
	(* Open the abstraction. *)
	printf "      let %s = %s %s in\n" t (openup t) x;
	(* Compute which variables are bound, free in inner scope, and
	   free in outer scope inside this abstraction. We exploit the
	   fact that [boundlive], [innerlive], and [binds] coincide.
	   The variables that are free in outer scope inside the
	   abstraction are free outside the abstraction, so, at
	   outer live sorts, we directly update the free variables
	   accumulator. *)
	printf "      let %s = %s %s %s in\n"
	  (biffvars binds binds outerlive) (bvfvaccu t) (nonofvars binds binds outerlive) t;
	(* Use this information to determine which variables are free
	   in this abstraction. It is the union of those that are free
	   in outer scope and of those that are free in inner scope and
	   not bound. *)
	StringSet.iter (fun sort ->
	  let accu = fvar sort in
	  let bigSort = atom sort in
	  printf "      let %s = %s.AtomSet.union %s (%s.AtomSet.diff %s %s) in\n"
	    accu bigSort accu bigSort (inner_fvar sort) (bvar sort)
	) binds

end

class fv impl params ty = object

  val live =
    getlive ty

  inherit basic_generator impl
  inherit function_separator impl
  inherit internal

  method name =
    fv ty

  method formals =
    ""

  method header =
    sprintf "%s%s -> %s" params ty (vars_type live)

  method summary =
    "collects free atoms"

  method in_comments =
    [ "an expression in internal form" ]

  method out_comments =
    free_vars_comment live

  method rhs =
    printf "\n  function %s -> %s %s %s\n" ty (fvaccu ty) (noatoms live) ty

end

class bvfvaccu ty summands = object

  val outerlive =
    getouterlive ty

  val innerlive =
    getinnerlive ty
    
  val boundlive =
    getboundlive ty

  inherit patpat
  inherit [ patfactor ] fold_generator true VInternal summands
  inherit function_separator true
  inherit notexported

  method name =
    bvfvaccu ty

  method formals =
    bifofvars boundlive innerlive outerlive

  method factor (x, _, factor) =
    match factor with
    | PAtom (_, _, sort) ->
	let accu = bvar sort in
	printf "      let %s = %s.AtomSet.add %s %s in\n" accu (atom sort) x accu
    | PEscape _ ->
	()
    | PTypRef (MRef, container, _, (_, _, t)) ->
	let outerlive = getouterlive t
	and innerlive = getinnerlive t
	and boundlive = getboundlive t in
	let accu = bifofvars boundlive innerlive outerlive in
	printf "      let %s = %s%s %s %s in\n" accu (Container.fold container) (bvfvaccu t) accu x
    | PTypRef ((MInner | MOuter | MNeutral) as modifier, container, _, (_, _, t)) ->
	let live = getlive t in
	let bound = bound_in_hole ty modifier in
	let accu = inner_or_outer_fvars bound live in
	printf "      let %s = %s%s %s %s in\n" accu (Container.fold container) (fvaccu t) accu x

end

class bvfv impl params ty = object

  val outerlive =
    getouterlive ty

  val innerlive =
    getinnerlive ty
    
  val boundlive =
    getboundlive ty

  inherit basic_generator impl
  inherit function_separator impl
  inherit internal

  method name =
    bvfv ty

  method formals =
    ""

  method header =
    sprintf "%s%s -> %s" params ty (vars_type3 boundlive innerlive outerlive)

  method summary =
    "collects atoms in a binding position and free atoms inside sub-expressions"

  method in_comments =
    [ "a pattern in internal form" ]

  method out_comments =
    (bound_vars_comment boundlive) @
    (free_inner_vars_comment innerlive) @
    (free_outer_vars_comment ty outerlive)

  method rhs =
    printf "\n  function %s -> %s %s %s\n" ty (bvfvaccu ty) (nonono boundlive innerlive outerlive) ty

end

(* ---------------------------------------------------------------------------- *)

(* Generate code for functions that freshen two patterns (of the same
   type) simultaneously. These functions carry two accumulators: for
   each pattern, a map [m] that maps the bound atoms that have been
   encountered so far in this pattern (and only them) to fresh atoms.
   The two maps are kept synchronized -- bound atoms that logically
   correspond to each other are mapped to the same fresh atom in both
   maps. The exception [Invalid_argument] is raised if the two patterns
   do not have alpha-convertible domains. *)

let freshen2_accu i sort =
  sprintf "%s%d" (subst sort) i

let freshen2_formals bound =
  tuple2 (freshen2_accu 1) bound (freshen2_accu 2) bound

let freshen2_seed bound =
  tuple2 nosubst bound nosubst bound

class freshen2 params ty summands = 

  let bound =
    getbinds ty
  in

  object (self)

  inherit patpat
  inherit [ patfactor ] fold2_generator true VInternal summands
  inherit function_separator true
  inherit notexported

  method name =
    freshen2 ty

  method header =
    let accus_type =
      tuple_type2 subst_type bound bound
    in
    sprintf "%s -> %s%s -> %s%s -> %s"
      accus_type params ty params ty accus_type

  method formals =
    freshen2_formals bound

  method factor2 (x1, label1, factor1) (x2, label2, factor2) =
    match factor1 with

    | PAtom (_, _, sort) ->

	let m1 = freshen2_accu 1 sort
	and m2 = freshen2_accu 2 sort in
	printf "      let %s, %s = %s.Subst.freshen2 %s %s %s %s in\n"
	  m1 m2 (atom sort) x1 m1 x2 m2

    | PTypRef (MRef, container, _, (_, _, t)) ->

	printf "      let %s = %s%s %s %s %s in\n"
	  self#formals
	  (Container.fold2 container)
	  (freshen2 t)
	  self#formals
	  x1 x2 

    | PEscape _
    | PTypRef ((MInner | MOuter | MNeutral), _, _, (_, _, _)) ->

	()

end

(* Generate code for comparing two structures up to alpha-equivalence.
   There are two variants of the function: one has a unit accumulator
   and raises an exception when the two structures are different --
   this interface is required by the fact that we are using fold2
   over containers -- and the other returns a Boolean result. *)

class aeq_expr params ty summands = object

  inherit [ expfactor] fold2_generator true VInternal summands
  inherit function_separator true
  inherit exppat
  inherit notexported

  method name =
    aeq ty

  method header =
    sprintf "unit -> %s%s -> %s%s -> unit"
      params ty
      params ty

  method formals =
    "()"

  method factor2 (x1, label1, factor1) (x2, label2, factor2) =
    match factor1 with
    | EAtom (_, _, sort) ->
	printf "      if not (%s.Atom.equal %s %s) then raise (Invalid_argument \"%s\");\n"
	  (atom sort) x1 x2 (aeq ty)
    | ETypRef (container, _, (_, _, t)) ->
	printf "      %s%s () %s %s;\n"
	  (Container.fold2 container)
	  (aeq t)
	  x1 x2
    | EEscape _ ->
	()
    | EAbstraction (_, (_, _, t)) ->
	printf "      let %s, %s = %s %s %s in\n" x1 x2 (openup2i t) x1 x2;
	printf "      %s () %s %s;\n" (aeq t) x1 x2

end

class aeq_pat params ty summands = object

  inherit [ patfactor] fold2_generator true VInternal summands
  inherit function_separator true
  inherit patpat
  inherit notexported

  method name =
    aeq ty

  method header =
    sprintf "unit -> %s%s -> %s%s -> unit"
      params ty
      params ty

  method formals =
    "()"

  method factor2 (x1, label1, factor1) (x2, label2, factor2) =
    match factor1 with
    | PAtom (_, _, sort) ->
	printf "      if not (%s.Atom.equal %s %s) then raise (Invalid_argument \"%s\");\n"
	  (atom sort) x1 x2 (aeq ty)
    | PTypRef (_, container, _, (_, _, t)) ->
	printf "      %s%s () %s %s;\n"
	  (Container.fold2 container)
	  (aeq t)
	  x1 x2
    | PEscape _ ->
	()

end

class equality impl params ty = object

  inherit basic_generator impl
  inherit function_separator impl

  method name =
    equality ty

  method header =
    sprintf "%s%s -> %s%s -> bool"
      params ty
      params ty

  method formals =
    ""

  method summary =
    "tests whether two data structures are related modulo alpha-conversion"

  method in_comments =
    [ "two data structures" ]

  method out_comments =
    [ "a Boolean outcome" ]

  method rhs =
    printf "fun x1 x2 -> \n  change_invalid_to_bool %s x1 x2\n" (aeq ty)

end

(* ---------------------------------------------------------------------------- *)
(* Generating code for dealing with abstractions. *)

(* The type of the delayed substitutions that are stored at every
   abstraction node. In principle, there is one for every atom sort
   that this abstraction binds. In practice, there is one for every
   atom sort that is live in this abstraction (either in inner or in
   outer scope), because delaying some (outerlive) substitutions
   while eagerly pushing down some (innerlive) others would require
   more complex code and would not necessarily be more efficient. *)

(* Generate the function that allows creating abstractions. *)

class create impl params t = object

  val live =
    inner_or_outer_live t

  inherit basic_generator impl
  inherit function_separator impl

  method name =
    create t

  method header =
    sprintf "%s%s -> %s%s" params t params (abstraction_type t)

  method formals =
    ""

  method summary =
    "creates an opaque abstraction"

  method in_comments =
    [ "a pattern in internal form";
      "  (a transparent version of the abstraction)" ]

  method out_comments =
    [ "an abstract data structure";
      "  (an opaque version of the abstraction)" ]

  method rhs =
    printf "\n  function body -> {
    %s = %s;
    %s = body
  }
" (delayed_field t) (subst_id live) (body_field t)

end

(* Generate the function that allows applying a tuple of
   substitutions to an abstraction. All substitutions are
   suspended. *)

class apply t = object

  val live =
    inner_or_outer_live t

  inherit basic_generator true
  inherit function_separator true
  inherit notexported

  method name =
    apply t

  method formals =
    ""

  method rhs =
    printf "
  fun %s abstraction ->
    let %s = abstraction.%s in {
      abstraction with %s = %s
    }
"
    (substs live)
    (delayeds live) (delayed_field t)
    (delayed_field t) (tuple (fun sort ->
      sprintf "%s.Subst.compose %s %s" (atom sort) (subst sort) (delayed_field sort)
    ) live)

end

(* Generate the function that opens an abstraction. *)

class openup impl params t = object

  val live =
    inner_or_outer_live t

  val bound =
    getbinds t

  val outerlive =
    getouterlive t

  inherit basic_generator impl
  inherit function_separator impl

  method name =
    openup t

  method header =
    sprintf "%s%s -> %s%s" params (abstraction_type t) params t

  method formals =
    ""

  method summary =
    "opens an opaque abstraction"

  method in_comments =
    [ "an abstract data structure";
      "  (an opaque version of the abstraction)" ]

  method out_comments =
    [ "a pattern in internal form";
      "  (a transparent version of the abstraction, where all bound atoms are freshly renamed)" ]

  method rhs =

    (* First, extract the delayed substitutions (one per live sort) and the
       body. *)

    printf "function abstraction ->
  let %s = abstraction.%s in
  let body = abstraction.%s in"
    (delayeds live) (delayed_field t) (body_field t);

    (* Next, compute the body's bound variables (one set per bound sort). *)

    printf "
  let %s = %s body in"
    (bvars bound) ((new internal)#bv t);

    (* Next, assign fresh names to the pattern's bound atoms, overriding
       any existing bindings in the delayed substitution. *)

    StringSet.iter (fun sort ->
      printf "
  let %s = %s.Subst.freshen %s %s in"
      (subst sort) (atom sort) (bvar sort) (delayed_field sort)
    ) bound;

    (* Next, rename throughout the body. At bound sorts, the
       unmodified delayed substitution is applied to atoms that lie in
       outer scope, while the overridden delayed substitution is
       applied to atoms that lie in inner scope. At other sorts, the
       delayed substitution is applied. *)

    printf "
  let body = %s %s body in"
    ((new internal)#substitute t) (tuple2 delayed_field outerlive subst bound);

    (* Last, if any of the delayed substitutions is nontrivial, the
       new body is memoized. Otherwise, memoizing is not useful -- on
       the contrary, it would lead to unnecessary GC activity. *)

    printf "
  if not %s then begin
    abstraction.%s <- %s;
    abstraction.%s <- body
  end;
  body
" (is_subst_id delayed_field live) (delayed_field t) (subst_id live) (body_field t)

end

(* Generate the function that opens two abstractions at once. *)

class openup2i params t = object

  val live =
    inner_or_outer_live t

  val bound =
    getbinds t

  val outerlive =
    getouterlive t

  inherit basic_generator true
  inherit function_separator true
  inherit notexported

  method name =
    openup2i t

  method header =
    sprintf "%s%s -> %s%s -> %s%s * %s%s"
      params (abstraction_type t) params (abstraction_type t)
      params t params t

  method formals =
    ""

  method rhs =

    printf "fun abstraction1 abstraction2 ->";

    (* First, extract the delayed substitutions (one per live sort) and the
       bodies. *)

    for i = 1 to 2 do
      printf "
  let %s = abstraction%d.%s in
  let body%d = abstraction%d.%s in"
        (idelayeds i live) i (delayed_field t) i i (body_field t)
    done;

    (* Next, match up the two bodies. This either fails or returns one
       substitution per side and per bound sort. *)

    printf "
  let %s = %s %s body1 body2 in"
      (freshen2_formals bound)
      (freshen2 t)
      (freshen2_seed bound);

    (* Next, override any existing bindings in the delayed substitutions
       with the maps thus obtained. *)

    for i = 1 to 2 do
      StringSet.iter (fun sort ->
	printf "
  let %s = %s.Subst.union %s %s in"
	(freshen2_accu i sort)
        (atom sort)
        (idelayed_field i sort)
        (freshen2_accu i sort)
      ) bound
    done;

    (* Next, rename throughout the bodies. *)

    for i = 1 to 2 do
      printf "
  let body%d = %s %s body%d in"
	i
	((new internal)#substitute t)
	(tuple2 (idelayed_field i) outerlive (freshen2_accu i) bound)
	i
    done;

    (* Last, memoize. *)

    for i = 1 to 2 do
      printf "
  if not %s then begin
    abstraction%d.%s <- %s;
    abstraction%d.%s <- body%d
  end;"
	(is_subst_id (idelayed_field i) live)
	i
	(delayed_field t)
	(subst_id live)
	i
	(body_field t)
	i
    done;

    printf "
  body1, body2
"

end

class openup2 impl params t = object

  inherit basic_generator impl
  inherit function_separator impl

  method name =
    openup2 t

  method header =
    sprintf "%s%s -> %s%s -> %s%s * %s%s"
      params (abstraction_type t) params (abstraction_type t)
      params t params t

  method formals =
    ""

  method summary =
    "opens two opaque abstractions at once, ensuring that their structure and bound atoms match"

  method in_comments =
    [ "two abstract data structures";
      "  (two opaque abstractions)" ]

  method out_comments =
    [ "two patterns in internal form, with identical structure and bound atoms";
      "  (transparent versions of the abstractions, where all bound atoms are freshly renamed)";
      "or an [Open2] exception" ]

  method rhs =
    printf "fun x1 x2 -> \n  change_invalid_to_open2 %s x1 x2\n" (openup2i t)

end

(* ---------------------------------------------------------------------------- *)
(* Generate all function definitions. *)

let process_declaration impl = function
  | DeclSort _
  | DeclContainer _
  | DeclIdentifier _ ->
      ()
  | DeclExpType (params, (_, _, t), summands) ->
      let params = (new param_translation)#params (strip params) in

      if not impl then
	printf "\n(* The following functions operate over the expression type \"%s\". *)\n" t;

      (new import_expr impl params t summands)#body;
      (new internal_expr impl params t summands)#body;
      (new export_expr impl params t summands)#body;
      (new flatten_or_unflatten_expr (new flatten) impl params t summands)#body;
      (new flatten_or_unflatten_expr (new unflatten) impl params t summands)#body;
      (new fv impl params t)#body;
      (new equality impl params t)#body;

      if impl then begin
	(new aeq_expr params t summands)#body;
	(new fvaccu t summands)#body
      end

  | DeclPatType (params, (_, _, t), _, summands) ->
      let params = (new param_translation)#params (strip params) in

      if not impl then
	printf "\n(* The following functions operate over the pattern type \"%s\". *)\n" t;

      (new internal_pat impl params t summands)#body;
      (new bv impl params t)#body;
      (new bvfv impl params t)#body;
      (new equality impl params t)#body;

      if impl then begin
	(new import_pat params t summands)#body;
	(new bviaccu params t summands)#body;
	(new bvi t)#body;
	(new bvaccu params t summands)#body;
	(new export_pat params t summands)#body;
	(new flatten_or_unflatten_pat (new flatten) params t summands)#body;
	(new flatten_or_unflatten_pat (new unflatten) params t summands)#body;
	(new bvfvaccu t summands)#body;
	(new aeq_pat params t summands)#body;
	(new freshen2 params t summands)#body
      end;

      if StringSet.mem t abstractions then begin

	(new create impl params t)#body;
	(new openup impl params t)#body;
	(new openup2 impl params t)#body;

	if impl then begin
	  (new openup2i params t)#body;
	  (new apply t)#body
	end

      end

(* ---------------------------------------------------------------------------- *)
(* Generating code for an extensible map function. *)

class virtual ['factor] omap params ty summands impl translate = object(self)

  inherit ['factor] map_generator impl VInternal summands as super
  inherit method_separator

  method name =
    ty

  method header =
    sprintf "%s%s -> %s%s" params ty params ty

  method formals =
    ""

  method prefix data =
    "self#" ^ (String.lowercase data)

  method record_factor (x, label, _) =
    (* Each field is passed individually to the per-field method. *)
    attach VInternal label " = " (
      sprintf "self#%s %s" (ido2string label) x
    )

  method data_factor (x, _, _) =
    (* Parameters are passed as-is to the per-data constructor method. *)
    x

  method body =

    (* Generate the main method. *)

    super#body;

    (* Generate one auxiliary method for every data constructor or field label. *)

    List.iter (function
      | Summand (Some (_, _, data), factors) ->

	  (* Generate a method for this data constructor.

	     The method maps a tuple of factors to a value of the current type.
	     The tuple is absent (not unit) when there are no factors. *)

	  let o = object

	    inherit basic_generator impl
	    inherit method_separator
		
	    method name =
	      String.lowercase data

	    method header =
	      sprintf "%s%s%s"
		(StringAux.show_list "" "" " * " " -> " (List.map translate#factor factors))
		params ty

	    method formals =
	      ""

	    method summary =
	      sprintf "transforms under the data constructor \"%s\"" data

	    method in_comments =
	      [ sprintf "a tuple of the parameters to \"%s\"" data ]

	    method out_comments =
	      [ sprintf "the result of applying \"%s\" to the transformed parameters" data ]

	    method rhs =
	      let ps, pfs = self#pats factors in
	      printf "%s" (StringAux.show_list "" "\n  function (" ", " ") -> \n      " ps);
	      printf "%s%s\n" data (duple (List.map self#factor pfs))

	  end
	  in
	  o#body

      | Summand (None, factors) ->
	  List.iter (function
	    | (Some (_, _, label) as labelid, factor) ->

		(* Generate a method for this field label.

		   The method maps a factor to a factor. *)

		let o = object

		  inherit basic_generator impl
		  inherit method_separator

		  method name =
		    label

		  method header =
		    let f = translate#nude_factor factor in
		    sprintf "%s -> %s" f f

		  method formals =
		    ""

		  method summary =
		    sprintf "transforms under the field label \"%s\"" label

		  method in_comments =
		    [ sprintf "the contents of field \"%s\"" label ]

		  method out_comments =
		    [ "the transformed parameter" ]

		  method rhs =
		    let x = self#pat 0 factor in
		    printf "\n  function %s -> %s\n" x (self#factor (x, labelid, factor))

		end
		in
		o#body

	    | (None, _) ->
		()
	  ) factors
    ) summands


end

class omap_expr params ty summands impl = object

  inherit exppat
  inherit [ expfactor ] omap params ty summands impl (new exp_type_translation VInternal)

  method summary =
    "transforms an expression"

  method in_comments =
    [ "an expression in internal form" ]

  method out_comments =
    [ "an expression in internal form";
      "  (the result of applying the transformation to the input expression)" ]

  method factor (x, _, factor) =
    match factor with
    | EAtom _
    | EEscape _ ->
	(* Identity. *)
	x
    | ETypRef (container, _, (_, _, t)) ->
	(* Recursive call. *)
	sprintf "%s(self#%s) %s" (Container.map container) t x
    | EAbstraction (_, (_, _, t)) ->
	(* Open, recursive call, close. *)
	sprintf "%s (self#%s (%s %s))" (create t) t (openup t) x

end

class omap_pat params ty summands impl = object

  inherit patpat
  inherit [ patfactor ] omap params ty summands impl (new pat_type_translation)

  method summary =
    "transforms a pattern"

  method in_comments =
    [ "a pattern in internal form" ]

  method out_comments =
    [ "a pattern in internal form";
      "  (the result of applying the transformation to the input pattern)" ]

  method factor (x, _, factor) =
    match factor with
    | PAtom _
    | PEscape _ ->
	(* Identity. *)
	x
    | PTypRef (_, container, _, (_, _, t)) ->
	(* Recursive call. *)
	sprintf "%s(self#%s) %s" (Container.map container) t x

end

let generate_map_class impl =

  (* Generate a class that implements an extensible map function. *)

  if not impl then
    printf "
(* The following class contains code that ``transforms'' a data structure.
   The methods provided in this class implement an identity transformation,
   that is, they traverse the data structure and produce a semantically
   equivalent copy of it. The intended use of this class is for the client
   to create a subclass and override one or several methods so as to obtain
   nontrivial behavior. *)
";

  printf "\nclass %smap %s\n"
    (StringAux.show_list "" "[ " ", " " ] "
       (List.map (new param_translation)#param (StringSet.elements allparams)))
    (if impl then "= object(self)" else ": object");

  (* Generate the methods. *)

  List.iter (function
    | DeclExpType (params, (_, _, t), summands) ->
	let params = (new param_translation)#params (strip params) in
	(new omap_expr params t summands impl)#body
    | DeclPatType (params, (_, _, t), _, summands) ->
	let params = (new param_translation)#params (strip params) in
	(new omap_pat params t summands impl)#body
    | DeclSort _
    | DeclContainer _ 
    | DeclIdentifier _ ->
	()
  ) declarations;

  printf "\nend\n"

(* ---------------------------------------------------------------------------- *)
(* Generating code for an extensible fold function. *)

let accuv =
  "accu"

let accutv =
  "'accumulator"

class virtual ['factor] ofold params ty summands impl translate = object(self)

  inherit ['factor] fold_generator impl VInternal summands as super
  inherit method_separator

  method name =
    ty

  method header =
    sprintf "%s -> %s%s -> %s" accutv params ty accutv

  method formals =
    accuv

  method record_factor (x, label, _) =
    (* Each field is passed individually to the per-field method. *)
    printf "      let %s = self#%s %s %s in\n" accuv (ido2string label) accuv x

  method data data pfs =
    (* Parameters are passed as-is to the per-data constructor method. *)
    let p (x, _, _) = x in
    printf "      self#%s %s%s\n" (String.lowercase data) accuv (duple (List.map p pfs))

  method body =

    (* Generate the main method. *)

    super#body;

    (* Generate one auxiliary method for every data constructor or field label. *)

    List.iter (function
      | Summand (Some (_, _, data), factors) ->

	  (* Generate a method for this data constructor.

	     The method accepts a tuple of factors.
	     The tuple is absent (not unit) when there are no factors. *)

	  let o = object

	    inherit basic_generator impl
	    inherit method_separator
		
	    method name =
	      String.lowercase data

	    method header =
	      sprintf "%s -> %s%s"
		accutv
		(StringAux.show_list "" "" " * " " -> " (List.map translate#factor factors))
		accutv

	    method formals =
	      accuv

	    method summary =
	      sprintf "iterates under the data constructor \"%s\"" data

	    method in_comments =
	      [ "an accumulator";
		sprintf "a tuple of the parameters to \"%s\"" data ]

	    method out_comments =
	      [ "an updated accumulator, obtained by traversing the parameters" ]

	    method rhs =
	      let ps, pfs = self#pats factors in
	      printf "%s" (StringAux.show_list "" "\n  function (" ", " ") -> \n" ps);
	      List.iter self#factor pfs;
	      printf "      %s\n" self#formals

	  end
	  in
	  o#body

      | Summand (None, factors) ->
	  List.iter (function
	    | (Some (_, _, label) as labelid, factor) ->

		(* Generate a method for this field label.

		   The method accepts a factor. *)

		let o = object

		  inherit basic_generator impl
		  inherit method_separator

		  method name =
		    label

		  method header =
		    let f = translate#nude_factor factor in
		    sprintf "%s -> %s -> %s" accutv f accutv

		  method formals =
		    accuv

		  method summary =
		    sprintf "iterates under the field label \"%s\"" label

		  method in_comments =
		    [ "an accumulator";
		      sprintf "the contents of field \"%s\"" label ]

		  method out_comments =
		    [ "an updated accumulator, obtained by traversing the parameter" ]

		  method rhs =
		    let x = self#pat 0 factor in
		    printf "\n  function %s ->\n" x;
		    self#factor (x, labelid, factor);
		    printf "      %s\n" self#formals

		end
		in
		o#body

	    | (None, _) ->
		()
	  ) factors
    ) summands


end

class ofold_expr params ty summands impl = object

  inherit exppat
  inherit [ expfactor ] ofold params ty summands impl (new exp_type_translation VInternal)

  method summary =
    "iterates over an expression"

  method in_comments =
    [ "an accumulator";
      "an expression in internal form" ]

  method out_comments =
    [ "an updated accumulator, obtained by traversing the input expression" ]

  method factor (x, _, factor) =
    match factor with
    | EAtom _
    | EEscape _ ->
	(* Do nothing. *)
	()
    | ETypRef (container, _, (_, _, t)) ->
	(* Recursive call. *)
	printf "      let %s = %s(self#%s) %s %s in\n" accuv (Container.fold container) t accuv x
    | EAbstraction (_, (_, _, t)) ->
        (* Open, recursive call. *)
	printf "      let %s = self#%s %s (%s %s) in\n" accuv t accuv (openup t) x

end

class ofold_pat params ty summands impl = object

  inherit patpat
  inherit [ patfactor ] ofold params ty summands impl (new pat_type_translation)

  method summary =
    "iterates over a pattern"

  method in_comments =
    [ "an accumulator";
      "a pattern in internal form" ]

  method out_comments =
    [ "an updated accumulator, obtained by traversing the input pattern" ]

  method factor (x, _, factor) =
    match factor with
    | PAtom _
    | PEscape _ ->
	(* Do nothing. *)
	()
    | PTypRef (_, container, _, (_, _, t)) ->
	(* Recursive call. *)
	printf "      let %s = %s(self#%s) %s %s in\n" accuv (Container.fold container) t accuv x

end

let generate_fold_class impl =

  (* Generate a class that implements an extensible fold function. *)

  if not impl then
    printf "
(* The following class contains code that iterates over a data structure
   while updating an accumulator. The methods provided in this class
   implement an identity over the accumulator, that is, they traverse the
   data structure and always return the initial accumulator. The intended
   use of this class is for the client to create a subclass and override
   one or several methods so as to obtain nontrivial behavior. *)
";

  printf "\nclass %sfold %s\n"
    (StringAux.show_list "" "[ " ", " " ] "
       (accutv :: (List.map (new param_translation)#param (StringSet.elements allparams))))
    (if impl then "= object(self)" else ": object");

  (* Generate the methods. *)

  List.iter (function
    | DeclExpType (params, (_, _, t), summands) ->
	let params = (new param_translation)#params (strip params) in
	(new ofold_expr params t summands impl)#body
    | DeclPatType (params, (_, _, t), _, summands) ->
	let params = (new param_translation)#params (strip params) in
	(new ofold_pat params t summands impl)#body
    | DeclSort _
    | DeclContainer _ 
    | DeclIdentifier _ ->
	()
  ) declarations;

  printf "\nend\n"

(* ---------------------------------------------------------------------------- *)
(* Generate the implementation file. *)

let () =
  printf "(* This file was generated from %s.mla. Do not edit! *)" basename;
  printf "%s" prologue;
  printf "\
module Identifier = %s

exception Open2

let change_invalid_to_bool f x1 x2 =
  try
    f () x1 x2;
    true
  with Invalid_argument _ ->
    false

let change_invalid_to_open2 f x1 x2 =
  try
    f x1 x2
  with Invalid_argument _ ->
    raise Open2

" identifier_module;
  translate_sort_declarations ();
  printf "\
module Raw = struct

";
  translate_type_declarations true VRaw;
  printf "
end

";
  printf "\
module Flat = struct

";
  translate_type_declarations true VFlat;
  printf "
end

";
  translate_type_declarations true VInternal;
  printf "
let option_map f = function
  | None ->
      None
  | Some x ->
      Some (f x)

let option_fold f accu = function
  | None ->
      accu
  | Some x ->
      f accu x

let option_fold2 f accu o1 o2 =
  match o1, o2 with
  | None, None ->
      accu
  | Some x1, Some x2 ->
      f accu x1 x2
  | None, Some _
  | Some _, None ->
      raise (Invalid_argument \"option_fold2\")

let rec _dummy x = x
";
  List.iter (process_declaration true) declarations;
  generate_map_class true;
  generate_fold_class true

(* ---------------------------------------------------------------------------- *)
(* Generate the interface file. *)

let () =
  close_out ml;
  output := mli

let translate_sort_declarations () =
  StringSet.iter (fun sort ->
    printf "\
      (* This module defines atoms of sort %s. *)
\n\
      module %s : Atom with type identifier = Identifier.t\n\n" sort (atom sort)
  ) sorts

let () =
  printf "(* This file was generated from %s.mla. Do not edit! *)" basename;
  printf "%s" prologue;
  printf "\
(* Expose the module signatures defined in alphaLib. *)

open AlphaLib.Signatures

(* This module defines identifiers, that is,
   external representations for atoms. *)

module Identifier : Identifier with type t = %s.t

(* This exception is raised by the functions that open two
   abstractions at once when the two abstractions have different
   structure, so that their bound atoms cannot be forced to be
   identical. *)

exception Open2

" identifier_module;
  translate_sort_declarations();
  printf "\
(* This module reflects the type definitions found in the source
   file, but in a raw form, that is, in a form where atoms are
   represented by identifiers and abstractions are transparent.
   Raw forms are usually produced by a parser and consumed by a
   pretty-printer. Functions that convert to and from raw forms
   are provided. *)

module Raw : sig

";
  translate_type_declarations false VRaw;
  printf "
end

(* This module reflects the type definitions found in the source file,
   in a flat internal form, that is, in a form where atoms are opaque,
   but abstractions are transparent. Functions that convert back and
   forth between internal and flat internal forms are provided. The
   conversion from internal form to flat internal form guarantees that
   all names are unique -- which is why flat internal form can be
   attractive. *)

module Flat : sig

";
  translate_type_declarations false VFlat;
  printf "
end

(* The following type definitions reflect those found in the source
   file, this time in an internal form, that is, in a form where both
   atoms and abstractions are opaque (abstract) data structures.
   Functions that convert between the opaque and transparent versions
   of each abstraction are provided. This approach provides safety --
   the contents of an abstraction cannot be inspected without
   appropriate freshening of its bound atoms -- and better efficiency
   -- sets and maps over atoms are usually less costly than sets and
   maps over identifiers. *)

";
  translate_type_declarations false VInternal;
  List.iter (process_declaration false) declarations;
  generate_map_class false;
  generate_fold_class false

let () =
  close_out mli

