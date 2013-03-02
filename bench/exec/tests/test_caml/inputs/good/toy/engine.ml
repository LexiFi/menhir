(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/yquem/cristal/fpottier/cvs/toy/engine.ml,v 1.23 2000/05/31 14:34:35 fpottier Exp $ *)

(* This module instantiates Wallace for our term language, and makes additional definitions in order to set up a
   typechecker for a simple $\lambda$-calculus. *)

module type S = sig

  (* At the core of the engine is Wallace. *)

  module Core : Core.S

  (* Type schemes and environments are made abstract in the module's external interface. *)

  type scheme
  type environment

  val builtin: environment

  (* Printing type schemes. *)

  module Print : sig

    val text : bool -> scheme -> unit

  end

  (* This function simplifies a type scheme. It is destructive, i.e. its argument must no longer be used afterwards.
     TEMPORARY could/should be made internal *)

  val simplify: scheme -> scheme

  (* The type inference engine. *)

  exception Failure of string

  val infer: environment -> InternalSyntax.expression -> scheme

end

  (* The module is parameterized by an implementation of environments. It is also parameterized by an implementation
     of contexts, which are maps from internal identifiers to type variables. *)

module Make
  (Env : Env.S)
  (IdMap : Context.S with type key = Env.identifier)
= struct

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Instantiating Wallace}

   First, let Wallace know about our term language, defined by MlAlgebra. Also, choose implementations for its
   abstract data structures. *)

  module OrderedString = struct
    type t = string
    let compare = Pervasives.compare
  end

  module OrderedInt = struct
    type t = int
    let compare = (-)
  end

  module RowMap =
    FixMap.Make(SetBasedMap.Make(Baltree.Weight))(OrderedString)

  (* This small functor accepts some representation of labels and modifies it so that it is no longer case-sensitive.
     This is useful because our toy implementation has ``dynamic messages'', a feature which requires record and
     variant labels to belong to a single namespace, but our parser assumes they are in different name spaces, and
     uses case to distinguish between the two. *)

  module NoCase (X : Label.S) = struct

    type t = X.t

    let get s =
      X.get (String.lowercase s)

    let print =
      X.print

  end

  module Core = Core.Make
      (MlAlgebra)
      (Baltree.Weight)
      (Baltree.Weight)
      (TrieSetMap.Make(Baltree.Weight))
      (RowMap)
      (NoCase(Label.Simple))

  (* Prepare for the interesting work. *)

  open Core

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Definitions for the typechecker} *)

  (* Contexts are maps from identifiers to variables. *)

  type context = leaf IdMap.t

  (* Define a concrete data structure for type schemes. A type scheme is a context together with a body. *)

  type scheme = Scheme of context * leaf

  (* An environment maps identifiers to type schemes. *)

  type environment = scheme Env.t

  (* Now, let's provide Wallace with abstract views of schemes. *)

  module Scheme = struct

    (* This function defines an iterator over a specified type scheme. This is the only place where the sign
       associated to each entry point is defined. Here, each entry in the context is defined as negative, and the body
       is defined as positive. *)

    let iterator (Scheme(context, body)) action =
      IdMap.iter (fun _ term ->
	action false term
      ) context;
      action true body

    (* This function defines a mapper over a specified type scheme. *)

    let mapper (Scheme(context, body)) f =
      Scheme(IdMap.map f context, f body)

  end

  (* This function allows copying concrete type schemes. It calls Wallace's generic copying function. *)

  let copy scheme =
    Copy.scheme (Scheme.mapper scheme)

  (* This function simplifies a type scheme. *)

  let simplify scheme =
    let iterator = Scheme.iterator scheme
    and mapper = Scheme.mapper scheme in
    Garbage.collect iterator;
    Minimize.minimize iterator mapper

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Setting up an internal type term parser}

   To make the typing rules more readable and easier to modify, we use textual representations of types wherever
   possible. The parsing rules for these types are automatically produced by Gromit. Here is how the parser is
   set up for easy invocation. *)

  module Parse : sig

    val scheme: string -> string list -> scheme

  end = struct

    let digest entry string =

      let failure message =
	Printf.printf "Internal error while parsing ``%s''.\n" string;
	Printf.printf "%s.\n" message;
	flush stdout;
	exit(1) in

      let lexbuf = Lexing.from_string string in
      try
	entry MlAlgebra_lexer.token lexbuf
      with
      | MlAlgebra_lexer.Error (error, start_loc, end_loc) ->
	  failure (Printf.sprintf
	    "%s at characters %d-%d" error start_loc end_loc)
      | Parsing.Parse_error ->
	  failure "Syntax error"

    open MlAlgebra_parser

    let scheme body coercions =

      (* Parse the body and the constraints. *)

      let body = digest one_single_term body
      and coercions = List.map (digest one_single_coercion) coercions in

      (* Elaborate the abstract syntax tree into an internal structure. This is done in two steps: first, call
	 [Core.Translate.expression], which analyzes the whole scheme expression and returns a translation function;
	 second, apply this function to the scheme's body. *)

      let failure message =
	Printf.printf "Internal error while parsing some type scheme.\n";
	Printf.printf "%s.\n" message;
	flush stdout;
	exit(1) in

      try

	let Core.Translate.TransFun translate =
	  Core.Translate.expression (fun action ->
	    action MlAlgebra.Kind.KRegular body
          ) coercions in

	Scheme(IdMap.empty, translate true body)

      with
      | Core.Translate.Kind.Inconsistency ->
	  failure "The type scheme is ill-kinded"
      | Core.Translate.Kind.UnderSpecified name ->
	  failure (Printf.sprintf
	    "The kind of variable ``%s'' cannot be determined" name)
      | Core.Translate.Sort.Inconsistency ->
	  failure "The type scheme is ill-sorted"
      | Core.Translate.Sort.UnderSpecified ->
	  failure "Some term's sort cannot be determined"

  end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Printing types} *)

  (* Implement pretty-printers. All we have to do is supply token printers and instantiate Wallace's functors. *)

  module Print = struct

    module Internal = Print.Make
	(MlAlgebra)
	(Baltree.Weight)
	(Baltree.Weight)
	(RowMap)
	(Label.Simple)
	(Core)

    module Text = Internal
	(TextPrinter.Make(Core))
	(Token.Text)

    let text substitute (Scheme(context, body) as scheme) =
      let pscheme action =
	(* TEMPORARY print context too, if non-empty *)
	action true body;
	Format.printf "@ where:@ " in
      Text.scheme substitute pscheme (Scheme.iterator scheme);
      Format.print_flush()

    module TeX = Internal
	(TeXPrinter.Make(Core))
	(Token.TeX)

  end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Built-in environment}

   Here, we define the types of several primitive operations, which are built into the type inference engine.
  
   For simplicity, these types are defined textually. These definitions are parsed only once at startup, except those
   which depend on some textual parameter (such as record access/update operations), which are parsed every time a new
   value of the parameter shows up. (Memoization is used to avoid computing the same type scheme several times.) *)

let complex term coercions =
  simplify (Parse.scheme term coercions)

let simple term =
  complex term []

let memoize f =
  let table = Hashtbl.create 1023 in
  fun label ->
    try
      Hashtbl.find table label
    with Not_found ->
      let result = f label in
      Hashtbl.add table label result;
      result

let parameterized1 template =
  memoize (fun label ->
    simple (Printf.sprintf template label)
  )

let parameterized2 template =
  memoize (fun label ->
    simple (Printf.sprintf template label label)
  )

let parameterized3 template =
  memoize (fun label ->
    simple (Printf.sprintf template label label label)
  )

let constants = List.flatten (
  List.map (fun ct -> [
    Printf.sprintf "_%s" ct,
      simple ct;
    Printf.sprintf "_cmatch_%s" ct,
      simple (Printf.sprintf "(unit -> 'a) -> (%s -> 'a) -> (%s -> 'a)" ct ct)
  ])
  [ "int"; "unit"; "bool"; "float"; "char"; "string" ]
)

let builtin = List.fold_right (fun (x, sigma) env -> Env.bind_let x sigma env)
(constants @ [

  "_pair", simple "'a -> 'b -> 'a * 'b";
  "_uncurry", simple "('a -> 'b -> 'c) -> 'a * 'b -> 'c";

  "_rec", simple "('a -> 'a) -> 'a";

  "ref", simple "'a -> ('a, 'a) ref";
  ":=", simple "('a, 'b) ref -> 'a -> unit";
  "!", simple "('a, 'b) ref -> 'b";
  "_deref", simple "('a -> 'b) -> (0, 'a) ref -> 'b";

  "{}", simple "{ RAbs }";
  "@@", complex "{ 'phi1 } -> { 'phi2 } -> { 'phi3 }" [
    "if RAbs < 'phi1 then 'phi2 < 'phi3";
    "if RAbs < 'phi2 then 'phi1 < 'phi3";
    "if RPre < 'phi1 then 'phi2 < RAbs"
  ];
  "@", complex "{ 'phi1 } -> { 'phi2 } -> { 'phi3 }" [
    "'phi2 < REither 'alpha2";
    "if RAbs < 'phi2 then 'phi1 < 'phi3";
    "if RPre < 'phi2 then RPre 'alpha2 < 'phi3"
  ];
  "default", simple "'a -> { \\(RPre 'a) }";
  "_force_record", simple "{ 'a } -> unit";

  "_reject", simple "0 -> 0";
  "_vreject", simple " [ VAbs ] -> 0";

  (* [_creject], which is used to close pattern matchings on constants, cannot be given type $\bot\rightarrow\bot$,
     as [_reject]. Indeed, because our constant types are coarse, pattern matchings on constants are not extensible.
     Thus, giving such a type to [_creject] would cause every pattern matching on constants to be ill-typed. We must
     thus adopt the opposite solution, where every pattern matching on constants is well-typed, but is possibly
     incomplete. In a type system where exceptions are tracked, [_creject]'s type would indicate that it may raise
     [MatchFailure]. *)

  "_creject", simple "1 -> 0";

  "if", simple "bool -> 'a -> 'a -> 'a";

  "[||]", simple "('a, 'a) vect";
  "_vector_extend", simple "'b -> ('a, 'b) vect -> ('a, 'b) vect";

  "dispatch", complex "{ 'phi } -> [ 'psi ] -> 'b" [
    "'psi < VPre 'a";
    "if VPre < 'psi then 'phi < RPre ('a -> (\\'b))"
  ]
]) Env.empty

let scheme_VRecordAccess =
  parameterized1 "{ %s: RPre 'elem; 'rest } -> 'elem"
let scheme_VRecordUpdate =
  parameterized2 "{ %s: 'any; 'rest } -> 'a -> { %s: RPre 'a; 'rest }"
let scheme_VRecordRestrict =
  parameterized2 "{ %s: 'any; 'rest } -> { %s: RAbs; 'rest }"
let scheme_VRecordModify =
  parameterized3 "{ %s: 'any; 'fields } -> { %s: 'field; 'whatever } -> { %s: 'field; 'fields }"
let scheme_VRecordTest =
  memoize (fun label ->
    let body = Printf.sprintf
	"{ %s: 'phi; 'rest } -> ({ %s: RPre 'elem; 'rest_yes } -> 'yes) -> ({ %s: RAbs; 'rest_no } -> 'no) -> 'result"
	label label label in
    complex body [
      "'phi < REither 'elem";
      "if RPre < (\\'phi) then 'rest < 'rest_yes";
      "if RAbs < (\\'phi) then 'rest < 'rest_no";
      "if RPre < 'phi then 'yes < 'result";
      "if RAbs < 'phi then 'no < 'result"
    ]
  )

let scheme_VConstruct =
  parameterized1 "'a -> [ %s: VPre 'a; VAbs ]"
let scheme_VMatch =
  memoize (fun label ->
    let body = Printf.sprintf
	"('a -> 'yes) -> ([ %s: VAbs; 'rest ] -> 'result) -> [ %s: 'phi; 'rest ] -> 'result"
	label label in
    complex body [
      "'phi < VPre 'a";
      "if VPre < 'phi then 'yes < 'result"
    ]
  )
  
(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{The typechecker} *)

  (* The typechecker's core. This code reflects the type inference rules. *)
  (* TEMPORARY perform simplification! *)
  (* TEMPORARY allow marking schemes to know whether they are simplified already *)

  exception Failure of string

  let failure message =
    raise (Failure message)

  let context_meet context1 context2 =
    IdMap.fine_union Closure.meet context1 context2

  open MlAlgebra
  open InternalSyntax

  let rec infer env = function
    | VVar name -> (
	
	(* Look up this name in the environment. *)

	let binding = try
	  Env.lookup name env
	with Env.Unbound ->
	  failure (Printf.sprintf "Identifier %s is unbound" name) in

	(* Determine whether it is \verb+let+-bound or $\lambda$-bound. If it is \verb+let+-bound, retrieve its
	   type scheme from the environment, and take a fresh copy of it. If it is $\lambda$-bound, then its
	   type scheme is $\scheme{\context{x: \alpha}}{\beta}{\{ \alpha\leq\beta \}}$, where $\alpha$ and $\beta$
	   must be taken fresh. *)

	match binding with
	| Env.BindingLet (_, scheme) ->
	    copy scheme
	| Env.BindingLambda id ->
	    let alpha, beta = Fresh.pair Regular Kind.KRegular in
	    Scheme(IdMap.singleton id alpha, beta)

      )
    | VLambda (KVar name, expression) ->

	(* Typecheck the function's body, in an environment extended with a new $\lambda$-binding. *)

	let id, env = Env.bind_lambda name env in
	let Scheme(context, body) = infer env expression in

	(* Find the entry associated with this identifier in the inferred context, and extract it. If there isn't
	   such an entry, use type $\top$, since in such a case, the function does not use its argument, and allows
	   it to have any type. *)

	let domain, context = try
	  IdMap.lookup_and_remove id context
	with Not_found ->
	  Fresh.hi Regular Term.TTop, context in

	(* Build the $\lambda$-expression's type scheme. Its body is an arrow type. *)

	Scheme(context, Fresh.lo Regular (Term.TArrow(domain, body)))

    | VLambda (pat, expression) ->

	(* A special case of the above. Because the pattern [pat] binds no variable, no entry is added to the
	   context. *)

	let Scheme(context, body) = infer env expression in

	let bound = match pat with
	| KWildcard ->
	    Term.TTop
	| KUnit ->
	    Term.TUnit
	| KVar _ ->
	    assert false in

	Scheme(context, Fresh.lo Regular (
	       Term.TArrow(Fresh.hi Regular bound, body)
        ))

    | VApp (expression1, expression2) ->

	(* Infer each sub-expression's type scheme. *)

	let Scheme(context1, body1) = infer env expression1
	and Scheme(context2, body2) = infer env expression2 in

	(* Create a constraint which forces the left-hand expression to have an $\rightarrow$ type, and (at the same
	   time) extracts its result type. The whole expression's context is the meet of the two contexts, which means
	   that each sub-expression's requirements about the environment is taken into account. *)

	let resultm, resultp = Fresh.pair Regular Kind.KRegular in
	Closure.hi body1 (Term.TArrow(body2, resultm));
	Scheme(context_meet context1 context2, resultp)

    | VLet (pattern, expression1, expression2) ->

	(* Infer the first expression's type scheme, within the current environment. *)

	let (Scheme(context1, body1)) as scheme1 = infer env expression1 in

	(* Infer the second expression's type scheme, within an environment augmented with a new binding, if the
	   pattern involved a name, and within the same environment, otherwise.

	   If the pattern was a [unit] pattern, take that constraint into account. *)

	let env =
	  match pattern with
	  | KWildcard ->
	      env
	  | KVar name ->
	      Env.bind_let name scheme1 env
	  | KUnit ->
	      Closure.hi body1 Term.TUnit;
	      env in

	let Scheme(context2, body2) = infer env expression2 in

	(* The result is essentially the second expression's type scheme, with a slight difference: the constraints
	   put by the first expression on its context are also taken into account. *)

	Scheme(context_meet context1 context2, body2)

    (* We could write a dedicated, optimized type inference rule for each language construct, rather than viewing many
       advanced constructs as syntactic sugar for more basic expressions. This would also be slightly more efficient;
       however, the type inference engine would become much more difficult to maintain. The current set-up allows any
       modifications to the basic typechecking rules to be transparently reflected in the more advanced rules, since
       the latter are derived. *)

    | VRecordAccess label ->
	copy (scheme_VRecordAccess label)
    | VRecordUpdate label ->
	copy (scheme_VRecordUpdate label)
    | VRecordRestrict label ->
	copy (scheme_VRecordRestrict label)
    | VRecordModify label ->
	copy (scheme_VRecordModify label)
    | VRecordTest label ->
	copy (scheme_VRecordTest label)

    | VConstruct label ->
	copy (scheme_VConstruct label)
    | VMatch label ->
	copy (scheme_VMatch label)

end

