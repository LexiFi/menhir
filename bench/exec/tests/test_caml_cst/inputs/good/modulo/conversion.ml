(* $Header: /home/yquem/cristal/fpottier/cvs/modulo/conversion.ml,v 1.25 2007/06/30 08:38:12 fpottier Exp $ *)

(* Conversion from external to internal syntax. *)

(* TEMPORARY check linearity of patterns/type definitions *)
(* TEMPORARY type abbreviations associated with record fields are currently accessible to the user,
             because they are introduced in the same environment *)

open Standard
open Id
open Syntax
open InternalSyntax
open KindUnification
open Error

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Types. *)

let appc t1 (ch, t2) =
  ITApp (t1, ch, t2)

let absc c id t =
  ITAbs (c, id, t)

let abs id t =
  absc "" id t

let arrow t1 t2 =
  ITArrow (t1, t2)

let marrow tl t =
  List.fold_right arrow tl t

let forall quantifiers body =
  List.fold_right (fun (q, kind) body ->
    ITForall (kind, abs q body)
  ) quantifiers body

let app body formals =
  List.fold_left (fun body (ch, param, kind) ->
    appc body (ch, ITVar param)
  ) body formals

(* TEMPORARY is kind checking partially redundant with translation of types to internal form? *)
let rec convert_type = function
  | TypVar id ->
      ITVar id
  | TypApp (t1, ch, t2) ->
      ITApp (convert_type t1, ch, convert_type t2)
  | TypAbs (ch, id, kind, t) ->
      ITAbs (ch, id, convert_type t)
  | TypArrow (t1, t2) ->
      arrow (convert_type t1) (convert_type t2)
  | TypRecord t ->
      ITRecord (convert_type t)
  | TypRowCons ((id, actualsopt), tl) ->
      let hd =
	match actualsopt with
	| None ->
	    ITAbsent
	| Some actuals ->
	    ITPresent (List.fold_left (fun t1 (ch, t2) ->
	      appc t1 (ch, convert_type t2)
            ) (ITVar id) actuals)
      in
      ITRowCons (id, hd, convert_type tl)
  | TypRowEmpty ->
      ITRowUniform ITAbsent
  | TypTuple tl ->
      ITTuple (List.map convert_type tl)
  | TypInteger ->
      ITInteger

let unit =
  ITTuple []

let int =
  ITInteger

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Helpers. *)

let quantifiers_of_formals formals =
  List.map (fun (channel, id, kind) ->
    id, kind
  ) formals

let etapp quantifiers e =
  List.fold_left (fun e (_, kind) -> IETypApp (e, kind)) e quantifiers

let etabs anonymous quantifiers e =
  List.fold_right (fun (q, kind) e ->
    let id = if anonymous then None else Some q in
    IETypAbs (id, kind, e)
  ) quantifiers e

let ptabs quantifiers p =
  List.fold_right (fun (_, kind) p -> IPTypAbs (p, kind)) quantifiers p

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Introducing constants. *)

(* [introduce_constant c quantifiers t e] introduces a constant named [c],
   with a polymorphic type described by [quantifiers] and [t], for use within
   [e]. The identifier [c] is [let]-bound within [e]. *)

let introduce_constant (c, quantifiers, t) e =

  (* Introduce the constant, with an explicit polymorphic type. *)

  IEAbs (c, Some (forall quantifiers t),

  (* Rebind it generically, so universal elimination will not be needed at the
     application site. *)

  IEBinding (IBLetGen (ref Polymorphic, IEVar c, etapp quantifiers (IEVar c) ),

  e))

let introduce_constants =
  List.fold_right introduce_constant

let declare constants (IProgram (d, e)) =
  IProgram (d, introduce_constants constants e)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Support for kind checking. *)

let bindf formals env =
  List.fold_left (fun env (_, id, kind) ->
    (id, kind) :: env
  ) env formals

let bindq quantifiers env =
  List.fold_left (fun env (id, kind) ->
    (id, kind) :: env
  ) env quantifiers

let defaultf formals =
  List.iter (fun (_, _, kind) ->
    default kind
  ) formals

let defaultq quantifiers =
  List.iter (fun (_, kind) ->
    default kind
  ) quantifiers

let arrowf formals kind =
  List.fold_right (fun (p, _, k1) k2 ->
    KindUnification.arrow p k1 k2
  ) formals kind

let iktcon () =
  Printf.eprintf "*** Invalid type constraint.\n"

let ikrfdecl label () =
  Printf.eprintf "*** Invalid record field declaration (%s):\n" label

let ikdtdecl name constructor () =
  Printf.eprintf "*** Invalid data type declaration (%s, case %s):\n" name constructor

let iktdecl id () =
  Printf.eprintf "*** Invalid type definition (%s):\n" id

let kmismatch k1 k2 () =
  Printf.eprintf "*** Kinds %s and %s do not match.\n" (print k1) (print k2)

let unboundt id () =
  Printf.eprintf "*** The type (or record field) %s is unbound.\n" id

let kprotect msg action =
  try
    action()
  with
  | Error (k1, k2) ->
      abort (msg >> kmismatch k1 k2)
  | KindInference.UnboundTypeIdentifier id ->
      abort (msg >> unboundt id)

let kinfer msg env t =
  kprotect msg (fun () -> KindInference.infer env t)

let kcheck msg env t kind =
  kprotect msg (fun () -> KindInference.check env t kind)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Information (obtained from the toplevel definitions) has to be passed down
   into the conversion functions.

   For every record field, we keep track of the list of universal quantifiers
   that should be introduced (resp. eliminated) when writing
   (resp. reading).

   We also keep track of a kind environment, i.e. a mapping from type names
   to kinds, used for kind inference. *)

exception UndefinedRecordField of id

module Info = struct

  type t = {
      quantifiers: (id * kind) list IdMap.t;
      environment: (id * kind) list
    } 

  let empty = {
    quantifiers = IdMap.empty;
    environment = []
  } 

  (* Recording and accessing quantifier information. *)

  let define x q info = {
    info with quantifiers = IdMap.add x q info.quantifiers
  } 

  let lookup x info =
    try
      IdMap.find x info.quantifiers
    with Not_found ->
      raise (UndefinedRecordField x)

  (* Augmenting and accessing the kind environment. *)

  let bind id kind info = {
    info with environment = (id, kind) :: info.environment
  }

  let bindq quantifiers info = {
    info with environment = bindq quantifiers info.environment
  } 

  let environment info =
    info.environment

end

let initial_info =
  Info.empty

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Support for exceptions. *)

(* [exn t] produces an application of the exception type [exn] to [t]. *)

let exn t =
  ITApp (ITVar "exn", "", t)

(* Support for exceptions is provided entirely by means of predefined types
   and operations. This function introduces them. *)

let declare_exception_facilities (IProgram (d, e)) =

  IProgram (d,

  (* Introduce the type of exceptions. *)

  IEForall ([ "exn" ],

  (* Introduce the primitive operations. *)

  introduce_constants [

    "exception", [ ("a", star) ],
	         arrow unit (exn (ITVar "a"));
    "raise",     [ ("a", star); ("b", star) ],
	         marrow [ exn (ITVar "a"); ITVar "a" ] (ITVar "b");
    "try",       [ ("a", star); ("b", star) ],
                 marrow [ arrow unit (ITVar "b"); exn (ITVar "a"); arrow (ITVar "a") (ITVar "b") ] (ITVar "b")

  ] e))

(* Add the type [exn] to the predefined kind environment. *)

let initial_info =
  Info.bind "exn" (KindUnification.arrow "" star star) initial_info

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Support for integers. *)

let declare_integer_facilities =
  declare [
    "+", [], marrow [ int; int ] int;
    "-", [], marrow [ int; int ] int;
    "=", [], marrow [ int; int ] int; (* TEMPORARY *)
  ] 

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Reading and writing record fields. *)

(* [inject x] is the name of the primitive injection operation for the type
   abbreviation associated with the record field [x]. *)

let inject x = "->" ^ x

(* [pack info x e] prepares the expression [e] to be written into the
   record field [x]. We introduce an appropriate number of universal
   quantifiers around [e], then inject it into the type abbreviation
   associated with the field [x]. *)

let pack info x e =
  let quantifiers = Info.lookup x info in
  IEApp (IEVar (inject x), etabs true quantifiers e)

(* [unpack info x p] prepares the pattern [p] to be read from the record field
   [x]. We eliminate the type abbreviation associated with the field [x],
   eliminate an appropriate number of universal quantifiers, then allow the
   result to be matched against [p]. *)

let unpack info x p =
  let quantifiers = Info.lookup x info in
  IEApp (IPConstant (IEVar (inject x)),
	 ptabs quantifiers p)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Patterns. *)

(* [convert_pattern info pat] converts the pattern [pat] to internal
   syntax. Attributes are ignored. *)

let rec convert_pattern info = function
  | PWildcard ->
      IPWildcard
  | PVar (_, id) ->
      IEVar (id)
  | PAlias (_, id, pat) ->
      IEChoice [ IEVar id; convert_pattern info pat ]
  | PRecord (idpatl, pat) ->
      IERecordExtend (
        List.map (fun (id, pat) ->
	  IRElement (id, unpack info id (convert_pattern info pat))
        ) idpatl,
        convert_pattern info pat
      )
  | PRecordEmpty ->
      IERecordEmpty
  | PTuple patl ->
      IETuple (List.map (convert_pattern info) patl)
  | PData (id, patl) ->
      List.fold_left (fun accu pat ->
	IEApp (accu, convert_pattern info pat)
      ) (IPConstant (IEVar id)) patl
  | PInteger k ->
      IEInteger k
  | PTypeConstraint (pat, t) ->
      kcheck iktcon (Info.environment info) t star;
      IETypeConstraint (convert_pattern info pat, convert_type t)

(* Gathering lists of exported identifiers. *)

let rec exports_pat accu = function
  | PWildcard
  | PVar (Binds, _) ->
      accu
  | PVar (BindsAndDefines, id) ->
      id :: accu
  | PAlias (Binds, _, pat)
  | PTypeConstraint (pat, _) ->
      exports_pat accu pat
  | PAlias (BindsAndDefines, id, pat) ->
      exports_pat (id :: accu) pat
  | PRecord (idpatl, pat) ->
      List.fold_left (fun accu (_, pat) ->
	exports_pat accu pat
      ) (exports_pat accu pat) idpatl
  | PRecordEmpty
  | PInteger _ ->
      accu
  | PTuple patl
  | PData (_, patl) ->
      List.fold_left exports_pat accu patl

let exports_vdef accu (_, pat, _) =
  exports_pat accu pat

let exports_binding accu = function
  | BindValue vdefs
  | BindRecValue vdefs ->
      List.fold_left exports_vdef accu vdefs
  | BindImport (BindsAndDefines, xs, _) ->
      xs @ accu
  | BindType _
  | BindImport (Binds, _, _) ->
      accu

let check noexports exports =
  if noexports & (exports <> []) then
    failwith "[export] disallowed here" (* TEMPORARY *)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Bindings. *)

exception NoType

(* The interesting aspect of value definitions is the possible presence of
   explicit quantifiers. We explicitly require the expression [e] to be
   polymorphic in the named quantifiers specified by the user, if any. Thus,
   these type identifiers are in scope (and rigid) within [e]. *)

let rec convert_value_definition info (quantifiers, pat, e) =
  let info = Info.bindq quantifiers info in
  let pat' = convert_pattern info pat
  and e' = IEForall (List.map fst quantifiers, convert_expr info e) in
  defaultq quantifiers;
  pat', e'
  (* TEMPORARY placing the Forall around the expression, and leaving the pattern out, looks like a bug to me. *)

(* For recursive value definitions, we check whether the expression carries
   enough type annotations to fully determine its type. (This is an
   incomplete, syntactic check.) If so, we consider it is explicitly typed,
   otherwise it is implicitly typed. We add the definition to the appropriate
   category. *)

      (* TEMPORARY si on d'efinit un champ, l'annotation peut etre deduite du
	 nom du champ! Mais il y a risque de capture si on le fait `a ce niveau.
         Car: (i) on doit introduire un exists qui peut capturer des noms dans e
             (ii) le contexte courant peut capturer des noms dans le type t du champ *)

and find_type = function
  | ETypeConstraint (e, t) ->
      convert_type t
  | EAbs (PTypeConstraint (p, t), e) ->
      arrow (convert_type t) (find_type e)
  | _ ->
      raise NoType

and convert_recursive_value_definition info (explicit, implicit) (quantifiers, pat, e) =
  match pat, e with
  | PVar (_, id), EAbs _ -> (
      try
	let e' = convert_expr (Info.bindq quantifiers info) e in
	defaultq quantifiers;
	(id, (List.map fst quantifiers, find_type e), e') :: explicit, implicit
      with NoType ->
	if (quantifiers <> []) then
	  failwith "incomplete [forall] specification";
	explicit, (id, convert_expr info e) :: implicit
    )
  | _, EAbs _ ->
      failwith "The left-hand side of a recursive definition must be a variable"
  | _, _ ->
      failwith "The right-hand side of a recursive definition must be a function"

and convert_type_definition info (id, t) =
  let kind = kinfer (iktdecl id) (Info.environment info) t in
  Info.bind id kind info, (id, convert_type t)

and convert_import info xs e =
  match xs, e with
  | ([] | [ _ ]), _
  | _, EVar _ ->
      List.map (fun x ->
	IBLetGen (ref Polymorphic,
		  IERecordExtend ([ IRElement (x, unpack info x (IEVar x)) ], IPWildcard),
		  convert_expr info e)
      ) xs
  | _, _ ->

      (* Introduce a [let] binding to avoid evaluating (or type-checking) [e]
	 several times. The use of a fixed variable name is okay because it
	 cannot capture variables in [e], nor be captured between its
	 definition and its uses. *)

      IBLetGen (ref Polymorphic, IEVar "*", convert_expr info e) ::
      convert_import info xs (EVar "*")

and convert_binding info = function
  | BindValue [ vdef ] ->
      let pat, e = convert_value_definition info vdef in
      info, [ IBLetGen (ref Polymorphic, pat, e) ]
  | BindValue vdefs ->
      let patterns, expressions =
	List.split (List.map (convert_value_definition info) vdefs) in
      info, [ IBLetGen (ref Polymorphic, IETuple patterns, IETuple expressions) ]
  | BindRecValue vdefs ->
      let explicit, implicit =
	List.fold_left (convert_recursive_value_definition info) ([], []) vdefs in
      info, [ IBLetRec (explicit, implicit) ]
  | BindType tdefs ->
      let info, tdefs = mapfold convert_type_definition info tdefs in
      info, [ IBLetType tdefs ]
  | BindImport (_, xs, e) ->
      info, convert_import info xs e

and convert_binding_noexports info binding =
  check true (exports_binding [] binding);
  convert_binding info binding

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Record bindings.

   Record bindings allow the [export] keyword to appear in front of any
   identifier. In that case, we must not only bind the identifier, but also
   define a record field. *)

and export info x = [

  (* We assume that the identifier [x] is defined. We package it for writing
     into the record, and bind the result to [x] again. *)

  IRBinding (IBLetGen (ref Polymorphic, IEVar x, pack info x (IEVar x)));

  (* We then define the field. *)

  IRElement (x, IEVar x);

  (* Before proceeding, we unpack [x] and bind the result to [x] again. The
     effect is the same as if [x] had just been read out of the record. In
     particular, this prevents [x] from receiving a type which is more general
     than the field's type (which would be possible if we instead made [x]'s
     original value directly available). *)

  IRBinding (IBLetGen (ref Polymorphic, unpack info x (IEVar x), IEVar x))

]

and convert_record_binding info binding =

  (* Introduce the appropriate bindings, then define every exported
     identifier. *)

  let info, bindings = convert_binding info binding in
  info, mapconcat (fun b -> IRBinding b) bindings
    (flatmap (export info) (exports_binding [] binding))

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Expressions. *)

and convert_expr info = function
  | EVar x ->
      IEVar x
  | EAbs (PVar (Binds, x), e) ->
      IEAbs (x, None, convert_expr info e)
  | EAbs (pat, e) ->
      check true (exports_pat [] pat);

      (* Introduce a [let] binding to avoid dealing with patterns in
	 abstractions internally. The use of a fixed variable name is okay
	 because it cannot capture variables in [e], nor be captured between
	 its definition and its use. *)

      IEAbs ("*", None,
	     IEBinding (IBLetGen (ref Polymorphic, convert_pattern info pat, IEVar "*"),
			convert_expr info e))

  | EMatch (e, pelist) ->
      IEApp (
        IEChoice (
          List.map (fun (pat, e) -> convert_expr info (EAbs (pat, e))) pelist
        ),
        convert_expr info e
      )
  | EApp (e1, e2) ->
      IEApp (convert_expr info e1, convert_expr info e2)
  | EBinding (b, e) ->
      let info, bindings = convert_binding_noexports info b in
      List.fold_right (fun b e -> IEBinding (b, e)) bindings (convert_expr info e)
  | ERecordEmpty ->
      IERecordEmpty
  | ERecordAccess (x, e) ->
      convert_expr info (
        EBinding (
          BindValue [[], PRecord ([ x, PVar (Binds, x) ], PWildcard), e],
          EVar x
	)
      )
  | ERecordExtend (bindings, e) ->
      let info, bindings = mapfold convert_record_binding info bindings in
      IERecordExtend (
        List.flatten bindings,
        convert_expr info e
      )
  | ERecordUpdate (e1, x, e2) ->
      IERecordUpdate (convert_expr info e1, x, pack info x (convert_expr info e2))
  | EExists (quantifiers, e) ->
      let e = convert_expr (Info.bindq quantifiers info) e in
      defaultq quantifiers;
      IEExists (List.map fst quantifiers, e)
  | ETuple elist ->
      IETuple (List.map (convert_expr info) elist)
  | EDisplay e ->
      IDisplay (convert_expr info e)
  | EData id ->
      IEVar id
  | EInteger k ->
      IEInteger k
  | ETypeConstraint (e, t) ->
      kcheck iktcon (Info.environment info) t star;
      IETypeConstraint (convert_expr info e, convert_type t)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Declarations and programs. *)

let convert (decls, e) =

      (* 1. On the way down, check that declarations are well-formed and
	 accumulate information to reflect their effect. *)

  let rec walk info = function
  | [] ->
      IProgram ([], IDisplay (convert_expr info e))

  | DeclField (label, mut, formals, (quantifiers, body)) :: decls ->

      (* Infer every parameter's and quantifier's kind, and make sure that the
	 body has kind [*]. If the kinds are under-specified, instantiate the
	 remaining kind variables with default values, as in Haskell. *)

      kcheck (ikrfdecl label) (bindq quantifiers (bindf formals (Info.environment info))) body star;
      defaultf formals;
      defaultq quantifiers;

      (* Introduce a type abbreviation associated with the record field into
	 the kind environment. Also, remember which quantifiers this field
	 requires. *)

      let info = Info.bind label (arrowf formals star) (Info.define label quantifiers info) in

      (* Walk down the rest of the program. *)

      let IProgram (d, e) = walk info decls in

      (* Translate the declaration. *)

      IProgram (

	(* Generate a field declaration. *)

        (IDeclareField (label, if mut then Mutable else Immutable)) :: d,

	(* In front of the expression, introduce a new abstract type, whose name
	   coincides with the field's name. *)

	IEForall ([ label ],

	(* Introduce an injection from the concrete type intro the abstract one. *)

	let collapsed = app (ITVar label) formals
	and expanded = forall quantifiers (convert_type body) in

	introduce_constant (inject label, quantifiers_of_formals formals, arrow expanded collapsed)

	e))

  | DeclData (name, formals, cases) :: decls ->
    (* TEMPORARY check that constructor names are distinct *)

      (* Introduce the data type's name in the kind environment. This has to
	 be done up front, since data types declarations are recursive. *)

      let info = Info.bind name (arrowf formals star) info in

      (* Infer every parameter's and quantifier's kind, and make sure that
	 every argument of every constructor has kind [*]. If the kinds are
	 under-specified, instantiate the remaining kind variables with
	 default values, as in Haskell. *)

      let env = bindf formals (Info.environment info) in
      List.iter (fun (constructor, ts) ->
	List.iter (fun t ->
	  kcheck (ikdtdecl name constructor) env t star
	) ts
      ) cases;
      defaultf formals;

      (* Walk down the rest of the program. *)

      let IProgram (d, e) = walk info decls in

      (* Translate the declaration. *)

      IProgram (
        d,

	(* In front of the expression, introduce a new abstract type. *)

        IEForall ([ name ],

	(* Introduce the data constructors. *)

	let head = app (ITVar name) formals in

	List.fold_right (fun (constructor, args) e ->

	  introduce_constant (
	    constructor,
	    quantifiers_of_formals formals,
	    List.fold_right arrow (List.map convert_type args) head
	  ) e

	) cases e))

  in
  declare_exception_facilities (
  declare_integer_facilities (
    walk initial_info decls
  ))

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Abstract syntax for inferred types. This syntax roughly corresponds to
   [Syntax.typ].

   Some internal built-in types, such as [Forall], [Present] and [Absent],
   could be entirely hidden if only top-level types were printed. But, if
   we print the types of arbitrary sub-expressions of the converted program,
   then they may appear. For the same reason, the constructor [Raw] is added
   to the type [content] below.

   One extra production has been added for meta-variables. Note that
   meta-variables and variables must not be confused, since the latter
   may actually bind free variables. *)

type typ =
  | OTVar of id
  | OTApp of typ * channel * typ
  | OTAbs of channel * id * typ
  | OTRowCons of id * content * typ
  | OTRowUniform of typ

  (* Built-in types. *)

  | OTForall of kind
  | OTPresent of typ
  | OTAbsent
  | OTRecord of typ
  | OTArrow of typ * typ
  | OTTuple of typ list
  | OTInteger

  (* Additional productions. *)

  | OTMetaVariable of id

and content =
  | Minus
  | Plus of actuals
  | Raw of typ

and actuals =
    (channel * typ) list

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* A pretty-printer for our output syntax. *)

module P = Tree.Make (struct

  type tree = typ

  type label =
    | LAbstraction
    | LApplicationL
    | LApplicationR
    | LRowCompL
    | LRowCompR
    | LRowUniform
    | LRecord
    | LArrowL
    | LArrowR
    | LTuple

  open Tree

  let end_row = function
    | OTRowUniform OTAbsent ->
	[]
    | t ->
	[ Token "; "; Son (LRowCompR, t) ]

  let rec describe = function
    | OTMetaVariable x
    | OTVar x ->
	[ Token x ]
    | OTAbs (x, p, t) ->
	[ Token ("\\" ^ p ^ (if p = "" then "" else ":") ^ x ^ "."); Son (LAbstraction, t) ]
    | OTApp (t1, p, t2) ->
	[ Son (LApplicationL, t1); Token (" " ^ p ^ (if p = "" then "" else ":")); Son (LApplicationR, t2) ]
    | OTRowCons (label, Minus, t2) ->
	[ Token ("-" ^ label) ] @ (end_row t2)
    | OTRowCons (label, Plus actuals, t2) ->
	( Token ("+" ^ label) ) :: (
        Standard.flatmap (fun (p, t) ->
	  [ Token (" " ^ p ^ (if p = "" then "" else ":")); Son (LApplicationR, t) ]
        ) actuals) @
	(end_row t2)
    | OTRowCons (label, Raw t1, t2) ->
	[ Token label; Token ": "; Son (LRowCompL, t1) ] @ (end_row t2)
    | OTRowUniform t ->
	[ Token "!"; Son (LRowUniform, t) ]
    | OTForall _ -> (* TEMPORARY *)
	[ Token "Forall" ]
    | OTPresent t ->
	[ Token "Present "; Son (LApplicationR, t) ]
    | OTAbsent ->
	[ Token "Absent" ]
    | OTRecord (OTRowUniform OTAbsent) ->
	[ Token "{}" ]
    | OTRecord ((OTRowCons _ | OTRowUniform _) as t) ->
	[ Token "{"; Son (LRecord, t); Token "}" ]
    | OTRecord t ->
	[ Token "{ "; Son (LRecord, t); Token " }" ]
    | OTArrow (t1, t2) ->
	[ Son (LArrowL, t1); Token " -> "; Son (LArrowR, t2) ]
    | OTTuple [] ->
	[ Token "()" ]
    | OTTuple [ _ ] ->
	assert false
    | OTTuple (t :: ts) ->
	[ Token "("; Son (LTuple, t) ] @
	flatmap (fun t -> [ Token ", "; Son (LTuple, t) ]) ts @
        [ Token ")" ]
    | OTInteger ->
	[ Token "int" ]

  let parenthesize label tree =
    match label, tree with
    | (LAbstraction | LApplicationR | LRecord), (OTRowCons _ | OTRowUniform _) ->
	angle_brackets
    | (LRecord | LTuple), _ ->
	nothing
    | LRowCompR, (OTRowCons _ | OTRowUniform _) ->
	nothing
    | (LApplicationL | LRowCompL | LRowUniform),
      (OTRowCons _ | OTRowUniform _ | OTRecord _ | OTArrow _)
    | LApplicationL, (OTAbsent | OTPresent _) ->
	assert false

    | _, OTMetaVariable _
    | _, OTVar _
    | _, OTForall _ (* TEMPORARY *)
    | _, OTAbsent
    | _, OTRecord _
    | _, OTTuple _
    | _, OTInteger ->
	nothing

    | LRowUniform, _ ->
	parentheses
    | LRowCompL, _
    | LRowCompR, _ ->
	nothing

    | LArrowR, OTArrow _ ->
	nothing
    | (LArrowR | LArrowL), OTApp _ ->
	nothing
    | (LArrowR | LArrowL), _ ->
	parentheses

    | LAbstraction, _ ->
	nothing
    | _, OTAbs _ ->
	parentheses

    | LApplicationL, OTApp _ ->
	nothing
    | LApplicationR, (OTApp _ | OTArrow _ | OTPresent _) ->
	parentheses

end)

let print =
  P.print

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Converting types back. *)

open Boil

exception UnexpectedForm

let rec complete f arity spine length =
  if length < arity then
    OTAbs ("", "x", complete f arity (spine @ [ "", OTVar "x" ]) (length + 1))
  else begin
    assert (length = arity);
    f spine
  end

let complete f arity spine =
  complete f arity spine (List.length spine)

let apply t spine =
  List.fold_left (fun a (p, b) -> OTApp (a, p, b)) t spine

let rec invert spine = function
  | IOTApp (t1, p, t2) ->
      invert ((p, invert [] t2) :: spine) t1
  | IOTVar x ->
      apply (OTVar x) spine
  | IOTMetaVariable x ->
      apply (OTMetaVariable x) spine
  | IOTAbs (p, x, t) ->
      apply (OTAbs (p, x, invert [] t)) spine
  | IOTRowCons (x, hd, tl) ->
      assert (spine = []);
      invert_row_cons x hd tl
  | IOTRowUniform t ->
      assert (spine = []);
      OTRowUniform (invert [] t)
  | IOTForall k ->
      OTForall k
  | IOTPresent ->
      complete (function [ "", t ] -> OTPresent t | _ -> assert false) 1 spine
  | IOTAbsent ->
      OTAbsent
  | IOTRecord ->
      complete (function [ "", t ] -> OTRecord t | _ -> assert false) 1 spine
  | IOTArrow ->
      complete (function [ "", t1; "", t2 ] -> OTArrow (t1, t2) | _ -> assert false) 2 spine
  | IOTTuple k ->
      complete (function ts -> OTTuple (List.map snd ts)) k spine
  | IOTInteger ->
      OTInteger

and invert_row_cons x hd tl =
  try
    match hd with
    | IOTAbsent ->
	OTRowCons (x, Minus, invert [] tl)
    | IOTApp (IOTPresent, p, hd) ->
	assert (p = "");
	OTRowCons (x, Plus (params [] hd), invert [] tl)
    | _ ->
	raise UnexpectedForm
  with UnexpectedForm ->
    OTRowCons (x, Raw (invert [] hd), invert [] tl)

and params accu = function
  | IOTVar _ -> (* must be the innermost type abbreviation *)
      accu
  | IOTApp (t1, p, t2) ->
      params ((p, invert [] t2) :: accu) t1
  | _ ->
      raise UnexpectedForm

let invert t =
  invert [] t

