(* This file was generated from src/coreSyntax.mla. Do not edit! *)

module Identifier = PIdentifier

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

module Var = AlphaLib.Atom.Make(Identifier)

module Raw = struct

type var =
  Identifier.t

 and program = 
  | PEmpty of ( Position.t )
  | PConsComponent of pcons_abs

 and component = 
  | CValue of ids * optional_lformula * lterm
  | CRecValue of ids * optional_lformula * lterm
  | CTypeDef of tid * kind * type_definition
  | CFact of fact_status * lformula
  | CPredicate of pid * predicate_definition
  | CLogicFunction of pid * logic_function_definition

 and logic_function_definition = 
  | LFInductive of lfi_abs
  | LFAbbrev of lformula

 and logic_clause = 
  logic_clause_abs

 and fact_status = 
  | Lemma of ( UniqueIdentifier.t )
  | Axiom of ( UniqueIdentifier.t )

 and id = 
  var

 and ids = 
  id list

 and tid = 
  var

 and pid = 
  var

 and term = 
  | EId of var
  | EKApp of var * lterm list
  | EApp of lterm * lformula list * lterm list
  | ELam of fun_abs
  | ELet of let_abs
  | ELetRec of letrec_abs
  | ECase of lterm * clause list
  | EPrimitive of primitive
  | EAnnot of lterm * term_type
  | EForallTys of eforalltys_abs
  | EExistsTys of eexiststys_abs
  | EProd of lterm list
  | EIf of lterm * lterm * lterm
  | EAbsurd
  | EAssert of lformula * lterm
  | ELetLogic of let_logic_abs
  | EDeferred

 and primitive = 
  | PInt of ( int )
  | PTrue
  | PFalse

 and logic_bindings = 
  bindings * optional_lformula

 and logic_arguments = 
  fbindings

 and bindings = 
  binding list

 and binding = 
  id * term_type

 and function_output = 
  function_output_abs

 and clause = 
  clause_abs

 and pattern = 
  | PVar of id
  | PApp of constructor * pattern list

 and constructor = 
  var

 and term_type = 
  | TPrimitive of primitive_type
  | TVar of var
  | TArrow of term_type * term_type
  | TProd of term_type list
  | TApp of var * term_type list

 and primitive_type = 
  | TInt
  | TBool
  | TUnit

 and type_scheme = 
  | TScheme of scheme_abs

 and type_parameters = 
  type_parameter list

 and type_parameter = 
  var

 and kind = 
  | KStar
  | KArrow of kind list

 and type_definition = 
  | DAlgebraic of type_parameters * dataconstructor_definition list
  | DeferredType

 and dataconstructor_definition = 
  var * term_type

 and optional_lformula = 
  | ImplicitFormula of ( Position.t )
  | ExplicitFormula of lformula

 and formula = 
  | FTrue
  | FFalse
  | FVar of var
  | FLam of lfun_abs
  | FForallTys of fforalltys_abs
  | FExistsTys of fexiststys_abs
  | FForall of lforall_abs
  | FExists of lexists_abs
  | FEq of lformula * lformula
  | FApp of lformula * lformula list
  | FKApp of var * lformula list
  | FProd of lformula list
  | FPrimitive of formula_primitive
  | FAnnot of lformula * formula_type
  | FDeferred

 and formula_primitive = 
  | Pre
  | Post
  | PAnd
  | POr
  | PEquiv
  | PImply
  | PNot
  | PLessThan
  | PGreaterThan
  | PLessEqualThan
  | PGreaterEqualThan
  | PAdd
  | PSub
  | PMult
  | PDiv
  | PNeg
  | PEPrimitive of primitive

 and fbindings = 
  fbinding list

 and fbinding = 
  var * formula_type

 and trigger = 
  var

 and formula_type = 
  | FTProp
  | FTVar of var
  | FTArrow of formula_type * formula_type
  | FTCArrow of formula_type * formula_type
  | FTProd of formula_type list
  | FTPrimitive of primitive_type
  | FTApp of var * formula_type list

 and formula_type_scheme = 
  | FTScheme of formula_scheme_abs

 and predicate_definition = 
  | PDAbbrev of lformula
  | PDInductive of formula_type_scheme option * lformula list

 and lterm = 
  { tpos: ( Position.t ); tvalue: term }

 and lformula = 
  { fpos: ( Position.t ); fvalue: formula }

 and pcons_abs = 
  ( Position.t ) * component * program

 and lfi_abs = 
  ids * logic_clause list

 and logic_clause_abs = 
  pattern * lformula

 and fun_abs = 
  logic_arguments * logic_bindings * function_output * lterm

 and let_abs = 
  ids * optional_lformula * lterm * lterm

 and letrec_abs = 
  ids * optional_lformula * lterm * lterm

 and eforalltys_abs = 
  type_parameters * lterm

 and eexiststys_abs = 
  type_parameters * lterm

 and let_logic_abs = 
  fbindings * lformula * lterm

 and function_output_abs = 
  logic_bindings

 and clause_abs = 
  pattern * lterm

 and scheme_abs = 
  type_parameters * term_type

 and lfun_abs = 
  fbindings * lformula

 and fforalltys_abs = 
  type_parameters * lformula

 and fexiststys_abs = 
  type_parameters * lformula

 and lforall_abs = 
  fbindings * trigger list * lformula

 and lexists_abs = 
  fbindings * lformula

 and formula_scheme_abs = 
  type_parameters * formula_type

end

module Flat = struct

type var =
  Var.Atom.t

 and program = 
  | PEmpty of ( Position.t )
  | PConsComponent of pcons_abs

 and component = 
  | CValue of ids * optional_lformula * lterm
  | CRecValue of ids * optional_lformula * lterm
  | CTypeDef of tid * kind * type_definition
  | CFact of fact_status * lformula
  | CPredicate of pid * predicate_definition
  | CLogicFunction of pid * logic_function_definition

 and logic_function_definition = 
  | LFInductive of lfi_abs
  | LFAbbrev of lformula

 and logic_clause = 
  logic_clause_abs

 and fact_status = 
  | Lemma of ( UniqueIdentifier.t )
  | Axiom of ( UniqueIdentifier.t )

 and id = 
  var

 and ids = 
  id list

 and tid = 
  var

 and pid = 
  var

 and term = 
  | EId of var
  | EKApp of var * lterm list
  | EApp of lterm * lformula list * lterm list
  | ELam of fun_abs
  | ELet of let_abs
  | ELetRec of letrec_abs
  | ECase of lterm * clause list
  | EPrimitive of primitive
  | EAnnot of lterm * term_type
  | EForallTys of eforalltys_abs
  | EExistsTys of eexiststys_abs
  | EProd of lterm list
  | EIf of lterm * lterm * lterm
  | EAbsurd
  | EAssert of lformula * lterm
  | ELetLogic of let_logic_abs
  | EDeferred

 and primitive = 
  | PInt of ( int )
  | PTrue
  | PFalse

 and logic_bindings = 
  bindings * optional_lformula

 and logic_arguments = 
  fbindings

 and bindings = 
  binding list

 and binding = 
  id * term_type

 and function_output = 
  function_output_abs

 and clause = 
  clause_abs

 and pattern = 
  | PVar of id
  | PApp of constructor * pattern list

 and constructor = 
  var

 and term_type = 
  | TPrimitive of primitive_type
  | TVar of var
  | TArrow of term_type * term_type
  | TProd of term_type list
  | TApp of var * term_type list

 and primitive_type = 
  | TInt
  | TBool
  | TUnit

 and type_scheme = 
  | TScheme of scheme_abs

 and type_parameters = 
  type_parameter list

 and type_parameter = 
  var

 and kind = 
  | KStar
  | KArrow of kind list

 and type_definition = 
  | DAlgebraic of type_parameters * dataconstructor_definition list
  | DeferredType

 and dataconstructor_definition = 
  var * term_type

 and optional_lformula = 
  | ImplicitFormula of ( Position.t )
  | ExplicitFormula of lformula

 and formula = 
  | FTrue
  | FFalse
  | FVar of var
  | FLam of lfun_abs
  | FForallTys of fforalltys_abs
  | FExistsTys of fexiststys_abs
  | FForall of lforall_abs
  | FExists of lexists_abs
  | FEq of lformula * lformula
  | FApp of lformula * lformula list
  | FKApp of var * lformula list
  | FProd of lformula list
  | FPrimitive of formula_primitive
  | FAnnot of lformula * formula_type
  | FDeferred

 and formula_primitive = 
  | Pre
  | Post
  | PAnd
  | POr
  | PEquiv
  | PImply
  | PNot
  | PLessThan
  | PGreaterThan
  | PLessEqualThan
  | PGreaterEqualThan
  | PAdd
  | PSub
  | PMult
  | PDiv
  | PNeg
  | PEPrimitive of primitive

 and fbindings = 
  fbinding list

 and fbinding = 
  var * formula_type

 and trigger = 
  var

 and formula_type = 
  | FTProp
  | FTVar of var
  | FTArrow of formula_type * formula_type
  | FTCArrow of formula_type * formula_type
  | FTProd of formula_type list
  | FTPrimitive of primitive_type
  | FTApp of var * formula_type list

 and formula_type_scheme = 
  | FTScheme of formula_scheme_abs

 and predicate_definition = 
  | PDAbbrev of lformula
  | PDInductive of formula_type_scheme option * lformula list

 and lterm = 
  { tpos: ( Position.t ); tvalue: term }

 and lformula = 
  { fpos: ( Position.t ); fvalue: formula }

 and pcons_abs = 
  ( Position.t ) * component * program

 and lfi_abs = 
  ids * logic_clause list

 and logic_clause_abs = 
  pattern * lformula

 and fun_abs = 
  logic_arguments * logic_bindings * function_output * lterm

 and let_abs = 
  ids * optional_lformula * lterm * lterm

 and letrec_abs = 
  ids * optional_lformula * lterm * lterm

 and eforalltys_abs = 
  type_parameters * lterm

 and eexiststys_abs = 
  type_parameters * lterm

 and let_logic_abs = 
  fbindings * lformula * lterm

 and function_output_abs = 
  logic_bindings

 and clause_abs = 
  pattern * lterm

 and scheme_abs = 
  type_parameters * term_type

 and lfun_abs = 
  fbindings * lformula

 and fforalltys_abs = 
  type_parameters * lformula

 and fexiststys_abs = 
  type_parameters * lformula

 and lforall_abs = 
  fbindings * trigger list * lformula

 and lexists_abs = 
  fbindings * lformula

 and formula_scheme_abs = 
  type_parameters * formula_type

end

type var =
  Var.Atom.t

 and program = 
  | PEmpty of ( Position.t )
  | PConsComponent of opaque_pcons_abs

 and component = 
  | CValue of ids * optional_lformula * lterm
  | CRecValue of ids * optional_lformula * lterm
  | CTypeDef of tid * kind * type_definition
  | CFact of fact_status * lformula
  | CPredicate of pid * predicate_definition
  | CLogicFunction of pid * logic_function_definition

 and logic_function_definition = 
  | LFInductive of opaque_lfi_abs
  | LFAbbrev of lformula

 and logic_clause = 
  opaque_logic_clause_abs

 and fact_status = 
  | Lemma of ( UniqueIdentifier.t )
  | Axiom of ( UniqueIdentifier.t )

 and id = 
  var

 and ids = 
  id list

 and tid = 
  var

 and pid = 
  var

 and term = 
  | EId of var
  | EKApp of var * lterm list
  | EApp of lterm * lformula list * lterm list
  | ELam of opaque_fun_abs
  | ELet of opaque_let_abs
  | ELetRec of opaque_letrec_abs
  | ECase of lterm * clause list
  | EPrimitive of primitive
  | EAnnot of lterm * term_type
  | EForallTys of opaque_eforalltys_abs
  | EExistsTys of opaque_eexiststys_abs
  | EProd of lterm list
  | EIf of lterm * lterm * lterm
  | EAbsurd
  | EAssert of lformula * lterm
  | ELetLogic of opaque_let_logic_abs
  | EDeferred

 and primitive = 
  | PInt of ( int )
  | PTrue
  | PFalse

 and logic_bindings = 
  bindings * optional_lformula

 and logic_arguments = 
  fbindings

 and bindings = 
  binding list

 and binding = 
  id * term_type

 and function_output = 
  opaque_function_output_abs

 and clause = 
  opaque_clause_abs

 and pattern = 
  | PVar of id
  | PApp of constructor * pattern list

 and constructor = 
  var

 and term_type = 
  | TPrimitive of primitive_type
  | TVar of var
  | TArrow of term_type * term_type
  | TProd of term_type list
  | TApp of var * term_type list

 and primitive_type = 
  | TInt
  | TBool
  | TUnit

 and type_scheme = 
  | TScheme of opaque_scheme_abs

 and type_parameters = 
  type_parameter list

 and type_parameter = 
  var

 and kind = 
  | KStar
  | KArrow of kind list

 and type_definition = 
  | DAlgebraic of type_parameters * dataconstructor_definition list
  | DeferredType

 and dataconstructor_definition = 
  var * term_type

 and optional_lformula = 
  | ImplicitFormula of ( Position.t )
  | ExplicitFormula of lformula

 and formula = 
  | FTrue
  | FFalse
  | FVar of var
  | FLam of opaque_lfun_abs
  | FForallTys of opaque_fforalltys_abs
  | FExistsTys of opaque_fexiststys_abs
  | FForall of opaque_lforall_abs
  | FExists of opaque_lexists_abs
  | FEq of lformula * lformula
  | FApp of lformula * lformula list
  | FKApp of var * lformula list
  | FProd of lformula list
  | FPrimitive of formula_primitive
  | FAnnot of lformula * formula_type
  | FDeferred

 and formula_primitive = 
  | Pre
  | Post
  | PAnd
  | POr
  | PEquiv
  | PImply
  | PNot
  | PLessThan
  | PGreaterThan
  | PLessEqualThan
  | PGreaterEqualThan
  | PAdd
  | PSub
  | PMult
  | PDiv
  | PNeg
  | PEPrimitive of primitive

 and fbindings = 
  fbinding list

 and fbinding = 
  var * formula_type

 and trigger = 
  var

 and formula_type = 
  | FTProp
  | FTVar of var
  | FTArrow of formula_type * formula_type
  | FTCArrow of formula_type * formula_type
  | FTProd of formula_type list
  | FTPrimitive of primitive_type
  | FTApp of var * formula_type list

 and formula_type_scheme = 
  | FTScheme of opaque_formula_scheme_abs

 and predicate_definition = 
  | PDAbbrev of lformula
  | PDInductive of formula_type_scheme option * lformula list

 and lterm = 
  { tpos: ( Position.t ); tvalue: term }

 and lformula = 
  { fpos: ( Position.t ); fvalue: formula }

 and pcons_abs = 
  ( Position.t ) * component * program

 and opaque_pcons_abs = {
    mutable pcons_abs_delayed: Var.Subst.t;
    mutable pcons_abs: pcons_abs
  }

 and lfi_abs = 
  ids * logic_clause list

 and opaque_lfi_abs = {
    mutable lfi_abs_delayed: Var.Subst.t;
    mutable lfi_abs: lfi_abs
  }

 and logic_clause_abs = 
  pattern * lformula

 and opaque_logic_clause_abs = {
    mutable logic_clause_abs_delayed: Var.Subst.t;
    mutable logic_clause_abs: logic_clause_abs
  }

 and fun_abs = 
  logic_arguments * logic_bindings * function_output * lterm

 and opaque_fun_abs = {
    mutable fun_abs_delayed: Var.Subst.t;
    mutable fun_abs: fun_abs
  }

 and let_abs = 
  ids * optional_lformula * lterm * lterm

 and opaque_let_abs = {
    mutable let_abs_delayed: Var.Subst.t;
    mutable let_abs: let_abs
  }

 and letrec_abs = 
  ids * optional_lformula * lterm * lterm

 and opaque_letrec_abs = {
    mutable letrec_abs_delayed: Var.Subst.t;
    mutable letrec_abs: letrec_abs
  }

 and eforalltys_abs = 
  type_parameters * lterm

 and opaque_eforalltys_abs = {
    mutable eforalltys_abs_delayed: Var.Subst.t;
    mutable eforalltys_abs: eforalltys_abs
  }

 and eexiststys_abs = 
  type_parameters * lterm

 and opaque_eexiststys_abs = {
    mutable eexiststys_abs_delayed: Var.Subst.t;
    mutable eexiststys_abs: eexiststys_abs
  }

 and let_logic_abs = 
  fbindings * lformula * lterm

 and opaque_let_logic_abs = {
    mutable let_logic_abs_delayed: Var.Subst.t;
    mutable let_logic_abs: let_logic_abs
  }

 and function_output_abs = 
  logic_bindings

 and opaque_function_output_abs = {
    mutable function_output_abs_delayed: Var.Subst.t;
    mutable function_output_abs: function_output_abs
  }

 and clause_abs = 
  pattern * lterm

 and opaque_clause_abs = {
    mutable clause_abs_delayed: Var.Subst.t;
    mutable clause_abs: clause_abs
  }

 and scheme_abs = 
  type_parameters * term_type

 and opaque_scheme_abs = {
    mutable scheme_abs_delayed: Var.Subst.t;
    mutable scheme_abs: scheme_abs
  }

 and lfun_abs = 
  fbindings * lformula

 and opaque_lfun_abs = {
    mutable lfun_abs_delayed: Var.Subst.t;
    mutable lfun_abs: lfun_abs
  }

 and fforalltys_abs = 
  type_parameters * lformula

 and opaque_fforalltys_abs = {
    mutable fforalltys_abs_delayed: Var.Subst.t;
    mutable fforalltys_abs: fforalltys_abs
  }

 and fexiststys_abs = 
  type_parameters * lformula

 and opaque_fexiststys_abs = {
    mutable fexiststys_abs_delayed: Var.Subst.t;
    mutable fexiststys_abs: fexiststys_abs
  }

 and lforall_abs = 
  fbindings * trigger list * lformula

 and opaque_lforall_abs = {
    mutable lforall_abs_delayed: Var.Subst.t;
    mutable lforall_abs: lforall_abs
  }

 and lexists_abs = 
  fbindings * lformula

 and opaque_lexists_abs = {
    mutable lexists_abs_delayed: Var.Subst.t;
    mutable lexists_abs: lexists_abs
  }

 and formula_scheme_abs = 
  type_parameters * formula_type

 and opaque_formula_scheme_abs = {
    mutable formula_scheme_abs_delayed: Var.Subst.t;
    mutable formula_scheme_abs: formula_scheme_abs
  }

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
      raise (Invalid_argument "option_fold2")

let rec _dummy x = x

and import_program : var Identifier.Map.t -> Raw.program -> program = fun (var_env) -> function
  | Raw.PEmpty (_x0) ->
      PEmpty (_x0)
  | Raw.PConsComponent (_pcons_abs0) ->
      let (var_bvars) = bvi_pcons_abs _pcons_abs0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _pcons_abs0 = import_pcons_abs (var_env, var_ienv) _pcons_abs0 in
      PConsComponent (create_pcons_abs _pcons_abs0)

and subst_program : Var.Subst.t -> program -> program = fun (var_env) -> function
  | PEmpty (_x0) ->
      PEmpty (_x0)
  | PConsComponent (_pcons_abs0) ->
      PConsComponent (apply_pcons_abs (var_env) _pcons_abs0)

and export_program : Var.AtomIdMap.t -> program -> Raw.program = fun (var_m) -> function
  | PEmpty (_x0) ->
      Raw.PEmpty (_x0)
  | PConsComponent (_pcons_abs0) ->
      let pcons_abs = open_pcons_abs _pcons_abs0 in
      let (var_bvars) = bound_pcons_abs pcons_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.PConsComponent (export_pcons_abs (var_m, var_im) pcons_abs)

and flatten_program : program -> Flat.program = function
  | PEmpty (_x0) ->
      Flat.PEmpty (_x0)
  | PConsComponent (_pcons_abs0) ->
      let pcons_abs = open_pcons_abs _pcons_abs0 in
      Flat.PConsComponent (flatten_pcons_abs pcons_abs)

and unflatten_program : Flat.program -> program = function
  | Flat.PEmpty (_x0) ->
      PEmpty (_x0)
  | Flat.PConsComponent (_pcons_abs0) ->
      let pcons_abs = unflatten_pcons_abs _pcons_abs0 in
      PConsComponent (create_pcons_abs pcons_abs)

and free_program : program -> Var.AtomSet.t = 
  function program -> free_accu_program (Var.AtomSet.empty) program

and equal_program : program -> program -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_program x1 x2

and aeq_program : unit -> program -> program -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | PEmpty (_x0), PEmpty (_x1) ->
      ()
  | PConsComponent (_pcons_abs0), PConsComponent (_pcons_abs1) ->
      let _pcons_abs0, _pcons_abs1 = open2i_pcons_abs _pcons_abs0 _pcons_abs1 in
      aeq_pcons_abs () _pcons_abs0 _pcons_abs1;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_program")

and free_accu_program = fun (var_fvars) -> function
  | PEmpty (_x0) ->
      (var_fvars)
  | PConsComponent (_pcons_abs0) ->
      let pcons_abs = open_pcons_abs _pcons_abs0 in
      let (var_bvars, var_ifvars, var_fvars) = bound_free_accu_pcons_abs (Var.AtomSet.empty, Var.AtomSet.empty, var_fvars) pcons_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)

and subst_component : Var.Subst.t * Var.Subst.t -> component -> component = fun (var_oenv, var_ienv) -> function
  | CValue (_ids2, _optional_lformula1, _lterm0) ->
      CValue ((subst_ids (var_ienv)) _ids2, (subst_optional_lformula (var_ienv)) _optional_lformula1, (subst_lterm (var_oenv)) _lterm0)
  | CRecValue (_ids2, _optional_lformula1, _lterm0) ->
      CRecValue ((subst_ids (var_ienv)) _ids2, (subst_optional_lformula (var_ienv)) _optional_lformula1, (subst_lterm (var_ienv)) _lterm0)
  | CTypeDef (_tid2, _kind1, _type_definition0) ->
      CTypeDef ((subst_tid (var_ienv)) _tid2, (subst_kind ()) _kind1, (subst_type_definition (var_ienv)) _type_definition0)
  | CFact (_fact_status1, _lformula0) ->
      CFact ((subst_fact_status ()) _fact_status1, (subst_lformula (var_oenv)) _lformula0)
  | CPredicate (_pid1, _predicate_definition0) ->
      CPredicate ((subst_pid (var_ienv)) _pid1, (subst_predicate_definition (var_ienv)) _predicate_definition0)
  | CLogicFunction (_pid1, _logic_function_definition0) ->
      CLogicFunction ((subst_pid (var_ienv)) _pid1, (subst_logic_function_definition (var_ienv)) _logic_function_definition0)

and bound_component : component -> Var.AtomSet.t = 
  function component -> bound_accu_component (Var.AtomSet.empty) component

and bound_free_component : component -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t = 
  function component -> bound_free_accu_component (Var.AtomSet.empty, Var.AtomSet.empty, Var.AtomSet.empty) component

and equal_component : component -> component -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_component x1 x2

and import_component = fun (var_oenv, var_ienv) -> function
  | Raw.CValue (_ids2, _optional_lformula1, _lterm0) ->
      CValue ((import_ids (var_ienv)) _ids2, (import_optional_lformula (var_ienv)) _optional_lformula1, (import_lterm (var_oenv)) _lterm0)
  | Raw.CRecValue (_ids2, _optional_lformula1, _lterm0) ->
      CRecValue ((import_ids (var_ienv)) _ids2, (import_optional_lformula (var_ienv)) _optional_lformula1, (import_lterm (var_ienv)) _lterm0)
  | Raw.CTypeDef (_tid2, _kind1, _type_definition0) ->
      CTypeDef ((import_tid (var_ienv)) _tid2, (import_kind ()) _kind1, (import_type_definition (var_ienv)) _type_definition0)
  | Raw.CFact (_fact_status1, _lformula0) ->
      CFact ((import_fact_status ()) _fact_status1, (import_lformula (var_oenv)) _lformula0)
  | Raw.CPredicate (_pid1, _predicate_definition0) ->
      CPredicate ((import_pid (var_ienv)) _pid1, (import_predicate_definition (var_ienv)) _predicate_definition0)
  | Raw.CLogicFunction (_pid1, _logic_function_definition0) ->
      CLogicFunction ((import_pid (var_ienv)) _pid1, (import_logic_function_definition (var_ienv)) _logic_function_definition0)

and bvi_accu_component = fun (var_bvars) -> function
  | Raw.CValue (_ids2, _optional_lformula1, _lterm0) ->
      let (var_bvars) = bvi_accu_ids (var_bvars) _ids2 in
      (var_bvars)
  | Raw.CRecValue (_ids2, _optional_lformula1, _lterm0) ->
      let (var_bvars) = bvi_accu_ids (var_bvars) _ids2 in
      (var_bvars)
  | Raw.CTypeDef (_tid2, _kind1, _type_definition0) ->
      let (var_bvars) = bvi_accu_tid (var_bvars) _tid2 in
      let (var_bvars) = bvi_accu_type_definition (var_bvars) _type_definition0 in
      (var_bvars)
  | Raw.CFact (_fact_status1, _lformula0) ->
      (var_bvars)
  | Raw.CPredicate (_pid1, _predicate_definition0) ->
      let (var_bvars) = bvi_accu_pid (var_bvars) _pid1 in
      (var_bvars)
  | Raw.CLogicFunction (_pid1, _logic_function_definition0) ->
      let (var_bvars) = bvi_accu_pid (var_bvars) _pid1 in
      (var_bvars)

and bvi_component = 
  function component -> bvi_accu_component (Identifier.Map.empty) component

and bound_accu_component = fun (var_bvars) -> function
  | CValue (_ids2, _optional_lformula1, _lterm0) ->
      let (var_bvars) = bound_accu_ids (var_bvars) _ids2 in
      (var_bvars)
  | CRecValue (_ids2, _optional_lformula1, _lterm0) ->
      let (var_bvars) = bound_accu_ids (var_bvars) _ids2 in
      (var_bvars)
  | CTypeDef (_tid2, _kind1, _type_definition0) ->
      let (var_bvars) = bound_accu_tid (var_bvars) _tid2 in
      let (var_bvars) = bound_accu_type_definition (var_bvars) _type_definition0 in
      (var_bvars)
  | CFact (_fact_status1, _lformula0) ->
      (var_bvars)
  | CPredicate (_pid1, _predicate_definition0) ->
      let (var_bvars) = bound_accu_pid (var_bvars) _pid1 in
      (var_bvars)
  | CLogicFunction (_pid1, _logic_function_definition0) ->
      let (var_bvars) = bound_accu_pid (var_bvars) _pid1 in
      (var_bvars)

and export_component : Var.AtomIdMap.t * Var.AtomIdMap.t -> component -> Raw.component = fun (var_om, var_im) -> function
  | CValue (_ids2, _optional_lformula1, _lterm0) ->
      Raw.CValue ((export_ids (var_im)) _ids2, (export_optional_lformula (var_im)) _optional_lformula1, (export_lterm (var_om)) _lterm0)
  | CRecValue (_ids2, _optional_lformula1, _lterm0) ->
      Raw.CRecValue ((export_ids (var_im)) _ids2, (export_optional_lformula (var_im)) _optional_lformula1, (export_lterm (var_im)) _lterm0)
  | CTypeDef (_tid2, _kind1, _type_definition0) ->
      Raw.CTypeDef ((export_tid (var_im)) _tid2, (export_kind ()) _kind1, (export_type_definition (var_im)) _type_definition0)
  | CFact (_fact_status1, _lformula0) ->
      Raw.CFact ((export_fact_status ()) _fact_status1, (export_lformula (var_om)) _lformula0)
  | CPredicate (_pid1, _predicate_definition0) ->
      Raw.CPredicate ((export_pid (var_im)) _pid1, (export_predicate_definition (var_im)) _predicate_definition0)
  | CLogicFunction (_pid1, _logic_function_definition0) ->
      Raw.CLogicFunction ((export_pid (var_im)) _pid1, (export_logic_function_definition (var_im)) _logic_function_definition0)

and flatten_component : component -> Flat.component = function
  | CValue (_ids2, _optional_lformula1, _lterm0) ->
      Flat.CValue (flatten_ids _ids2, flatten_optional_lformula _optional_lformula1, flatten_lterm _lterm0)
  | CRecValue (_ids2, _optional_lformula1, _lterm0) ->
      Flat.CRecValue (flatten_ids _ids2, flatten_optional_lformula _optional_lformula1, flatten_lterm _lterm0)
  | CTypeDef (_tid2, _kind1, _type_definition0) ->
      Flat.CTypeDef (flatten_tid _tid2, flatten_kind _kind1, flatten_type_definition _type_definition0)
  | CFact (_fact_status1, _lformula0) ->
      Flat.CFact (flatten_fact_status _fact_status1, flatten_lformula _lformula0)
  | CPredicate (_pid1, _predicate_definition0) ->
      Flat.CPredicate (flatten_pid _pid1, flatten_predicate_definition _predicate_definition0)
  | CLogicFunction (_pid1, _logic_function_definition0) ->
      Flat.CLogicFunction (flatten_pid _pid1, flatten_logic_function_definition _logic_function_definition0)

and unflatten_component : Flat.component -> component = function
  | Flat.CValue (_ids2, _optional_lformula1, _lterm0) ->
      CValue (unflatten_ids _ids2, unflatten_optional_lformula _optional_lformula1, unflatten_lterm _lterm0)
  | Flat.CRecValue (_ids2, _optional_lformula1, _lterm0) ->
      CRecValue (unflatten_ids _ids2, unflatten_optional_lformula _optional_lformula1, unflatten_lterm _lterm0)
  | Flat.CTypeDef (_tid2, _kind1, _type_definition0) ->
      CTypeDef (unflatten_tid _tid2, unflatten_kind _kind1, unflatten_type_definition _type_definition0)
  | Flat.CFact (_fact_status1, _lformula0) ->
      CFact (unflatten_fact_status _fact_status1, unflatten_lformula _lformula0)
  | Flat.CPredicate (_pid1, _predicate_definition0) ->
      CPredicate (unflatten_pid _pid1, unflatten_predicate_definition _predicate_definition0)
  | Flat.CLogicFunction (_pid1, _logic_function_definition0) ->
      CLogicFunction (unflatten_pid _pid1, unflatten_logic_function_definition _logic_function_definition0)

and bound_free_accu_component = fun (var_bvars, var_ifvars, var_ofvars) -> function
  | CValue (_ids2, _optional_lformula1, _lterm0) ->
      let (var_bvars) = bound_free_accu_ids (var_bvars) _ids2 in
      let (var_ifvars) = free_accu_optional_lformula (var_ifvars) _optional_lformula1 in
      let (var_ofvars) = free_accu_lterm (var_ofvars) _lterm0 in
      (var_bvars, var_ifvars, var_ofvars)
  | CRecValue (_ids2, _optional_lformula1, _lterm0) ->
      let (var_bvars) = bound_free_accu_ids (var_bvars) _ids2 in
      let (var_ifvars) = free_accu_optional_lformula (var_ifvars) _optional_lformula1 in
      let (var_ifvars) = free_accu_lterm (var_ifvars) _lterm0 in
      (var_bvars, var_ifvars, var_ofvars)
  | CTypeDef (_tid2, _kind1, _type_definition0) ->
      let (var_bvars) = bound_free_accu_tid (var_bvars) _tid2 in
      let () = free_accu_kind () _kind1 in
      let (var_bvars, var_ifvars) = bound_free_accu_type_definition (var_bvars, var_ifvars) _type_definition0 in
      (var_bvars, var_ifvars, var_ofvars)
  | CFact (_fact_status1, _lformula0) ->
      let () = free_accu_fact_status () _fact_status1 in
      let (var_ofvars) = free_accu_lformula (var_ofvars) _lformula0 in
      (var_bvars, var_ifvars, var_ofvars)
  | CPredicate (_pid1, _predicate_definition0) ->
      let (var_bvars) = bound_free_accu_pid (var_bvars) _pid1 in
      let (var_ifvars) = free_accu_predicate_definition (var_ifvars) _predicate_definition0 in
      (var_bvars, var_ifvars, var_ofvars)
  | CLogicFunction (_pid1, _logic_function_definition0) ->
      let (var_bvars) = bound_free_accu_pid (var_bvars) _pid1 in
      let (var_ifvars) = free_accu_logic_function_definition (var_ifvars) _logic_function_definition0 in
      (var_bvars, var_ifvars, var_ofvars)

and aeq_component : unit -> component -> component -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | CValue (_ids2, _optional_lformula1, _lterm0), CValue (_ids5, _optional_lformula4, _lterm3) ->
      aeq_ids () _ids2 _ids5;
      aeq_optional_lformula () _optional_lformula1 _optional_lformula4;
      aeq_lterm () _lterm0 _lterm3;
      ()
  | CRecValue (_ids2, _optional_lformula1, _lterm0), CRecValue (_ids5, _optional_lformula4, _lterm3) ->
      aeq_ids () _ids2 _ids5;
      aeq_optional_lformula () _optional_lformula1 _optional_lformula4;
      aeq_lterm () _lterm0 _lterm3;
      ()
  | CTypeDef (_tid2, _kind1, _type_definition0), CTypeDef (_tid5, _kind4, _type_definition3) ->
      aeq_tid () _tid2 _tid5;
      aeq_kind () _kind1 _kind4;
      aeq_type_definition () _type_definition0 _type_definition3;
      ()
  | CFact (_fact_status1, _lformula0), CFact (_fact_status3, _lformula2) ->
      aeq_fact_status () _fact_status1 _fact_status3;
      aeq_lformula () _lformula0 _lformula2;
      ()
  | CPredicate (_pid1, _predicate_definition0), CPredicate (_pid3, _predicate_definition2) ->
      aeq_pid () _pid1 _pid3;
      aeq_predicate_definition () _predicate_definition0 _predicate_definition2;
      ()
  | CLogicFunction (_pid1, _logic_function_definition0), CLogicFunction (_pid3, _logic_function_definition2) ->
      aeq_pid () _pid1 _pid3;
      aeq_logic_function_definition () _logic_function_definition0 _logic_function_definition2;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_component")

and freshen2_component : Var.Subst.t * Var.Subst.t -> component -> component -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | CValue (_ids2, _optional_lformula1, _lterm0), CValue (_ids5, _optional_lformula4, _lterm3) ->
      let (var_env1, var_env2) = freshen2_ids (var_env1, var_env2) _ids2 _ids5 in
      (var_env1, var_env2)
  | CRecValue (_ids2, _optional_lformula1, _lterm0), CRecValue (_ids5, _optional_lformula4, _lterm3) ->
      let (var_env1, var_env2) = freshen2_ids (var_env1, var_env2) _ids2 _ids5 in
      (var_env1, var_env2)
  | CTypeDef (_tid2, _kind1, _type_definition0), CTypeDef (_tid5, _kind4, _type_definition3) ->
      let (var_env1, var_env2) = freshen2_tid (var_env1, var_env2) _tid2 _tid5 in
      let (var_env1, var_env2) = freshen2_type_definition (var_env1, var_env2) _type_definition0 _type_definition3 in
      (var_env1, var_env2)
  | CFact (_fact_status1, _lformula0), CFact (_fact_status3, _lformula2) ->
      (var_env1, var_env2)
  | CPredicate (_pid1, _predicate_definition0), CPredicate (_pid3, _predicate_definition2) ->
      let (var_env1, var_env2) = freshen2_pid (var_env1, var_env2) _pid1 _pid3 in
      (var_env1, var_env2)
  | CLogicFunction (_pid1, _logic_function_definition0), CLogicFunction (_pid3, _logic_function_definition2) ->
      let (var_env1, var_env2) = freshen2_pid (var_env1, var_env2) _pid1 _pid3 in
      (var_env1, var_env2)
  | _, _ ->
      raise (Invalid_argument "freshen2_component")

and import_logic_function_definition : var Identifier.Map.t -> Raw.logic_function_definition -> logic_function_definition = fun (var_env) -> function
  | Raw.LFInductive (_lfi_abs0) ->
      let (var_bvars) = bvi_lfi_abs _lfi_abs0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _lfi_abs0 = import_lfi_abs (var_ienv) _lfi_abs0 in
      LFInductive (create_lfi_abs _lfi_abs0)
  | Raw.LFAbbrev (_lformula0) ->
      LFAbbrev ((import_lformula (var_env)) _lformula0)

and subst_logic_function_definition : Var.Subst.t -> logic_function_definition -> logic_function_definition = fun (var_env) -> function
  | LFInductive (_lfi_abs0) ->
      LFInductive (apply_lfi_abs (var_env) _lfi_abs0)
  | LFAbbrev (_lformula0) ->
      LFAbbrev ((subst_lformula (var_env)) _lformula0)

and export_logic_function_definition : Var.AtomIdMap.t -> logic_function_definition -> Raw.logic_function_definition = fun (var_m) -> function
  | LFInductive (_lfi_abs0) ->
      let lfi_abs = open_lfi_abs _lfi_abs0 in
      let (var_bvars) = bound_lfi_abs lfi_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.LFInductive (export_lfi_abs (var_im) lfi_abs)
  | LFAbbrev (_lformula0) ->
      Raw.LFAbbrev ((export_lformula (var_m)) _lformula0)

and flatten_logic_function_definition : logic_function_definition -> Flat.logic_function_definition = function
  | LFInductive (_lfi_abs0) ->
      let lfi_abs = open_lfi_abs _lfi_abs0 in
      Flat.LFInductive (flatten_lfi_abs lfi_abs)
  | LFAbbrev (_lformula0) ->
      Flat.LFAbbrev (flatten_lformula _lformula0)

and unflatten_logic_function_definition : Flat.logic_function_definition -> logic_function_definition = function
  | Flat.LFInductive (_lfi_abs0) ->
      let lfi_abs = unflatten_lfi_abs _lfi_abs0 in
      LFInductive (create_lfi_abs lfi_abs)
  | Flat.LFAbbrev (_lformula0) ->
      LFAbbrev (unflatten_lformula _lformula0)

and free_logic_function_definition : logic_function_definition -> Var.AtomSet.t = 
  function logic_function_definition -> free_accu_logic_function_definition (Var.AtomSet.empty) logic_function_definition

and equal_logic_function_definition : logic_function_definition -> logic_function_definition -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_logic_function_definition x1 x2

and aeq_logic_function_definition : unit -> logic_function_definition -> logic_function_definition -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | LFInductive (_lfi_abs0), LFInductive (_lfi_abs1) ->
      let _lfi_abs0, _lfi_abs1 = open2i_lfi_abs _lfi_abs0 _lfi_abs1 in
      aeq_lfi_abs () _lfi_abs0 _lfi_abs1;
      ()
  | LFAbbrev (_lformula0), LFAbbrev (_lformula1) ->
      aeq_lformula () _lformula0 _lformula1;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_logic_function_definition")

and free_accu_logic_function_definition = fun (var_fvars) -> function
  | LFInductive (_lfi_abs0) ->
      let lfi_abs = open_lfi_abs _lfi_abs0 in
      let (var_bvars, var_ifvars) = bound_free_accu_lfi_abs (Var.AtomSet.empty, Var.AtomSet.empty) lfi_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)
  | LFAbbrev (_lformula0) ->
      let (var_fvars) = free_accu_lformula (var_fvars) _lformula0 in 
      (var_fvars)

and import_logic_clause : var Identifier.Map.t -> Raw.logic_clause -> logic_clause = fun (var_env) -> function
  (_logic_clause_abs0) ->
      let (var_bvars) = bvi_logic_clause_abs _logic_clause_abs0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _logic_clause_abs0 = import_logic_clause_abs (var_env, var_ienv) _logic_clause_abs0 in
    (create_logic_clause_abs _logic_clause_abs0)

and subst_logic_clause : Var.Subst.t -> logic_clause -> logic_clause = fun (var_env) -> function
  (_logic_clause_abs0) ->
    (apply_logic_clause_abs (var_env) _logic_clause_abs0)

and export_logic_clause : Var.AtomIdMap.t -> logic_clause -> Raw.logic_clause = fun (var_m) -> function
  (_logic_clause_abs0) ->
      let logic_clause_abs = open_logic_clause_abs _logic_clause_abs0 in
      let (var_bvars) = bound_logic_clause_abs logic_clause_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
    (export_logic_clause_abs (var_m, var_im) logic_clause_abs)

and flatten_logic_clause : logic_clause -> Flat.logic_clause = function
  (_logic_clause_abs0) ->
      let logic_clause_abs = open_logic_clause_abs _logic_clause_abs0 in
    (flatten_logic_clause_abs logic_clause_abs)

and unflatten_logic_clause : Flat.logic_clause -> logic_clause = function
  (_logic_clause_abs0) ->
      let logic_clause_abs = unflatten_logic_clause_abs _logic_clause_abs0 in
    (create_logic_clause_abs logic_clause_abs)

and free_logic_clause : logic_clause -> Var.AtomSet.t = 
  function logic_clause -> free_accu_logic_clause (Var.AtomSet.empty) logic_clause

and equal_logic_clause : logic_clause -> logic_clause -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_logic_clause x1 x2

and aeq_logic_clause : unit -> logic_clause -> logic_clause -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_logic_clause_abs0), (_logic_clause_abs1) ->
      let _logic_clause_abs0, _logic_clause_abs1 = open2i_logic_clause_abs _logic_clause_abs0 _logic_clause_abs1 in
      aeq_logic_clause_abs () _logic_clause_abs0 _logic_clause_abs1;
      ()

and free_accu_logic_clause = fun (var_fvars) -> function
  (_logic_clause_abs0) ->
      let logic_clause_abs = open_logic_clause_abs _logic_clause_abs0 in
      let (var_bvars, var_ifvars, var_fvars) = bound_free_accu_logic_clause_abs (Var.AtomSet.empty, Var.AtomSet.empty, var_fvars) logic_clause_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)

and import_fact_status : unit -> Raw.fact_status -> fact_status = fun () -> function
  | Raw.Lemma (_x0) ->
      Lemma (_x0)
  | Raw.Axiom (_x0) ->
      Axiom (_x0)

and subst_fact_status : unit -> fact_status -> fact_status = fun () -> function
  | Lemma (_x0) ->
      Lemma (_x0)
  | Axiom (_x0) ->
      Axiom (_x0)

and export_fact_status : unit -> fact_status -> Raw.fact_status = fun () -> function
  | Lemma (_x0) ->
      Raw.Lemma (_x0)
  | Axiom (_x0) ->
      Raw.Axiom (_x0)

and flatten_fact_status : fact_status -> Flat.fact_status = function
  | Lemma (_x0) ->
      Flat.Lemma (_x0)
  | Axiom (_x0) ->
      Flat.Axiom (_x0)

and unflatten_fact_status : Flat.fact_status -> fact_status = function
  | Flat.Lemma (_x0) ->
      Lemma (_x0)
  | Flat.Axiom (_x0) ->
      Axiom (_x0)

and free_fact_status : fact_status -> unit = 
  function fact_status -> free_accu_fact_status () fact_status

and equal_fact_status : fact_status -> fact_status -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_fact_status x1 x2

and aeq_fact_status : unit -> fact_status -> fact_status -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | Lemma (_x0), Lemma (_x1) ->
      ()
  | Axiom (_x0), Axiom (_x1) ->
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_fact_status")

and free_accu_fact_status = fun () -> function
  | Lemma (_x0) ->
      ()
  | Axiom (_x0) ->
      ()

and subst_id : Var.Subst.t -> id -> id = fun (var_ienv) -> function
  (_var0) ->
    (Var.Subst.lookup _var0 var_ienv)

and bound_id : id -> Var.AtomSet.t = 
  function id -> bound_accu_id (Var.AtomSet.empty) id

and bound_free_id : id -> Var.AtomSet.t = 
  function id -> bound_free_accu_id (Var.AtomSet.empty) id

and equal_id : id -> id -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_id x1 x2

and import_id = fun (var_ienv) -> function
  (_var0) ->
    (Var.find _var0 var_ienv)

and bvi_accu_id = fun (var_bvars) -> function
  (_var0) ->
      let var_bvars = Identifier.Map.add _var0 () var_bvars in
      (var_bvars)

and bvi_id = 
  function id -> bvi_accu_id (Identifier.Map.empty) id

and bound_accu_id = fun (var_bvars) -> function
  (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars)

and export_id : Var.AtomIdMap.t -> id -> Raw.id = fun (var_im) -> function
  (_var0) ->
    (Var.AtomIdMap.lookup _var0 var_im)

and flatten_id : id -> Flat.id = function
  (_var0) ->
    (_var0)

and unflatten_id : Flat.id -> id = function
  (_var0) ->
    (_var0)

and bound_free_accu_id = fun (var_bvars) -> function
  (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars)

and aeq_id : unit -> id -> id -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var0), (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_id");
      ()

and freshen2_id : Var.Subst.t * Var.Subst.t -> id -> id -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var0), (_var1) ->
      let var_env1, var_env2 = Var.Subst.freshen2 _var0 var_env1 _var1 var_env2 in
      (var_env1, var_env2)

and subst_ids : Var.Subst.t -> ids -> ids = fun (var_ienv) -> function
  (_ids0) ->
    (List.map (subst_id (var_ienv)) _ids0)

and bound_ids : ids -> Var.AtomSet.t = 
  function ids -> bound_accu_ids (Var.AtomSet.empty) ids

and bound_free_ids : ids -> Var.AtomSet.t = 
  function ids -> bound_free_accu_ids (Var.AtomSet.empty) ids

and equal_ids : ids -> ids -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_ids x1 x2

and import_ids = fun (var_ienv) -> function
  (_ids0) ->
    (List.map (import_id (var_ienv)) _ids0)

and bvi_accu_ids = fun (var_bvars) -> function
  (_ids0) ->
      let (var_bvars) = List.fold_left bvi_accu_id (var_bvars) _ids0 in
      (var_bvars)

and bvi_ids = 
  function ids -> bvi_accu_ids (Identifier.Map.empty) ids

and bound_accu_ids = fun (var_bvars) -> function
  (_ids0) ->
      let (var_bvars) = List.fold_left bound_accu_id (var_bvars) _ids0 in
      (var_bvars)

and export_ids : Var.AtomIdMap.t -> ids -> Raw.ids = fun (var_im) -> function
  (_ids0) ->
    (List.map (export_id (var_im)) _ids0)

and flatten_ids : ids -> Flat.ids = function
  (_ids0) ->
    (List.map flatten_id _ids0)

and unflatten_ids : Flat.ids -> ids = function
  (_ids0) ->
    (List.map unflatten_id _ids0)

and bound_free_accu_ids = fun (var_bvars) -> function
  (_ids0) ->
      let (var_bvars) = List.fold_left bound_free_accu_id (var_bvars) _ids0 in
      (var_bvars)

and aeq_ids : unit -> ids -> ids -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_ids0), (_ids1) ->
      List.fold_left2 aeq_id () _ids0 _ids1;
      ()

and freshen2_ids : Var.Subst.t * Var.Subst.t -> ids -> ids -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_ids0), (_ids1) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_id (var_env1, var_env2) _ids0 _ids1 in
      (var_env1, var_env2)

and subst_tid : Var.Subst.t -> tid -> tid = fun (var_ienv) -> function
  (_var0) ->
    (Var.Subst.lookup _var0 var_ienv)

and bound_tid : tid -> Var.AtomSet.t = 
  function tid -> bound_accu_tid (Var.AtomSet.empty) tid

and bound_free_tid : tid -> Var.AtomSet.t = 
  function tid -> bound_free_accu_tid (Var.AtomSet.empty) tid

and equal_tid : tid -> tid -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_tid x1 x2

and import_tid = fun (var_ienv) -> function
  (_var0) ->
    (Var.find _var0 var_ienv)

and bvi_accu_tid = fun (var_bvars) -> function
  (_var0) ->
      let var_bvars = Identifier.Map.add _var0 () var_bvars in
      (var_bvars)

and bvi_tid = 
  function tid -> bvi_accu_tid (Identifier.Map.empty) tid

and bound_accu_tid = fun (var_bvars) -> function
  (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars)

and export_tid : Var.AtomIdMap.t -> tid -> Raw.tid = fun (var_im) -> function
  (_var0) ->
    (Var.AtomIdMap.lookup _var0 var_im)

and flatten_tid : tid -> Flat.tid = function
  (_var0) ->
    (_var0)

and unflatten_tid : Flat.tid -> tid = function
  (_var0) ->
    (_var0)

and bound_free_accu_tid = fun (var_bvars) -> function
  (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars)

and aeq_tid : unit -> tid -> tid -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var0), (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_tid");
      ()

and freshen2_tid : Var.Subst.t * Var.Subst.t -> tid -> tid -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var0), (_var1) ->
      let var_env1, var_env2 = Var.Subst.freshen2 _var0 var_env1 _var1 var_env2 in
      (var_env1, var_env2)

and subst_pid : Var.Subst.t -> pid -> pid = fun (var_ienv) -> function
  (_var0) ->
    (Var.Subst.lookup _var0 var_ienv)

and bound_pid : pid -> Var.AtomSet.t = 
  function pid -> bound_accu_pid (Var.AtomSet.empty) pid

and bound_free_pid : pid -> Var.AtomSet.t = 
  function pid -> bound_free_accu_pid (Var.AtomSet.empty) pid

and equal_pid : pid -> pid -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_pid x1 x2

and import_pid = fun (var_ienv) -> function
  (_var0) ->
    (Var.find _var0 var_ienv)

and bvi_accu_pid = fun (var_bvars) -> function
  (_var0) ->
      let var_bvars = Identifier.Map.add _var0 () var_bvars in
      (var_bvars)

and bvi_pid = 
  function pid -> bvi_accu_pid (Identifier.Map.empty) pid

and bound_accu_pid = fun (var_bvars) -> function
  (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars)

and export_pid : Var.AtomIdMap.t -> pid -> Raw.pid = fun (var_im) -> function
  (_var0) ->
    (Var.AtomIdMap.lookup _var0 var_im)

and flatten_pid : pid -> Flat.pid = function
  (_var0) ->
    (_var0)

and unflatten_pid : Flat.pid -> pid = function
  (_var0) ->
    (_var0)

and bound_free_accu_pid = fun (var_bvars) -> function
  (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars)

and aeq_pid : unit -> pid -> pid -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var0), (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_pid");
      ()

and freshen2_pid : Var.Subst.t * Var.Subst.t -> pid -> pid -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var0), (_var1) ->
      let var_env1, var_env2 = Var.Subst.freshen2 _var0 var_env1 _var1 var_env2 in
      (var_env1, var_env2)

and import_term : var Identifier.Map.t -> Raw.term -> term = fun (var_env) -> function
  | Raw.EId (_var0) ->
      EId (Var.find _var0 var_env)
  | Raw.EKApp (_var1, _lterms0) ->
      EKApp (Var.find _var1 var_env, List.map (import_lterm (var_env)) _lterms0)
  | Raw.EApp (_lterm2, _lformulas1, _lterms0) ->
      EApp ((import_lterm (var_env)) _lterm2, List.map (import_lformula (var_env)) _lformulas1, List.map (import_lterm (var_env)) _lterms0)
  | Raw.ELam (_fun_abs0) ->
      let (var_bvars) = bvi_fun_abs _fun_abs0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _fun_abs0 = import_fun_abs (var_env, var_ienv) _fun_abs0 in
      ELam (create_fun_abs _fun_abs0)
  | Raw.ELet (_let_abs0) ->
      let (var_bvars) = bvi_let_abs _let_abs0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _let_abs0 = import_let_abs (var_env, var_ienv) _let_abs0 in
      ELet (create_let_abs _let_abs0)
  | Raw.ELetRec (_letrec_abs0) ->
      let (var_bvars) = bvi_letrec_abs _letrec_abs0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _letrec_abs0 = import_letrec_abs (var_ienv) _letrec_abs0 in
      ELetRec (create_letrec_abs _letrec_abs0)
  | Raw.ECase (_lterm1, _clauses0) ->
      ECase ((import_lterm (var_env)) _lterm1, List.map (import_clause (var_env)) _clauses0)
  | Raw.EPrimitive (_primitive0) ->
      EPrimitive ((import_primitive ()) _primitive0)
  | Raw.EAnnot (_lterm1, _term_type0) ->
      EAnnot ((import_lterm (var_env)) _lterm1, (import_term_type (var_env)) _term_type0)
  | Raw.EForallTys (_eforalltys_abs0) ->
      let (var_bvars) = bvi_eforalltys_abs _eforalltys_abs0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _eforalltys_abs0 = import_eforalltys_abs (var_ienv) _eforalltys_abs0 in
      EForallTys (create_eforalltys_abs _eforalltys_abs0)
  | Raw.EExistsTys (_eexiststys_abs0) ->
      let (var_bvars) = bvi_eexiststys_abs _eexiststys_abs0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _eexiststys_abs0 = import_eexiststys_abs (var_ienv) _eexiststys_abs0 in
      EExistsTys (create_eexiststys_abs _eexiststys_abs0)
  | Raw.EProd (_lterms0) ->
      EProd (List.map (import_lterm (var_env)) _lterms0)
  | Raw.EIf (_lterm2, _lterm1, _lterm0) ->
      EIf ((import_lterm (var_env)) _lterm2, (import_lterm (var_env)) _lterm1, (import_lterm (var_env)) _lterm0)
  | Raw.EAbsurd ->
      EAbsurd
  | Raw.EAssert (_lformula1, _lterm0) ->
      EAssert ((import_lformula (var_env)) _lformula1, (import_lterm (var_env)) _lterm0)
  | Raw.ELetLogic (_let_logic_abs0) ->
      let (var_bvars) = bvi_let_logic_abs _let_logic_abs0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _let_logic_abs0 = import_let_logic_abs (var_env, var_ienv) _let_logic_abs0 in
      ELetLogic (create_let_logic_abs _let_logic_abs0)
  | Raw.EDeferred ->
      EDeferred

and subst_term : Var.Subst.t -> term -> term = fun (var_env) -> function
  | EId (_var0) ->
      EId (Var.Subst.lookup _var0 var_env)
  | EKApp (_var1, _lterms0) ->
      EKApp (Var.Subst.lookup _var1 var_env, List.map (subst_lterm (var_env)) _lterms0)
  | EApp (_lterm2, _lformulas1, _lterms0) ->
      EApp ((subst_lterm (var_env)) _lterm2, List.map (subst_lformula (var_env)) _lformulas1, List.map (subst_lterm (var_env)) _lterms0)
  | ELam (_fun_abs0) ->
      ELam (apply_fun_abs (var_env) _fun_abs0)
  | ELet (_let_abs0) ->
      ELet (apply_let_abs (var_env) _let_abs0)
  | ELetRec (_letrec_abs0) ->
      ELetRec (apply_letrec_abs (var_env) _letrec_abs0)
  | ECase (_lterm1, _clauses0) ->
      ECase ((subst_lterm (var_env)) _lterm1, List.map (subst_clause (var_env)) _clauses0)
  | EPrimitive (_primitive0) ->
      EPrimitive ((subst_primitive ()) _primitive0)
  | EAnnot (_lterm1, _term_type0) ->
      EAnnot ((subst_lterm (var_env)) _lterm1, (subst_term_type (var_env)) _term_type0)
  | EForallTys (_eforalltys_abs0) ->
      EForallTys (apply_eforalltys_abs (var_env) _eforalltys_abs0)
  | EExistsTys (_eexiststys_abs0) ->
      EExistsTys (apply_eexiststys_abs (var_env) _eexiststys_abs0)
  | EProd (_lterms0) ->
      EProd (List.map (subst_lterm (var_env)) _lterms0)
  | EIf (_lterm2, _lterm1, _lterm0) ->
      EIf ((subst_lterm (var_env)) _lterm2, (subst_lterm (var_env)) _lterm1, (subst_lterm (var_env)) _lterm0)
  | EAbsurd ->
      EAbsurd
  | EAssert (_lformula1, _lterm0) ->
      EAssert ((subst_lformula (var_env)) _lformula1, (subst_lterm (var_env)) _lterm0)
  | ELetLogic (_let_logic_abs0) ->
      ELetLogic (apply_let_logic_abs (var_env) _let_logic_abs0)
  | EDeferred ->
      EDeferred

and export_term : Var.AtomIdMap.t -> term -> Raw.term = fun (var_m) -> function
  | EId (_var0) ->
      Raw.EId (Var.AtomIdMap.lookup _var0 var_m)
  | EKApp (_var1, _lterms0) ->
      Raw.EKApp (Var.AtomIdMap.lookup _var1 var_m, List.map (export_lterm (var_m)) _lterms0)
  | EApp (_lterm2, _lformulas1, _lterms0) ->
      Raw.EApp ((export_lterm (var_m)) _lterm2, List.map (export_lformula (var_m)) _lformulas1, List.map (export_lterm (var_m)) _lterms0)
  | ELam (_fun_abs0) ->
      let fun_abs = open_fun_abs _fun_abs0 in
      let (var_bvars) = bound_fun_abs fun_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.ELam (export_fun_abs (var_m, var_im) fun_abs)
  | ELet (_let_abs0) ->
      let let_abs = open_let_abs _let_abs0 in
      let (var_bvars) = bound_let_abs let_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.ELet (export_let_abs (var_m, var_im) let_abs)
  | ELetRec (_letrec_abs0) ->
      let letrec_abs = open_letrec_abs _letrec_abs0 in
      let (var_bvars) = bound_letrec_abs letrec_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.ELetRec (export_letrec_abs (var_im) letrec_abs)
  | ECase (_lterm1, _clauses0) ->
      Raw.ECase ((export_lterm (var_m)) _lterm1, List.map (export_clause (var_m)) _clauses0)
  | EPrimitive (_primitive0) ->
      Raw.EPrimitive ((export_primitive ()) _primitive0)
  | EAnnot (_lterm1, _term_type0) ->
      Raw.EAnnot ((export_lterm (var_m)) _lterm1, (export_term_type (var_m)) _term_type0)
  | EForallTys (_eforalltys_abs0) ->
      let eforalltys_abs = open_eforalltys_abs _eforalltys_abs0 in
      let (var_bvars) = bound_eforalltys_abs eforalltys_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.EForallTys (export_eforalltys_abs (var_im) eforalltys_abs)
  | EExistsTys (_eexiststys_abs0) ->
      let eexiststys_abs = open_eexiststys_abs _eexiststys_abs0 in
      let (var_bvars) = bound_eexiststys_abs eexiststys_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.EExistsTys (export_eexiststys_abs (var_im) eexiststys_abs)
  | EProd (_lterms0) ->
      Raw.EProd (List.map (export_lterm (var_m)) _lterms0)
  | EIf (_lterm2, _lterm1, _lterm0) ->
      Raw.EIf ((export_lterm (var_m)) _lterm2, (export_lterm (var_m)) _lterm1, (export_lterm (var_m)) _lterm0)
  | EAbsurd ->
      Raw.EAbsurd
  | EAssert (_lformula1, _lterm0) ->
      Raw.EAssert ((export_lformula (var_m)) _lformula1, (export_lterm (var_m)) _lterm0)
  | ELetLogic (_let_logic_abs0) ->
      let let_logic_abs = open_let_logic_abs _let_logic_abs0 in
      let (var_bvars) = bound_let_logic_abs let_logic_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.ELetLogic (export_let_logic_abs (var_m, var_im) let_logic_abs)
  | EDeferred ->
      Raw.EDeferred

and flatten_term : term -> Flat.term = function
  | EId (_var0) ->
      Flat.EId (_var0)
  | EKApp (_var1, _lterms0) ->
      Flat.EKApp (_var1, List.map flatten_lterm _lterms0)
  | EApp (_lterm2, _lformulas1, _lterms0) ->
      Flat.EApp (flatten_lterm _lterm2, List.map flatten_lformula _lformulas1, List.map flatten_lterm _lterms0)
  | ELam (_fun_abs0) ->
      let fun_abs = open_fun_abs _fun_abs0 in
      Flat.ELam (flatten_fun_abs fun_abs)
  | ELet (_let_abs0) ->
      let let_abs = open_let_abs _let_abs0 in
      Flat.ELet (flatten_let_abs let_abs)
  | ELetRec (_letrec_abs0) ->
      let letrec_abs = open_letrec_abs _letrec_abs0 in
      Flat.ELetRec (flatten_letrec_abs letrec_abs)
  | ECase (_lterm1, _clauses0) ->
      Flat.ECase (flatten_lterm _lterm1, List.map flatten_clause _clauses0)
  | EPrimitive (_primitive0) ->
      Flat.EPrimitive (flatten_primitive _primitive0)
  | EAnnot (_lterm1, _term_type0) ->
      Flat.EAnnot (flatten_lterm _lterm1, flatten_term_type _term_type0)
  | EForallTys (_eforalltys_abs0) ->
      let eforalltys_abs = open_eforalltys_abs _eforalltys_abs0 in
      Flat.EForallTys (flatten_eforalltys_abs eforalltys_abs)
  | EExistsTys (_eexiststys_abs0) ->
      let eexiststys_abs = open_eexiststys_abs _eexiststys_abs0 in
      Flat.EExistsTys (flatten_eexiststys_abs eexiststys_abs)
  | EProd (_lterms0) ->
      Flat.EProd (List.map flatten_lterm _lterms0)
  | EIf (_lterm2, _lterm1, _lterm0) ->
      Flat.EIf (flatten_lterm _lterm2, flatten_lterm _lterm1, flatten_lterm _lterm0)
  | EAbsurd ->
      Flat.EAbsurd
  | EAssert (_lformula1, _lterm0) ->
      Flat.EAssert (flatten_lformula _lformula1, flatten_lterm _lterm0)
  | ELetLogic (_let_logic_abs0) ->
      let let_logic_abs = open_let_logic_abs _let_logic_abs0 in
      Flat.ELetLogic (flatten_let_logic_abs let_logic_abs)
  | EDeferred ->
      Flat.EDeferred

and unflatten_term : Flat.term -> term = function
  | Flat.EId (_var0) ->
      EId (_var0)
  | Flat.EKApp (_var1, _lterms0) ->
      EKApp (_var1, List.map unflatten_lterm _lterms0)
  | Flat.EApp (_lterm2, _lformulas1, _lterms0) ->
      EApp (unflatten_lterm _lterm2, List.map unflatten_lformula _lformulas1, List.map unflatten_lterm _lterms0)
  | Flat.ELam (_fun_abs0) ->
      let fun_abs = unflatten_fun_abs _fun_abs0 in
      ELam (create_fun_abs fun_abs)
  | Flat.ELet (_let_abs0) ->
      let let_abs = unflatten_let_abs _let_abs0 in
      ELet (create_let_abs let_abs)
  | Flat.ELetRec (_letrec_abs0) ->
      let letrec_abs = unflatten_letrec_abs _letrec_abs0 in
      ELetRec (create_letrec_abs letrec_abs)
  | Flat.ECase (_lterm1, _clauses0) ->
      ECase (unflatten_lterm _lterm1, List.map unflatten_clause _clauses0)
  | Flat.EPrimitive (_primitive0) ->
      EPrimitive (unflatten_primitive _primitive0)
  | Flat.EAnnot (_lterm1, _term_type0) ->
      EAnnot (unflatten_lterm _lterm1, unflatten_term_type _term_type0)
  | Flat.EForallTys (_eforalltys_abs0) ->
      let eforalltys_abs = unflatten_eforalltys_abs _eforalltys_abs0 in
      EForallTys (create_eforalltys_abs eforalltys_abs)
  | Flat.EExistsTys (_eexiststys_abs0) ->
      let eexiststys_abs = unflatten_eexiststys_abs _eexiststys_abs0 in
      EExistsTys (create_eexiststys_abs eexiststys_abs)
  | Flat.EProd (_lterms0) ->
      EProd (List.map unflatten_lterm _lterms0)
  | Flat.EIf (_lterm2, _lterm1, _lterm0) ->
      EIf (unflatten_lterm _lterm2, unflatten_lterm _lterm1, unflatten_lterm _lterm0)
  | Flat.EAbsurd ->
      EAbsurd
  | Flat.EAssert (_lformula1, _lterm0) ->
      EAssert (unflatten_lformula _lformula1, unflatten_lterm _lterm0)
  | Flat.ELetLogic (_let_logic_abs0) ->
      let let_logic_abs = unflatten_let_logic_abs _let_logic_abs0 in
      ELetLogic (create_let_logic_abs let_logic_abs)
  | Flat.EDeferred ->
      EDeferred

and free_term : term -> Var.AtomSet.t = 
  function term -> free_accu_term (Var.AtomSet.empty) term

and equal_term : term -> term -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_term x1 x2

and aeq_term : unit -> term -> term -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | EId (_var0), EId (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_term");
      ()
  | EKApp (_var1, _lterms0), EKApp (_var3, _lterms2) ->
      if not (Var.Atom.equal _var1 _var3) then raise (Invalid_argument "aeq_term");
      List.fold_left2 aeq_lterm () _lterms0 _lterms2;
      ()
  | EApp (_lterm2, _lformulas1, _lterms0), EApp (_lterm5, _lformulas4, _lterms3) ->
      aeq_lterm () _lterm2 _lterm5;
      List.fold_left2 aeq_lformula () _lformulas1 _lformulas4;
      List.fold_left2 aeq_lterm () _lterms0 _lterms3;
      ()
  | ELam (_fun_abs0), ELam (_fun_abs1) ->
      let _fun_abs0, _fun_abs1 = open2i_fun_abs _fun_abs0 _fun_abs1 in
      aeq_fun_abs () _fun_abs0 _fun_abs1;
      ()
  | ELet (_let_abs0), ELet (_let_abs1) ->
      let _let_abs0, _let_abs1 = open2i_let_abs _let_abs0 _let_abs1 in
      aeq_let_abs () _let_abs0 _let_abs1;
      ()
  | ELetRec (_letrec_abs0), ELetRec (_letrec_abs1) ->
      let _letrec_abs0, _letrec_abs1 = open2i_letrec_abs _letrec_abs0 _letrec_abs1 in
      aeq_letrec_abs () _letrec_abs0 _letrec_abs1;
      ()
  | ECase (_lterm1, _clauses0), ECase (_lterm3, _clauses2) ->
      aeq_lterm () _lterm1 _lterm3;
      List.fold_left2 aeq_clause () _clauses0 _clauses2;
      ()
  | EPrimitive (_primitive0), EPrimitive (_primitive1) ->
      aeq_primitive () _primitive0 _primitive1;
      ()
  | EAnnot (_lterm1, _term_type0), EAnnot (_lterm3, _term_type2) ->
      aeq_lterm () _lterm1 _lterm3;
      aeq_term_type () _term_type0 _term_type2;
      ()
  | EForallTys (_eforalltys_abs0), EForallTys (_eforalltys_abs1) ->
      let _eforalltys_abs0, _eforalltys_abs1 = open2i_eforalltys_abs _eforalltys_abs0 _eforalltys_abs1 in
      aeq_eforalltys_abs () _eforalltys_abs0 _eforalltys_abs1;
      ()
  | EExistsTys (_eexiststys_abs0), EExistsTys (_eexiststys_abs1) ->
      let _eexiststys_abs0, _eexiststys_abs1 = open2i_eexiststys_abs _eexiststys_abs0 _eexiststys_abs1 in
      aeq_eexiststys_abs () _eexiststys_abs0 _eexiststys_abs1;
      ()
  | EProd (_lterms0), EProd (_lterms1) ->
      List.fold_left2 aeq_lterm () _lterms0 _lterms1;
      ()
  | EIf (_lterm2, _lterm1, _lterm0), EIf (_lterm5, _lterm4, _lterm3) ->
      aeq_lterm () _lterm2 _lterm5;
      aeq_lterm () _lterm1 _lterm4;
      aeq_lterm () _lterm0 _lterm3;
      ()
  | EAbsurd, EAbsurd ->
      ()
  | EAssert (_lformula1, _lterm0), EAssert (_lformula3, _lterm2) ->
      aeq_lformula () _lformula1 _lformula3;
      aeq_lterm () _lterm0 _lterm2;
      ()
  | ELetLogic (_let_logic_abs0), ELetLogic (_let_logic_abs1) ->
      let _let_logic_abs0, _let_logic_abs1 = open2i_let_logic_abs _let_logic_abs0 _let_logic_abs1 in
      aeq_let_logic_abs () _let_logic_abs0 _let_logic_abs1;
      ()
  | EDeferred, EDeferred ->
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_term")

and free_accu_term = fun (var_fvars) -> function
  | EId (_var0) ->
      let var_fvars = Var.AtomSet.add _var0 var_fvars in
      (var_fvars)
  | EKApp (_var1, _lterms0) ->
      let var_fvars = Var.AtomSet.add _var1 var_fvars in
      let (var_fvars) = List.fold_left free_accu_lterm (var_fvars) _lterms0 in 
      (var_fvars)
  | EApp (_lterm2, _lformulas1, _lterms0) ->
      let (var_fvars) = free_accu_lterm (var_fvars) _lterm2 in 
      let (var_fvars) = List.fold_left free_accu_lformula (var_fvars) _lformulas1 in 
      let (var_fvars) = List.fold_left free_accu_lterm (var_fvars) _lterms0 in 
      (var_fvars)
  | ELam (_fun_abs0) ->
      let fun_abs = open_fun_abs _fun_abs0 in
      let (var_bvars, var_ifvars, var_fvars) = bound_free_accu_fun_abs (Var.AtomSet.empty, Var.AtomSet.empty, var_fvars) fun_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)
  | ELet (_let_abs0) ->
      let let_abs = open_let_abs _let_abs0 in
      let (var_bvars, var_ifvars, var_fvars) = bound_free_accu_let_abs (Var.AtomSet.empty, Var.AtomSet.empty, var_fvars) let_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)
  | ELetRec (_letrec_abs0) ->
      let letrec_abs = open_letrec_abs _letrec_abs0 in
      let (var_bvars, var_ifvars) = bound_free_accu_letrec_abs (Var.AtomSet.empty, Var.AtomSet.empty) letrec_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)
  | ECase (_lterm1, _clauses0) ->
      let (var_fvars) = free_accu_lterm (var_fvars) _lterm1 in 
      let (var_fvars) = List.fold_left free_accu_clause (var_fvars) _clauses0 in 
      (var_fvars)
  | EPrimitive (_primitive0) ->
      let () = free_accu_primitive () _primitive0 in 
      (var_fvars)
  | EAnnot (_lterm1, _term_type0) ->
      let (var_fvars) = free_accu_lterm (var_fvars) _lterm1 in 
      let (var_fvars) = free_accu_term_type (var_fvars) _term_type0 in 
      (var_fvars)
  | EForallTys (_eforalltys_abs0) ->
      let eforalltys_abs = open_eforalltys_abs _eforalltys_abs0 in
      let (var_bvars, var_ifvars) = bound_free_accu_eforalltys_abs (Var.AtomSet.empty, Var.AtomSet.empty) eforalltys_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)
  | EExistsTys (_eexiststys_abs0) ->
      let eexiststys_abs = open_eexiststys_abs _eexiststys_abs0 in
      let (var_bvars, var_ifvars) = bound_free_accu_eexiststys_abs (Var.AtomSet.empty, Var.AtomSet.empty) eexiststys_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)
  | EProd (_lterms0) ->
      let (var_fvars) = List.fold_left free_accu_lterm (var_fvars) _lterms0 in 
      (var_fvars)
  | EIf (_lterm2, _lterm1, _lterm0) ->
      let (var_fvars) = free_accu_lterm (var_fvars) _lterm2 in 
      let (var_fvars) = free_accu_lterm (var_fvars) _lterm1 in 
      let (var_fvars) = free_accu_lterm (var_fvars) _lterm0 in 
      (var_fvars)
  | EAbsurd ->
      (var_fvars)
  | EAssert (_lformula1, _lterm0) ->
      let (var_fvars) = free_accu_lformula (var_fvars) _lformula1 in 
      let (var_fvars) = free_accu_lterm (var_fvars) _lterm0 in 
      (var_fvars)
  | ELetLogic (_let_logic_abs0) ->
      let let_logic_abs = open_let_logic_abs _let_logic_abs0 in
      let (var_bvars, var_ifvars, var_fvars) = bound_free_accu_let_logic_abs (Var.AtomSet.empty, Var.AtomSet.empty, var_fvars) let_logic_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)
  | EDeferred ->
      (var_fvars)

and import_primitive : unit -> Raw.primitive -> primitive = fun () -> function
  | Raw.PInt (_x0) ->
      PInt (_x0)
  | Raw.PTrue ->
      PTrue
  | Raw.PFalse ->
      PFalse

and subst_primitive : unit -> primitive -> primitive = fun () -> function
  | PInt (_x0) ->
      PInt (_x0)
  | PTrue ->
      PTrue
  | PFalse ->
      PFalse

and export_primitive : unit -> primitive -> Raw.primitive = fun () -> function
  | PInt (_x0) ->
      Raw.PInt (_x0)
  | PTrue ->
      Raw.PTrue
  | PFalse ->
      Raw.PFalse

and flatten_primitive : primitive -> Flat.primitive = function
  | PInt (_x0) ->
      Flat.PInt (_x0)
  | PTrue ->
      Flat.PTrue
  | PFalse ->
      Flat.PFalse

and unflatten_primitive : Flat.primitive -> primitive = function
  | Flat.PInt (_x0) ->
      PInt (_x0)
  | Flat.PTrue ->
      PTrue
  | Flat.PFalse ->
      PFalse

and free_primitive : primitive -> unit = 
  function primitive -> free_accu_primitive () primitive

and equal_primitive : primitive -> primitive -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_primitive x1 x2

and aeq_primitive : unit -> primitive -> primitive -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | PInt (_x0), PInt (_x1) ->
      ()
  | PTrue, PTrue ->
      ()
  | PFalse, PFalse ->
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_primitive")

and free_accu_primitive = fun () -> function
  | PInt (_x0) ->
      ()
  | PTrue ->
      ()
  | PFalse ->
      ()

and subst_logic_bindings : Var.Subst.t -> logic_bindings -> logic_bindings = fun (var_ienv) -> function
  (_bindings1, _optional_lformula0) ->
    ((subst_bindings (var_ienv)) _bindings1, (subst_optional_lformula (var_ienv)) _optional_lformula0)

and bound_logic_bindings : logic_bindings -> Var.AtomSet.t = 
  function logic_bindings -> bound_accu_logic_bindings (Var.AtomSet.empty) logic_bindings

and bound_free_logic_bindings : logic_bindings -> Var.AtomSet.t * Var.AtomSet.t = 
  function logic_bindings -> bound_free_accu_logic_bindings (Var.AtomSet.empty, Var.AtomSet.empty) logic_bindings

and equal_logic_bindings : logic_bindings -> logic_bindings -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_logic_bindings x1 x2

and import_logic_bindings = fun (var_ienv) -> function
  (_bindings1, _optional_lformula0) ->
    ((import_bindings (var_ienv)) _bindings1, (import_optional_lformula (var_ienv)) _optional_lformula0)

and bvi_accu_logic_bindings = fun (var_bvars) -> function
  (_bindings1, _optional_lformula0) ->
      let (var_bvars) = bvi_accu_bindings (var_bvars) _bindings1 in
      (var_bvars)

and bvi_logic_bindings = 
  function logic_bindings -> bvi_accu_logic_bindings (Identifier.Map.empty) logic_bindings

and bound_accu_logic_bindings = fun (var_bvars) -> function
  (_bindings1, _optional_lformula0) ->
      let (var_bvars) = bound_accu_bindings (var_bvars) _bindings1 in
      (var_bvars)

and export_logic_bindings : Var.AtomIdMap.t -> logic_bindings -> Raw.logic_bindings = fun (var_im) -> function
  (_bindings1, _optional_lformula0) ->
    ((export_bindings (var_im)) _bindings1, (export_optional_lformula (var_im)) _optional_lformula0)

and flatten_logic_bindings : logic_bindings -> Flat.logic_bindings = function
  (_bindings1, _optional_lformula0) ->
    (flatten_bindings _bindings1, flatten_optional_lformula _optional_lformula0)

and unflatten_logic_bindings : Flat.logic_bindings -> logic_bindings = function
  (_bindings1, _optional_lformula0) ->
    (unflatten_bindings _bindings1, unflatten_optional_lformula _optional_lformula0)

and bound_free_accu_logic_bindings = fun (var_bvars, var_ifvars) -> function
  (_bindings1, _optional_lformula0) ->
      let (var_bvars, var_ifvars) = bound_free_accu_bindings (var_bvars, var_ifvars) _bindings1 in
      let (var_ifvars) = free_accu_optional_lformula (var_ifvars) _optional_lformula0 in
      (var_bvars, var_ifvars)

and aeq_logic_bindings : unit -> logic_bindings -> logic_bindings -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_bindings1, _optional_lformula0), (_bindings3, _optional_lformula2) ->
      aeq_bindings () _bindings1 _bindings3;
      aeq_optional_lformula () _optional_lformula0 _optional_lformula2;
      ()

and freshen2_logic_bindings : Var.Subst.t * Var.Subst.t -> logic_bindings -> logic_bindings -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_bindings1, _optional_lformula0), (_bindings3, _optional_lformula2) ->
      let (var_env1, var_env2) = freshen2_bindings (var_env1, var_env2) _bindings1 _bindings3 in
      (var_env1, var_env2)

and subst_logic_arguments : Var.Subst.t * Var.Subst.t -> logic_arguments -> logic_arguments = fun (var_oenv, var_ienv) -> function
  (_fbindings0) ->
    ((subst_fbindings (var_oenv, var_ienv)) _fbindings0)

and bound_logic_arguments : logic_arguments -> Var.AtomSet.t = 
  function logic_arguments -> bound_accu_logic_arguments (Var.AtomSet.empty) logic_arguments

and bound_free_logic_arguments : logic_arguments -> Var.AtomSet.t * Var.AtomSet.t = 
  function logic_arguments -> bound_free_accu_logic_arguments (Var.AtomSet.empty, Var.AtomSet.empty) logic_arguments

and equal_logic_arguments : logic_arguments -> logic_arguments -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_logic_arguments x1 x2

and import_logic_arguments = fun (var_oenv, var_ienv) -> function
  (_fbindings0) ->
    ((import_fbindings (var_oenv, var_ienv)) _fbindings0)

and bvi_accu_logic_arguments = fun (var_bvars) -> function
  (_fbindings0) ->
      let (var_bvars) = bvi_accu_fbindings (var_bvars) _fbindings0 in
      (var_bvars)

and bvi_logic_arguments = 
  function logic_arguments -> bvi_accu_logic_arguments (Identifier.Map.empty) logic_arguments

and bound_accu_logic_arguments = fun (var_bvars) -> function
  (_fbindings0) ->
      let (var_bvars) = bound_accu_fbindings (var_bvars) _fbindings0 in
      (var_bvars)

and export_logic_arguments : Var.AtomIdMap.t * Var.AtomIdMap.t -> logic_arguments -> Raw.logic_arguments = fun (var_om, var_im) -> function
  (_fbindings0) ->
    ((export_fbindings (var_om, var_im)) _fbindings0)

and flatten_logic_arguments : logic_arguments -> Flat.logic_arguments = function
  (_fbindings0) ->
    (flatten_fbindings _fbindings0)

and unflatten_logic_arguments : Flat.logic_arguments -> logic_arguments = function
  (_fbindings0) ->
    (unflatten_fbindings _fbindings0)

and bound_free_accu_logic_arguments = fun (var_bvars, var_ofvars) -> function
  (_fbindings0) ->
      let (var_bvars, var_ofvars) = bound_free_accu_fbindings (var_bvars, var_ofvars) _fbindings0 in
      (var_bvars, var_ofvars)

and aeq_logic_arguments : unit -> logic_arguments -> logic_arguments -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_fbindings0), (_fbindings1) ->
      aeq_fbindings () _fbindings0 _fbindings1;
      ()

and freshen2_logic_arguments : Var.Subst.t * Var.Subst.t -> logic_arguments -> logic_arguments -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_fbindings0), (_fbindings1) ->
      let (var_env1, var_env2) = freshen2_fbindings (var_env1, var_env2) _fbindings0 _fbindings1 in
      (var_env1, var_env2)

and subst_bindings : Var.Subst.t -> bindings -> bindings = fun (var_ienv) -> function
  (_bindings0) ->
    (List.map (subst_binding (var_ienv)) _bindings0)

and bound_bindings : bindings -> Var.AtomSet.t = 
  function bindings -> bound_accu_bindings (Var.AtomSet.empty) bindings

and bound_free_bindings : bindings -> Var.AtomSet.t * Var.AtomSet.t = 
  function bindings -> bound_free_accu_bindings (Var.AtomSet.empty, Var.AtomSet.empty) bindings

and equal_bindings : bindings -> bindings -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_bindings x1 x2

and import_bindings = fun (var_ienv) -> function
  (_bindings0) ->
    (List.map (import_binding (var_ienv)) _bindings0)

and bvi_accu_bindings = fun (var_bvars) -> function
  (_bindings0) ->
      let (var_bvars) = List.fold_left bvi_accu_binding (var_bvars) _bindings0 in
      (var_bvars)

and bvi_bindings = 
  function bindings -> bvi_accu_bindings (Identifier.Map.empty) bindings

and bound_accu_bindings = fun (var_bvars) -> function
  (_bindings0) ->
      let (var_bvars) = List.fold_left bound_accu_binding (var_bvars) _bindings0 in
      (var_bvars)

and export_bindings : Var.AtomIdMap.t -> bindings -> Raw.bindings = fun (var_im) -> function
  (_bindings0) ->
    (List.map (export_binding (var_im)) _bindings0)

and flatten_bindings : bindings -> Flat.bindings = function
  (_bindings0) ->
    (List.map flatten_binding _bindings0)

and unflatten_bindings : Flat.bindings -> bindings = function
  (_bindings0) ->
    (List.map unflatten_binding _bindings0)

and bound_free_accu_bindings = fun (var_bvars, var_ifvars) -> function
  (_bindings0) ->
      let (var_bvars, var_ifvars) = List.fold_left bound_free_accu_binding (var_bvars, var_ifvars) _bindings0 in
      (var_bvars, var_ifvars)

and aeq_bindings : unit -> bindings -> bindings -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_bindings0), (_bindings1) ->
      List.fold_left2 aeq_binding () _bindings0 _bindings1;
      ()

and freshen2_bindings : Var.Subst.t * Var.Subst.t -> bindings -> bindings -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_bindings0), (_bindings1) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_binding (var_env1, var_env2) _bindings0 _bindings1 in
      (var_env1, var_env2)

and subst_binding : Var.Subst.t -> binding -> binding = fun (var_ienv) -> function
  (_id1, _term_type0) ->
    ((subst_id (var_ienv)) _id1, (subst_term_type (var_ienv)) _term_type0)

and bound_binding : binding -> Var.AtomSet.t = 
  function binding -> bound_accu_binding (Var.AtomSet.empty) binding

and bound_free_binding : binding -> Var.AtomSet.t * Var.AtomSet.t = 
  function binding -> bound_free_accu_binding (Var.AtomSet.empty, Var.AtomSet.empty) binding

and equal_binding : binding -> binding -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_binding x1 x2

and import_binding = fun (var_ienv) -> function
  (_id1, _term_type0) ->
    ((import_id (var_ienv)) _id1, (import_term_type (var_ienv)) _term_type0)

and bvi_accu_binding = fun (var_bvars) -> function
  (_id1, _term_type0) ->
      let (var_bvars) = bvi_accu_id (var_bvars) _id1 in
      (var_bvars)

and bvi_binding = 
  function binding -> bvi_accu_binding (Identifier.Map.empty) binding

and bound_accu_binding = fun (var_bvars) -> function
  (_id1, _term_type0) ->
      let (var_bvars) = bound_accu_id (var_bvars) _id1 in
      (var_bvars)

and export_binding : Var.AtomIdMap.t -> binding -> Raw.binding = fun (var_im) -> function
  (_id1, _term_type0) ->
    ((export_id (var_im)) _id1, (export_term_type (var_im)) _term_type0)

and flatten_binding : binding -> Flat.binding = function
  (_id1, _term_type0) ->
    (flatten_id _id1, flatten_term_type _term_type0)

and unflatten_binding : Flat.binding -> binding = function
  (_id1, _term_type0) ->
    (unflatten_id _id1, unflatten_term_type _term_type0)

and bound_free_accu_binding = fun (var_bvars, var_ifvars) -> function
  (_id1, _term_type0) ->
      let (var_bvars) = bound_free_accu_id (var_bvars) _id1 in
      let (var_ifvars) = free_accu_term_type (var_ifvars) _term_type0 in
      (var_bvars, var_ifvars)

and aeq_binding : unit -> binding -> binding -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_id1, _term_type0), (_id3, _term_type2) ->
      aeq_id () _id1 _id3;
      aeq_term_type () _term_type0 _term_type2;
      ()

and freshen2_binding : Var.Subst.t * Var.Subst.t -> binding -> binding -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_id1, _term_type0), (_id3, _term_type2) ->
      let (var_env1, var_env2) = freshen2_id (var_env1, var_env2) _id1 _id3 in
      (var_env1, var_env2)

and import_function_output : var Identifier.Map.t -> Raw.function_output -> function_output = fun (var_env) -> function
  (_function_output_abs0) ->
      let (var_bvars) = bvi_function_output_abs _function_output_abs0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _function_output_abs0 = import_function_output_abs (var_ienv) _function_output_abs0 in
    (create_function_output_abs _function_output_abs0)

and subst_function_output : Var.Subst.t -> function_output -> function_output = fun (var_env) -> function
  (_function_output_abs0) ->
    (apply_function_output_abs (var_env) _function_output_abs0)

and export_function_output : Var.AtomIdMap.t -> function_output -> Raw.function_output = fun (var_m) -> function
  (_function_output_abs0) ->
      let function_output_abs = open_function_output_abs _function_output_abs0 in
      let (var_bvars) = bound_function_output_abs function_output_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
    (export_function_output_abs (var_im) function_output_abs)

and flatten_function_output : function_output -> Flat.function_output = function
  (_function_output_abs0) ->
      let function_output_abs = open_function_output_abs _function_output_abs0 in
    (flatten_function_output_abs function_output_abs)

and unflatten_function_output : Flat.function_output -> function_output = function
  (_function_output_abs0) ->
      let function_output_abs = unflatten_function_output_abs _function_output_abs0 in
    (create_function_output_abs function_output_abs)

and free_function_output : function_output -> Var.AtomSet.t = 
  function function_output -> free_accu_function_output (Var.AtomSet.empty) function_output

and equal_function_output : function_output -> function_output -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_function_output x1 x2

and aeq_function_output : unit -> function_output -> function_output -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_function_output_abs0), (_function_output_abs1) ->
      let _function_output_abs0, _function_output_abs1 = open2i_function_output_abs _function_output_abs0 _function_output_abs1 in
      aeq_function_output_abs () _function_output_abs0 _function_output_abs1;
      ()

and free_accu_function_output = fun (var_fvars) -> function
  (_function_output_abs0) ->
      let function_output_abs = open_function_output_abs _function_output_abs0 in
      let (var_bvars, var_ifvars) = bound_free_accu_function_output_abs (Var.AtomSet.empty, Var.AtomSet.empty) function_output_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)

and import_clause : var Identifier.Map.t -> Raw.clause -> clause = fun (var_env) -> function
  (_clause_abs0) ->
      let (var_bvars) = bvi_clause_abs _clause_abs0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _clause_abs0 = import_clause_abs (var_env, var_ienv) _clause_abs0 in
    (create_clause_abs _clause_abs0)

and subst_clause : Var.Subst.t -> clause -> clause = fun (var_env) -> function
  (_clause_abs0) ->
    (apply_clause_abs (var_env) _clause_abs0)

and export_clause : Var.AtomIdMap.t -> clause -> Raw.clause = fun (var_m) -> function
  (_clause_abs0) ->
      let clause_abs = open_clause_abs _clause_abs0 in
      let (var_bvars) = bound_clause_abs clause_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
    (export_clause_abs (var_m, var_im) clause_abs)

and flatten_clause : clause -> Flat.clause = function
  (_clause_abs0) ->
      let clause_abs = open_clause_abs _clause_abs0 in
    (flatten_clause_abs clause_abs)

and unflatten_clause : Flat.clause -> clause = function
  (_clause_abs0) ->
      let clause_abs = unflatten_clause_abs _clause_abs0 in
    (create_clause_abs clause_abs)

and free_clause : clause -> Var.AtomSet.t = 
  function clause -> free_accu_clause (Var.AtomSet.empty) clause

and equal_clause : clause -> clause -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_clause x1 x2

and aeq_clause : unit -> clause -> clause -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_clause_abs0), (_clause_abs1) ->
      let _clause_abs0, _clause_abs1 = open2i_clause_abs _clause_abs0 _clause_abs1 in
      aeq_clause_abs () _clause_abs0 _clause_abs1;
      ()

and free_accu_clause = fun (var_fvars) -> function
  (_clause_abs0) ->
      let clause_abs = open_clause_abs _clause_abs0 in
      let (var_bvars, var_ifvars, var_fvars) = bound_free_accu_clause_abs (Var.AtomSet.empty, Var.AtomSet.empty, var_fvars) clause_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)

and subst_pattern : Var.Subst.t * Var.Subst.t -> pattern -> pattern = fun (var_oenv, var_ienv) -> function
  | PVar (_id0) ->
      PVar ((subst_id (var_ienv)) _id0)
  | PApp (_constructor1, _patterns0) ->
      PApp ((subst_constructor (var_oenv)) _constructor1, List.map (subst_pattern (var_oenv, var_ienv)) _patterns0)

and bound_pattern : pattern -> Var.AtomSet.t = 
  function pattern -> bound_accu_pattern (Var.AtomSet.empty) pattern

and bound_free_pattern : pattern -> Var.AtomSet.t * Var.AtomSet.t = 
  function pattern -> bound_free_accu_pattern (Var.AtomSet.empty, Var.AtomSet.empty) pattern

and equal_pattern : pattern -> pattern -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_pattern x1 x2

and import_pattern = fun (var_oenv, var_ienv) -> function
  | Raw.PVar (_id0) ->
      PVar ((import_id (var_ienv)) _id0)
  | Raw.PApp (_constructor1, _patterns0) ->
      PApp ((import_constructor (var_oenv)) _constructor1, List.map (import_pattern (var_oenv, var_ienv)) _patterns0)

and bvi_accu_pattern = fun (var_bvars) -> function
  | Raw.PVar (_id0) ->
      let (var_bvars) = bvi_accu_id (var_bvars) _id0 in
      (var_bvars)
  | Raw.PApp (_constructor1, _patterns0) ->
      let (var_bvars) = List.fold_left bvi_accu_pattern (var_bvars) _patterns0 in
      (var_bvars)

and bvi_pattern = 
  function pattern -> bvi_accu_pattern (Identifier.Map.empty) pattern

and bound_accu_pattern = fun (var_bvars) -> function
  | PVar (_id0) ->
      let (var_bvars) = bound_accu_id (var_bvars) _id0 in
      (var_bvars)
  | PApp (_constructor1, _patterns0) ->
      let (var_bvars) = List.fold_left bound_accu_pattern (var_bvars) _patterns0 in
      (var_bvars)

and export_pattern : Var.AtomIdMap.t * Var.AtomIdMap.t -> pattern -> Raw.pattern = fun (var_om, var_im) -> function
  | PVar (_id0) ->
      Raw.PVar ((export_id (var_im)) _id0)
  | PApp (_constructor1, _patterns0) ->
      Raw.PApp ((export_constructor (var_om)) _constructor1, List.map (export_pattern (var_om, var_im)) _patterns0)

and flatten_pattern : pattern -> Flat.pattern = function
  | PVar (_id0) ->
      Flat.PVar (flatten_id _id0)
  | PApp (_constructor1, _patterns0) ->
      Flat.PApp (flatten_constructor _constructor1, List.map flatten_pattern _patterns0)

and unflatten_pattern : Flat.pattern -> pattern = function
  | Flat.PVar (_id0) ->
      PVar (unflatten_id _id0)
  | Flat.PApp (_constructor1, _patterns0) ->
      PApp (unflatten_constructor _constructor1, List.map unflatten_pattern _patterns0)

and bound_free_accu_pattern = fun (var_bvars, var_ofvars) -> function
  | PVar (_id0) ->
      let (var_bvars) = bound_free_accu_id (var_bvars) _id0 in
      (var_bvars, var_ofvars)
  | PApp (_constructor1, _patterns0) ->
      let (var_ofvars) = free_accu_constructor (var_ofvars) _constructor1 in
      let (var_bvars, var_ofvars) = List.fold_left bound_free_accu_pattern (var_bvars, var_ofvars) _patterns0 in
      (var_bvars, var_ofvars)

and aeq_pattern : unit -> pattern -> pattern -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | PVar (_id0), PVar (_id1) ->
      aeq_id () _id0 _id1;
      ()
  | PApp (_constructor1, _patterns0), PApp (_constructor3, _patterns2) ->
      aeq_constructor () _constructor1 _constructor3;
      List.fold_left2 aeq_pattern () _patterns0 _patterns2;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_pattern")

and freshen2_pattern : Var.Subst.t * Var.Subst.t -> pattern -> pattern -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | PVar (_id0), PVar (_id1) ->
      let (var_env1, var_env2) = freshen2_id (var_env1, var_env2) _id0 _id1 in
      (var_env1, var_env2)
  | PApp (_constructor1, _patterns0), PApp (_constructor3, _patterns2) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_pattern (var_env1, var_env2) _patterns0 _patterns2 in
      (var_env1, var_env2)
  | _, _ ->
      raise (Invalid_argument "freshen2_pattern")

and import_constructor : var Identifier.Map.t -> Raw.constructor -> constructor = fun (var_env) -> function
  (_var0) ->
    (Var.find _var0 var_env)

and subst_constructor : Var.Subst.t -> constructor -> constructor = fun (var_env) -> function
  (_var0) ->
    (Var.Subst.lookup _var0 var_env)

and export_constructor : Var.AtomIdMap.t -> constructor -> Raw.constructor = fun (var_m) -> function
  (_var0) ->
    (Var.AtomIdMap.lookup _var0 var_m)

and flatten_constructor : constructor -> Flat.constructor = function
  (_var0) ->
    (_var0)

and unflatten_constructor : Flat.constructor -> constructor = function
  (_var0) ->
    (_var0)

and free_constructor : constructor -> Var.AtomSet.t = 
  function constructor -> free_accu_constructor (Var.AtomSet.empty) constructor

and equal_constructor : constructor -> constructor -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_constructor x1 x2

and aeq_constructor : unit -> constructor -> constructor -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var0), (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_constructor");
      ()

and free_accu_constructor = fun (var_fvars) -> function
  (_var0) ->
      let var_fvars = Var.AtomSet.add _var0 var_fvars in
      (var_fvars)

and import_term_type : var Identifier.Map.t -> Raw.term_type -> term_type = fun (var_env) -> function
  | Raw.TPrimitive (_primitive_type0) ->
      TPrimitive ((import_primitive_type ()) _primitive_type0)
  | Raw.TVar (_var0) ->
      TVar (Var.find _var0 var_env)
  | Raw.TArrow (_term_type1, _term_type0) ->
      TArrow ((import_term_type (var_env)) _term_type1, (import_term_type (var_env)) _term_type0)
  | Raw.TProd (_term_types0) ->
      TProd (List.map (import_term_type (var_env)) _term_types0)
  | Raw.TApp (_var1, _term_types0) ->
      TApp (Var.find _var1 var_env, List.map (import_term_type (var_env)) _term_types0)

and subst_term_type : Var.Subst.t -> term_type -> term_type = fun (var_env) -> function
  | TPrimitive (_primitive_type0) ->
      TPrimitive ((subst_primitive_type ()) _primitive_type0)
  | TVar (_var0) ->
      TVar (Var.Subst.lookup _var0 var_env)
  | TArrow (_term_type1, _term_type0) ->
      TArrow ((subst_term_type (var_env)) _term_type1, (subst_term_type (var_env)) _term_type0)
  | TProd (_term_types0) ->
      TProd (List.map (subst_term_type (var_env)) _term_types0)
  | TApp (_var1, _term_types0) ->
      TApp (Var.Subst.lookup _var1 var_env, List.map (subst_term_type (var_env)) _term_types0)

and export_term_type : Var.AtomIdMap.t -> term_type -> Raw.term_type = fun (var_m) -> function
  | TPrimitive (_primitive_type0) ->
      Raw.TPrimitive ((export_primitive_type ()) _primitive_type0)
  | TVar (_var0) ->
      Raw.TVar (Var.AtomIdMap.lookup _var0 var_m)
  | TArrow (_term_type1, _term_type0) ->
      Raw.TArrow ((export_term_type (var_m)) _term_type1, (export_term_type (var_m)) _term_type0)
  | TProd (_term_types0) ->
      Raw.TProd (List.map (export_term_type (var_m)) _term_types0)
  | TApp (_var1, _term_types0) ->
      Raw.TApp (Var.AtomIdMap.lookup _var1 var_m, List.map (export_term_type (var_m)) _term_types0)

and flatten_term_type : term_type -> Flat.term_type = function
  | TPrimitive (_primitive_type0) ->
      Flat.TPrimitive (flatten_primitive_type _primitive_type0)
  | TVar (_var0) ->
      Flat.TVar (_var0)
  | TArrow (_term_type1, _term_type0) ->
      Flat.TArrow (flatten_term_type _term_type1, flatten_term_type _term_type0)
  | TProd (_term_types0) ->
      Flat.TProd (List.map flatten_term_type _term_types0)
  | TApp (_var1, _term_types0) ->
      Flat.TApp (_var1, List.map flatten_term_type _term_types0)

and unflatten_term_type : Flat.term_type -> term_type = function
  | Flat.TPrimitive (_primitive_type0) ->
      TPrimitive (unflatten_primitive_type _primitive_type0)
  | Flat.TVar (_var0) ->
      TVar (_var0)
  | Flat.TArrow (_term_type1, _term_type0) ->
      TArrow (unflatten_term_type _term_type1, unflatten_term_type _term_type0)
  | Flat.TProd (_term_types0) ->
      TProd (List.map unflatten_term_type _term_types0)
  | Flat.TApp (_var1, _term_types0) ->
      TApp (_var1, List.map unflatten_term_type _term_types0)

and free_term_type : term_type -> Var.AtomSet.t = 
  function term_type -> free_accu_term_type (Var.AtomSet.empty) term_type

and equal_term_type : term_type -> term_type -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_term_type x1 x2

and aeq_term_type : unit -> term_type -> term_type -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | TPrimitive (_primitive_type0), TPrimitive (_primitive_type1) ->
      aeq_primitive_type () _primitive_type0 _primitive_type1;
      ()
  | TVar (_var0), TVar (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_term_type");
      ()
  | TArrow (_term_type1, _term_type0), TArrow (_term_type3, _term_type2) ->
      aeq_term_type () _term_type1 _term_type3;
      aeq_term_type () _term_type0 _term_type2;
      ()
  | TProd (_term_types0), TProd (_term_types1) ->
      List.fold_left2 aeq_term_type () _term_types0 _term_types1;
      ()
  | TApp (_var1, _term_types0), TApp (_var3, _term_types2) ->
      if not (Var.Atom.equal _var1 _var3) then raise (Invalid_argument "aeq_term_type");
      List.fold_left2 aeq_term_type () _term_types0 _term_types2;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_term_type")

and free_accu_term_type = fun (var_fvars) -> function
  | TPrimitive (_primitive_type0) ->
      let () = free_accu_primitive_type () _primitive_type0 in 
      (var_fvars)
  | TVar (_var0) ->
      let var_fvars = Var.AtomSet.add _var0 var_fvars in
      (var_fvars)
  | TArrow (_term_type1, _term_type0) ->
      let (var_fvars) = free_accu_term_type (var_fvars) _term_type1 in 
      let (var_fvars) = free_accu_term_type (var_fvars) _term_type0 in 
      (var_fvars)
  | TProd (_term_types0) ->
      let (var_fvars) = List.fold_left free_accu_term_type (var_fvars) _term_types0 in 
      (var_fvars)
  | TApp (_var1, _term_types0) ->
      let var_fvars = Var.AtomSet.add _var1 var_fvars in
      let (var_fvars) = List.fold_left free_accu_term_type (var_fvars) _term_types0 in 
      (var_fvars)

and import_primitive_type : unit -> Raw.primitive_type -> primitive_type = fun () -> function
  | Raw.TInt ->
      TInt
  | Raw.TBool ->
      TBool
  | Raw.TUnit ->
      TUnit

and subst_primitive_type : unit -> primitive_type -> primitive_type = fun () -> function
  | TInt ->
      TInt
  | TBool ->
      TBool
  | TUnit ->
      TUnit

and export_primitive_type : unit -> primitive_type -> Raw.primitive_type = fun () -> function
  | TInt ->
      Raw.TInt
  | TBool ->
      Raw.TBool
  | TUnit ->
      Raw.TUnit

and flatten_primitive_type : primitive_type -> Flat.primitive_type = function
  | TInt ->
      Flat.TInt
  | TBool ->
      Flat.TBool
  | TUnit ->
      Flat.TUnit

and unflatten_primitive_type : Flat.primitive_type -> primitive_type = function
  | Flat.TInt ->
      TInt
  | Flat.TBool ->
      TBool
  | Flat.TUnit ->
      TUnit

and free_primitive_type : primitive_type -> unit = 
  function primitive_type -> free_accu_primitive_type () primitive_type

and equal_primitive_type : primitive_type -> primitive_type -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_primitive_type x1 x2

and aeq_primitive_type : unit -> primitive_type -> primitive_type -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | TInt, TInt ->
      ()
  | TBool, TBool ->
      ()
  | TUnit, TUnit ->
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_primitive_type")

and free_accu_primitive_type = fun () -> function
  | TInt ->
      ()
  | TBool ->
      ()
  | TUnit ->
      ()

and import_type_scheme : var Identifier.Map.t -> Raw.type_scheme -> type_scheme = fun (var_env) -> function
  | Raw.TScheme (_scheme_abs0) ->
      let (var_bvars) = bvi_scheme_abs _scheme_abs0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _scheme_abs0 = import_scheme_abs (var_ienv) _scheme_abs0 in
      TScheme (create_scheme_abs _scheme_abs0)

and subst_type_scheme : Var.Subst.t -> type_scheme -> type_scheme = fun (var_env) -> function
  | TScheme (_scheme_abs0) ->
      TScheme (apply_scheme_abs (var_env) _scheme_abs0)

and export_type_scheme : Var.AtomIdMap.t -> type_scheme -> Raw.type_scheme = fun (var_m) -> function
  | TScheme (_scheme_abs0) ->
      let scheme_abs = open_scheme_abs _scheme_abs0 in
      let (var_bvars) = bound_scheme_abs scheme_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.TScheme (export_scheme_abs (var_im) scheme_abs)

and flatten_type_scheme : type_scheme -> Flat.type_scheme = function
  | TScheme (_scheme_abs0) ->
      let scheme_abs = open_scheme_abs _scheme_abs0 in
      Flat.TScheme (flatten_scheme_abs scheme_abs)

and unflatten_type_scheme : Flat.type_scheme -> type_scheme = function
  | Flat.TScheme (_scheme_abs0) ->
      let scheme_abs = unflatten_scheme_abs _scheme_abs0 in
      TScheme (create_scheme_abs scheme_abs)

and free_type_scheme : type_scheme -> Var.AtomSet.t = 
  function type_scheme -> free_accu_type_scheme (Var.AtomSet.empty) type_scheme

and equal_type_scheme : type_scheme -> type_scheme -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_type_scheme x1 x2

and aeq_type_scheme : unit -> type_scheme -> type_scheme -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | TScheme (_scheme_abs0), TScheme (_scheme_abs1) ->
      let _scheme_abs0, _scheme_abs1 = open2i_scheme_abs _scheme_abs0 _scheme_abs1 in
      aeq_scheme_abs () _scheme_abs0 _scheme_abs1;
      ()

and free_accu_type_scheme = fun (var_fvars) -> function
  | TScheme (_scheme_abs0) ->
      let scheme_abs = open_scheme_abs _scheme_abs0 in
      let (var_bvars, var_ifvars) = bound_free_accu_scheme_abs (Var.AtomSet.empty, Var.AtomSet.empty) scheme_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)

and subst_type_parameters : Var.Subst.t -> type_parameters -> type_parameters = fun (var_ienv) -> function
  (_type_parameters0) ->
    (List.map (subst_type_parameter (var_ienv)) _type_parameters0)

and bound_type_parameters : type_parameters -> Var.AtomSet.t = 
  function type_parameters -> bound_accu_type_parameters (Var.AtomSet.empty) type_parameters

and bound_free_type_parameters : type_parameters -> Var.AtomSet.t = 
  function type_parameters -> bound_free_accu_type_parameters (Var.AtomSet.empty) type_parameters

and equal_type_parameters : type_parameters -> type_parameters -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_type_parameters x1 x2

and import_type_parameters = fun (var_ienv) -> function
  (_type_parameters0) ->
    (List.map (import_type_parameter (var_ienv)) _type_parameters0)

and bvi_accu_type_parameters = fun (var_bvars) -> function
  (_type_parameters0) ->
      let (var_bvars) = List.fold_left bvi_accu_type_parameter (var_bvars) _type_parameters0 in
      (var_bvars)

and bvi_type_parameters = 
  function type_parameters -> bvi_accu_type_parameters (Identifier.Map.empty) type_parameters

and bound_accu_type_parameters = fun (var_bvars) -> function
  (_type_parameters0) ->
      let (var_bvars) = List.fold_left bound_accu_type_parameter (var_bvars) _type_parameters0 in
      (var_bvars)

and export_type_parameters : Var.AtomIdMap.t -> type_parameters -> Raw.type_parameters = fun (var_im) -> function
  (_type_parameters0) ->
    (List.map (export_type_parameter (var_im)) _type_parameters0)

and flatten_type_parameters : type_parameters -> Flat.type_parameters = function
  (_type_parameters0) ->
    (List.map flatten_type_parameter _type_parameters0)

and unflatten_type_parameters : Flat.type_parameters -> type_parameters = function
  (_type_parameters0) ->
    (List.map unflatten_type_parameter _type_parameters0)

and bound_free_accu_type_parameters = fun (var_bvars) -> function
  (_type_parameters0) ->
      let (var_bvars) = List.fold_left bound_free_accu_type_parameter (var_bvars) _type_parameters0 in
      (var_bvars)

and aeq_type_parameters : unit -> type_parameters -> type_parameters -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_type_parameters0), (_type_parameters1) ->
      List.fold_left2 aeq_type_parameter () _type_parameters0 _type_parameters1;
      ()

and freshen2_type_parameters : Var.Subst.t * Var.Subst.t -> type_parameters -> type_parameters -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_type_parameters0), (_type_parameters1) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_type_parameter (var_env1, var_env2) _type_parameters0 _type_parameters1 in
      (var_env1, var_env2)

and subst_type_parameter : Var.Subst.t -> type_parameter -> type_parameter = fun (var_ienv) -> function
  (_var0) ->
    (Var.Subst.lookup _var0 var_ienv)

and bound_type_parameter : type_parameter -> Var.AtomSet.t = 
  function type_parameter -> bound_accu_type_parameter (Var.AtomSet.empty) type_parameter

and bound_free_type_parameter : type_parameter -> Var.AtomSet.t = 
  function type_parameter -> bound_free_accu_type_parameter (Var.AtomSet.empty) type_parameter

and equal_type_parameter : type_parameter -> type_parameter -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_type_parameter x1 x2

and import_type_parameter = fun (var_ienv) -> function
  (_var0) ->
    (Var.find _var0 var_ienv)

and bvi_accu_type_parameter = fun (var_bvars) -> function
  (_var0) ->
      let var_bvars = Identifier.Map.add _var0 () var_bvars in
      (var_bvars)

and bvi_type_parameter = 
  function type_parameter -> bvi_accu_type_parameter (Identifier.Map.empty) type_parameter

and bound_accu_type_parameter = fun (var_bvars) -> function
  (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars)

and export_type_parameter : Var.AtomIdMap.t -> type_parameter -> Raw.type_parameter = fun (var_im) -> function
  (_var0) ->
    (Var.AtomIdMap.lookup _var0 var_im)

and flatten_type_parameter : type_parameter -> Flat.type_parameter = function
  (_var0) ->
    (_var0)

and unflatten_type_parameter : Flat.type_parameter -> type_parameter = function
  (_var0) ->
    (_var0)

and bound_free_accu_type_parameter = fun (var_bvars) -> function
  (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars)

and aeq_type_parameter : unit -> type_parameter -> type_parameter -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var0), (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_type_parameter");
      ()

and freshen2_type_parameter : Var.Subst.t * Var.Subst.t -> type_parameter -> type_parameter -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var0), (_var1) ->
      let var_env1, var_env2 = Var.Subst.freshen2 _var0 var_env1 _var1 var_env2 in
      (var_env1, var_env2)

and import_kind : unit -> Raw.kind -> kind = fun () -> function
  | Raw.KStar ->
      KStar
  | Raw.KArrow (_kinds0) ->
      KArrow (List.map (import_kind ()) _kinds0)

and subst_kind : unit -> kind -> kind = fun () -> function
  | KStar ->
      KStar
  | KArrow (_kinds0) ->
      KArrow (List.map (subst_kind ()) _kinds0)

and export_kind : unit -> kind -> Raw.kind = fun () -> function
  | KStar ->
      Raw.KStar
  | KArrow (_kinds0) ->
      Raw.KArrow (List.map (export_kind ()) _kinds0)

and flatten_kind : kind -> Flat.kind = function
  | KStar ->
      Flat.KStar
  | KArrow (_kinds0) ->
      Flat.KArrow (List.map flatten_kind _kinds0)

and unflatten_kind : Flat.kind -> kind = function
  | Flat.KStar ->
      KStar
  | Flat.KArrow (_kinds0) ->
      KArrow (List.map unflatten_kind _kinds0)

and free_kind : kind -> unit = 
  function kind -> free_accu_kind () kind

and equal_kind : kind -> kind -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_kind x1 x2

and aeq_kind : unit -> kind -> kind -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | KStar, KStar ->
      ()
  | KArrow (_kinds0), KArrow (_kinds1) ->
      List.fold_left2 aeq_kind () _kinds0 _kinds1;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_kind")

and free_accu_kind = fun () -> function
  | KStar ->
      ()
  | KArrow (_kinds0) ->
      let () = List.fold_left free_accu_kind () _kinds0 in 
      ()

and subst_type_definition : Var.Subst.t -> type_definition -> type_definition = fun (var_ienv) -> function
  | DAlgebraic (_type_parameters1, _dataconstructor_definitions0) ->
      DAlgebraic ((subst_type_parameters (var_ienv)) _type_parameters1, List.map (subst_dataconstructor_definition (var_ienv)) _dataconstructor_definitions0)
  | DeferredType ->
      DeferredType

and bound_type_definition : type_definition -> Var.AtomSet.t = 
  function type_definition -> bound_accu_type_definition (Var.AtomSet.empty) type_definition

and bound_free_type_definition : type_definition -> Var.AtomSet.t * Var.AtomSet.t = 
  function type_definition -> bound_free_accu_type_definition (Var.AtomSet.empty, Var.AtomSet.empty) type_definition

and equal_type_definition : type_definition -> type_definition -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_type_definition x1 x2

and import_type_definition = fun (var_ienv) -> function
  | Raw.DAlgebraic (_type_parameters1, _dataconstructor_definitions0) ->
      DAlgebraic ((import_type_parameters (var_ienv)) _type_parameters1, List.map (import_dataconstructor_definition (var_ienv)) _dataconstructor_definitions0)
  | Raw.DeferredType ->
      DeferredType

and bvi_accu_type_definition = fun (var_bvars) -> function
  | Raw.DAlgebraic (_type_parameters1, _dataconstructor_definitions0) ->
      let (var_bvars) = bvi_accu_type_parameters (var_bvars) _type_parameters1 in
      let (var_bvars) = List.fold_left bvi_accu_dataconstructor_definition (var_bvars) _dataconstructor_definitions0 in
      (var_bvars)
  | Raw.DeferredType ->
      (var_bvars)

and bvi_type_definition = 
  function type_definition -> bvi_accu_type_definition (Identifier.Map.empty) type_definition

and bound_accu_type_definition = fun (var_bvars) -> function
  | DAlgebraic (_type_parameters1, _dataconstructor_definitions0) ->
      let (var_bvars) = bound_accu_type_parameters (var_bvars) _type_parameters1 in
      let (var_bvars) = List.fold_left bound_accu_dataconstructor_definition (var_bvars) _dataconstructor_definitions0 in
      (var_bvars)
  | DeferredType ->
      (var_bvars)

and export_type_definition : Var.AtomIdMap.t -> type_definition -> Raw.type_definition = fun (var_im) -> function
  | DAlgebraic (_type_parameters1, _dataconstructor_definitions0) ->
      Raw.DAlgebraic ((export_type_parameters (var_im)) _type_parameters1, List.map (export_dataconstructor_definition (var_im)) _dataconstructor_definitions0)
  | DeferredType ->
      Raw.DeferredType

and flatten_type_definition : type_definition -> Flat.type_definition = function
  | DAlgebraic (_type_parameters1, _dataconstructor_definitions0) ->
      Flat.DAlgebraic (flatten_type_parameters _type_parameters1, List.map flatten_dataconstructor_definition _dataconstructor_definitions0)
  | DeferredType ->
      Flat.DeferredType

and unflatten_type_definition : Flat.type_definition -> type_definition = function
  | Flat.DAlgebraic (_type_parameters1, _dataconstructor_definitions0) ->
      DAlgebraic (unflatten_type_parameters _type_parameters1, List.map unflatten_dataconstructor_definition _dataconstructor_definitions0)
  | Flat.DeferredType ->
      DeferredType

and bound_free_accu_type_definition = fun (var_bvars, var_ifvars) -> function
  | DAlgebraic (_type_parameters1, _dataconstructor_definitions0) ->
      let (var_bvars) = bound_free_accu_type_parameters (var_bvars) _type_parameters1 in
      let (var_bvars, var_ifvars) = List.fold_left bound_free_accu_dataconstructor_definition (var_bvars, var_ifvars) _dataconstructor_definitions0 in
      (var_bvars, var_ifvars)
  | DeferredType ->
      (var_bvars, var_ifvars)

and aeq_type_definition : unit -> type_definition -> type_definition -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | DAlgebraic (_type_parameters1, _dataconstructor_definitions0), DAlgebraic (_type_parameters3, _dataconstructor_definitions2) ->
      aeq_type_parameters () _type_parameters1 _type_parameters3;
      List.fold_left2 aeq_dataconstructor_definition () _dataconstructor_definitions0 _dataconstructor_definitions2;
      ()
  | DeferredType, DeferredType ->
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_type_definition")

and freshen2_type_definition : Var.Subst.t * Var.Subst.t -> type_definition -> type_definition -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | DAlgebraic (_type_parameters1, _dataconstructor_definitions0), DAlgebraic (_type_parameters3, _dataconstructor_definitions2) ->
      let (var_env1, var_env2) = freshen2_type_parameters (var_env1, var_env2) _type_parameters1 _type_parameters3 in
      let (var_env1, var_env2) = List.fold_left2 freshen2_dataconstructor_definition (var_env1, var_env2) _dataconstructor_definitions0 _dataconstructor_definitions2 in
      (var_env1, var_env2)
  | DeferredType, DeferredType ->
      (var_env1, var_env2)
  | _, _ ->
      raise (Invalid_argument "freshen2_type_definition")

and subst_dataconstructor_definition : Var.Subst.t -> dataconstructor_definition -> dataconstructor_definition = fun (var_ienv) -> function
  (_var1, _term_type0) ->
    (Var.Subst.lookup _var1 var_ienv, (subst_term_type (var_ienv)) _term_type0)

and bound_dataconstructor_definition : dataconstructor_definition -> Var.AtomSet.t = 
  function dataconstructor_definition -> bound_accu_dataconstructor_definition (Var.AtomSet.empty) dataconstructor_definition

and bound_free_dataconstructor_definition : dataconstructor_definition -> Var.AtomSet.t * Var.AtomSet.t = 
  function dataconstructor_definition -> bound_free_accu_dataconstructor_definition (Var.AtomSet.empty, Var.AtomSet.empty) dataconstructor_definition

and equal_dataconstructor_definition : dataconstructor_definition -> dataconstructor_definition -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_dataconstructor_definition x1 x2

and import_dataconstructor_definition = fun (var_ienv) -> function
  (_var1, _term_type0) ->
    (Var.find _var1 var_ienv, (import_term_type (var_ienv)) _term_type0)

and bvi_accu_dataconstructor_definition = fun (var_bvars) -> function
  (_var1, _term_type0) ->
      let var_bvars = Identifier.Map.add _var1 () var_bvars in
      (var_bvars)

and bvi_dataconstructor_definition = 
  function dataconstructor_definition -> bvi_accu_dataconstructor_definition (Identifier.Map.empty) dataconstructor_definition

and bound_accu_dataconstructor_definition = fun (var_bvars) -> function
  (_var1, _term_type0) ->
      let var_bvars = Var.AtomSet.add _var1 var_bvars in
      (var_bvars)

and export_dataconstructor_definition : Var.AtomIdMap.t -> dataconstructor_definition -> Raw.dataconstructor_definition = fun (var_im) -> function
  (_var1, _term_type0) ->
    (Var.AtomIdMap.lookup _var1 var_im, (export_term_type (var_im)) _term_type0)

and flatten_dataconstructor_definition : dataconstructor_definition -> Flat.dataconstructor_definition = function
  (_var1, _term_type0) ->
    (_var1, flatten_term_type _term_type0)

and unflatten_dataconstructor_definition : Flat.dataconstructor_definition -> dataconstructor_definition = function
  (_var1, _term_type0) ->
    (_var1, unflatten_term_type _term_type0)

and bound_free_accu_dataconstructor_definition = fun (var_bvars, var_ifvars) -> function
  (_var1, _term_type0) ->
      let var_bvars = Var.AtomSet.add _var1 var_bvars in
      let (var_ifvars) = free_accu_term_type (var_ifvars) _term_type0 in
      (var_bvars, var_ifvars)

and aeq_dataconstructor_definition : unit -> dataconstructor_definition -> dataconstructor_definition -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var1, _term_type0), (_var3, _term_type2) ->
      if not (Var.Atom.equal _var1 _var3) then raise (Invalid_argument "aeq_dataconstructor_definition");
      aeq_term_type () _term_type0 _term_type2;
      ()

and freshen2_dataconstructor_definition : Var.Subst.t * Var.Subst.t -> dataconstructor_definition -> dataconstructor_definition -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var1, _term_type0), (_var3, _term_type2) ->
      let var_env1, var_env2 = Var.Subst.freshen2 _var1 var_env1 _var3 var_env2 in
      (var_env1, var_env2)

and import_optional_lformula : var Identifier.Map.t -> Raw.optional_lformula -> optional_lformula = fun (var_env) -> function
  | Raw.ImplicitFormula (_x0) ->
      ImplicitFormula (_x0)
  | Raw.ExplicitFormula (_lformula0) ->
      ExplicitFormula ((import_lformula (var_env)) _lformula0)

and subst_optional_lformula : Var.Subst.t -> optional_lformula -> optional_lformula = fun (var_env) -> function
  | ImplicitFormula (_x0) ->
      ImplicitFormula (_x0)
  | ExplicitFormula (_lformula0) ->
      ExplicitFormula ((subst_lformula (var_env)) _lformula0)

and export_optional_lformula : Var.AtomIdMap.t -> optional_lformula -> Raw.optional_lformula = fun (var_m) -> function
  | ImplicitFormula (_x0) ->
      Raw.ImplicitFormula (_x0)
  | ExplicitFormula (_lformula0) ->
      Raw.ExplicitFormula ((export_lformula (var_m)) _lformula0)

and flatten_optional_lformula : optional_lformula -> Flat.optional_lformula = function
  | ImplicitFormula (_x0) ->
      Flat.ImplicitFormula (_x0)
  | ExplicitFormula (_lformula0) ->
      Flat.ExplicitFormula (flatten_lformula _lformula0)

and unflatten_optional_lformula : Flat.optional_lformula -> optional_lformula = function
  | Flat.ImplicitFormula (_x0) ->
      ImplicitFormula (_x0)
  | Flat.ExplicitFormula (_lformula0) ->
      ExplicitFormula (unflatten_lformula _lformula0)

and free_optional_lformula : optional_lformula -> Var.AtomSet.t = 
  function optional_lformula -> free_accu_optional_lformula (Var.AtomSet.empty) optional_lformula

and equal_optional_lformula : optional_lformula -> optional_lformula -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_optional_lformula x1 x2

and aeq_optional_lformula : unit -> optional_lformula -> optional_lformula -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | ImplicitFormula (_x0), ImplicitFormula (_x1) ->
      ()
  | ExplicitFormula (_lformula0), ExplicitFormula (_lformula1) ->
      aeq_lformula () _lformula0 _lformula1;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_optional_lformula")

and free_accu_optional_lformula = fun (var_fvars) -> function
  | ImplicitFormula (_x0) ->
      (var_fvars)
  | ExplicitFormula (_lformula0) ->
      let (var_fvars) = free_accu_lformula (var_fvars) _lformula0 in 
      (var_fvars)

and import_formula : var Identifier.Map.t -> Raw.formula -> formula = fun (var_env) -> function
  | Raw.FTrue ->
      FTrue
  | Raw.FFalse ->
      FFalse
  | Raw.FVar (_var0) ->
      FVar (Var.find _var0 var_env)
  | Raw.FLam (_lfun_abs0) ->
      let (var_bvars) = bvi_lfun_abs _lfun_abs0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _lfun_abs0 = import_lfun_abs (var_env, var_ienv) _lfun_abs0 in
      FLam (create_lfun_abs _lfun_abs0)
  | Raw.FForallTys (_fforalltys_abs0) ->
      let (var_bvars) = bvi_fforalltys_abs _fforalltys_abs0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _fforalltys_abs0 = import_fforalltys_abs (var_ienv) _fforalltys_abs0 in
      FForallTys (create_fforalltys_abs _fforalltys_abs0)
  | Raw.FExistsTys (_fexiststys_abs0) ->
      let (var_bvars) = bvi_fexiststys_abs _fexiststys_abs0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _fexiststys_abs0 = import_fexiststys_abs (var_ienv) _fexiststys_abs0 in
      FExistsTys (create_fexiststys_abs _fexiststys_abs0)
  | Raw.FForall (_lforall_abs0) ->
      let (var_bvars) = bvi_lforall_abs _lforall_abs0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _lforall_abs0 = import_lforall_abs (var_env, var_ienv) _lforall_abs0 in
      FForall (create_lforall_abs _lforall_abs0)
  | Raw.FExists (_lexists_abs0) ->
      let (var_bvars) = bvi_lexists_abs _lexists_abs0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _lexists_abs0 = import_lexists_abs (var_env, var_ienv) _lexists_abs0 in
      FExists (create_lexists_abs _lexists_abs0)
  | Raw.FEq (_lformula1, _lformula0) ->
      FEq ((import_lformula (var_env)) _lformula1, (import_lformula (var_env)) _lformula0)
  | Raw.FApp (_lformula1, _lformulas0) ->
      FApp ((import_lformula (var_env)) _lformula1, List.map (import_lformula (var_env)) _lformulas0)
  | Raw.FKApp (_var1, _lformulas0) ->
      FKApp (Var.find _var1 var_env, List.map (import_lformula (var_env)) _lformulas0)
  | Raw.FProd (_lformulas0) ->
      FProd (List.map (import_lformula (var_env)) _lformulas0)
  | Raw.FPrimitive (_formula_primitive0) ->
      FPrimitive ((import_formula_primitive ()) _formula_primitive0)
  | Raw.FAnnot (_lformula1, _formula_type0) ->
      FAnnot ((import_lformula (var_env)) _lformula1, (import_formula_type (var_env)) _formula_type0)
  | Raw.FDeferred ->
      FDeferred

and subst_formula : Var.Subst.t -> formula -> formula = fun (var_env) -> function
  | FTrue ->
      FTrue
  | FFalse ->
      FFalse
  | FVar (_var0) ->
      FVar (Var.Subst.lookup _var0 var_env)
  | FLam (_lfun_abs0) ->
      FLam (apply_lfun_abs (var_env) _lfun_abs0)
  | FForallTys (_fforalltys_abs0) ->
      FForallTys (apply_fforalltys_abs (var_env) _fforalltys_abs0)
  | FExistsTys (_fexiststys_abs0) ->
      FExistsTys (apply_fexiststys_abs (var_env) _fexiststys_abs0)
  | FForall (_lforall_abs0) ->
      FForall (apply_lforall_abs (var_env) _lforall_abs0)
  | FExists (_lexists_abs0) ->
      FExists (apply_lexists_abs (var_env) _lexists_abs0)
  | FEq (_lformula1, _lformula0) ->
      FEq ((subst_lformula (var_env)) _lformula1, (subst_lformula (var_env)) _lformula0)
  | FApp (_lformula1, _lformulas0) ->
      FApp ((subst_lformula (var_env)) _lformula1, List.map (subst_lformula (var_env)) _lformulas0)
  | FKApp (_var1, _lformulas0) ->
      FKApp (Var.Subst.lookup _var1 var_env, List.map (subst_lformula (var_env)) _lformulas0)
  | FProd (_lformulas0) ->
      FProd (List.map (subst_lformula (var_env)) _lformulas0)
  | FPrimitive (_formula_primitive0) ->
      FPrimitive ((subst_formula_primitive ()) _formula_primitive0)
  | FAnnot (_lformula1, _formula_type0) ->
      FAnnot ((subst_lformula (var_env)) _lformula1, (subst_formula_type (var_env)) _formula_type0)
  | FDeferred ->
      FDeferred

and export_formula : Var.AtomIdMap.t -> formula -> Raw.formula = fun (var_m) -> function
  | FTrue ->
      Raw.FTrue
  | FFalse ->
      Raw.FFalse
  | FVar (_var0) ->
      Raw.FVar (Var.AtomIdMap.lookup _var0 var_m)
  | FLam (_lfun_abs0) ->
      let lfun_abs = open_lfun_abs _lfun_abs0 in
      let (var_bvars) = bound_lfun_abs lfun_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.FLam (export_lfun_abs (var_m, var_im) lfun_abs)
  | FForallTys (_fforalltys_abs0) ->
      let fforalltys_abs = open_fforalltys_abs _fforalltys_abs0 in
      let (var_bvars) = bound_fforalltys_abs fforalltys_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.FForallTys (export_fforalltys_abs (var_im) fforalltys_abs)
  | FExistsTys (_fexiststys_abs0) ->
      let fexiststys_abs = open_fexiststys_abs _fexiststys_abs0 in
      let (var_bvars) = bound_fexiststys_abs fexiststys_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.FExistsTys (export_fexiststys_abs (var_im) fexiststys_abs)
  | FForall (_lforall_abs0) ->
      let lforall_abs = open_lforall_abs _lforall_abs0 in
      let (var_bvars) = bound_lforall_abs lforall_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.FForall (export_lforall_abs (var_m, var_im) lforall_abs)
  | FExists (_lexists_abs0) ->
      let lexists_abs = open_lexists_abs _lexists_abs0 in
      let (var_bvars) = bound_lexists_abs lexists_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.FExists (export_lexists_abs (var_m, var_im) lexists_abs)
  | FEq (_lformula1, _lformula0) ->
      Raw.FEq ((export_lformula (var_m)) _lformula1, (export_lformula (var_m)) _lformula0)
  | FApp (_lformula1, _lformulas0) ->
      Raw.FApp ((export_lformula (var_m)) _lformula1, List.map (export_lformula (var_m)) _lformulas0)
  | FKApp (_var1, _lformulas0) ->
      Raw.FKApp (Var.AtomIdMap.lookup _var1 var_m, List.map (export_lformula (var_m)) _lformulas0)
  | FProd (_lformulas0) ->
      Raw.FProd (List.map (export_lformula (var_m)) _lformulas0)
  | FPrimitive (_formula_primitive0) ->
      Raw.FPrimitive ((export_formula_primitive ()) _formula_primitive0)
  | FAnnot (_lformula1, _formula_type0) ->
      Raw.FAnnot ((export_lformula (var_m)) _lformula1, (export_formula_type (var_m)) _formula_type0)
  | FDeferred ->
      Raw.FDeferred

and flatten_formula : formula -> Flat.formula = function
  | FTrue ->
      Flat.FTrue
  | FFalse ->
      Flat.FFalse
  | FVar (_var0) ->
      Flat.FVar (_var0)
  | FLam (_lfun_abs0) ->
      let lfun_abs = open_lfun_abs _lfun_abs0 in
      Flat.FLam (flatten_lfun_abs lfun_abs)
  | FForallTys (_fforalltys_abs0) ->
      let fforalltys_abs = open_fforalltys_abs _fforalltys_abs0 in
      Flat.FForallTys (flatten_fforalltys_abs fforalltys_abs)
  | FExistsTys (_fexiststys_abs0) ->
      let fexiststys_abs = open_fexiststys_abs _fexiststys_abs0 in
      Flat.FExistsTys (flatten_fexiststys_abs fexiststys_abs)
  | FForall (_lforall_abs0) ->
      let lforall_abs = open_lforall_abs _lforall_abs0 in
      Flat.FForall (flatten_lforall_abs lforall_abs)
  | FExists (_lexists_abs0) ->
      let lexists_abs = open_lexists_abs _lexists_abs0 in
      Flat.FExists (flatten_lexists_abs lexists_abs)
  | FEq (_lformula1, _lformula0) ->
      Flat.FEq (flatten_lformula _lformula1, flatten_lformula _lformula0)
  | FApp (_lformula1, _lformulas0) ->
      Flat.FApp (flatten_lformula _lformula1, List.map flatten_lformula _lformulas0)
  | FKApp (_var1, _lformulas0) ->
      Flat.FKApp (_var1, List.map flatten_lformula _lformulas0)
  | FProd (_lformulas0) ->
      Flat.FProd (List.map flatten_lformula _lformulas0)
  | FPrimitive (_formula_primitive0) ->
      Flat.FPrimitive (flatten_formula_primitive _formula_primitive0)
  | FAnnot (_lformula1, _formula_type0) ->
      Flat.FAnnot (flatten_lformula _lformula1, flatten_formula_type _formula_type0)
  | FDeferred ->
      Flat.FDeferred

and unflatten_formula : Flat.formula -> formula = function
  | Flat.FTrue ->
      FTrue
  | Flat.FFalse ->
      FFalse
  | Flat.FVar (_var0) ->
      FVar (_var0)
  | Flat.FLam (_lfun_abs0) ->
      let lfun_abs = unflatten_lfun_abs _lfun_abs0 in
      FLam (create_lfun_abs lfun_abs)
  | Flat.FForallTys (_fforalltys_abs0) ->
      let fforalltys_abs = unflatten_fforalltys_abs _fforalltys_abs0 in
      FForallTys (create_fforalltys_abs fforalltys_abs)
  | Flat.FExistsTys (_fexiststys_abs0) ->
      let fexiststys_abs = unflatten_fexiststys_abs _fexiststys_abs0 in
      FExistsTys (create_fexiststys_abs fexiststys_abs)
  | Flat.FForall (_lforall_abs0) ->
      let lforall_abs = unflatten_lforall_abs _lforall_abs0 in
      FForall (create_lforall_abs lforall_abs)
  | Flat.FExists (_lexists_abs0) ->
      let lexists_abs = unflatten_lexists_abs _lexists_abs0 in
      FExists (create_lexists_abs lexists_abs)
  | Flat.FEq (_lformula1, _lformula0) ->
      FEq (unflatten_lformula _lformula1, unflatten_lformula _lformula0)
  | Flat.FApp (_lformula1, _lformulas0) ->
      FApp (unflatten_lformula _lformula1, List.map unflatten_lformula _lformulas0)
  | Flat.FKApp (_var1, _lformulas0) ->
      FKApp (_var1, List.map unflatten_lformula _lformulas0)
  | Flat.FProd (_lformulas0) ->
      FProd (List.map unflatten_lformula _lformulas0)
  | Flat.FPrimitive (_formula_primitive0) ->
      FPrimitive (unflatten_formula_primitive _formula_primitive0)
  | Flat.FAnnot (_lformula1, _formula_type0) ->
      FAnnot (unflatten_lformula _lformula1, unflatten_formula_type _formula_type0)
  | Flat.FDeferred ->
      FDeferred

and free_formula : formula -> Var.AtomSet.t = 
  function formula -> free_accu_formula (Var.AtomSet.empty) formula

and equal_formula : formula -> formula -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_formula x1 x2

and aeq_formula : unit -> formula -> formula -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | FTrue, FTrue ->
      ()
  | FFalse, FFalse ->
      ()
  | FVar (_var0), FVar (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_formula");
      ()
  | FLam (_lfun_abs0), FLam (_lfun_abs1) ->
      let _lfun_abs0, _lfun_abs1 = open2i_lfun_abs _lfun_abs0 _lfun_abs1 in
      aeq_lfun_abs () _lfun_abs0 _lfun_abs1;
      ()
  | FForallTys (_fforalltys_abs0), FForallTys (_fforalltys_abs1) ->
      let _fforalltys_abs0, _fforalltys_abs1 = open2i_fforalltys_abs _fforalltys_abs0 _fforalltys_abs1 in
      aeq_fforalltys_abs () _fforalltys_abs0 _fforalltys_abs1;
      ()
  | FExistsTys (_fexiststys_abs0), FExistsTys (_fexiststys_abs1) ->
      let _fexiststys_abs0, _fexiststys_abs1 = open2i_fexiststys_abs _fexiststys_abs0 _fexiststys_abs1 in
      aeq_fexiststys_abs () _fexiststys_abs0 _fexiststys_abs1;
      ()
  | FForall (_lforall_abs0), FForall (_lforall_abs1) ->
      let _lforall_abs0, _lforall_abs1 = open2i_lforall_abs _lforall_abs0 _lforall_abs1 in
      aeq_lforall_abs () _lforall_abs0 _lforall_abs1;
      ()
  | FExists (_lexists_abs0), FExists (_lexists_abs1) ->
      let _lexists_abs0, _lexists_abs1 = open2i_lexists_abs _lexists_abs0 _lexists_abs1 in
      aeq_lexists_abs () _lexists_abs0 _lexists_abs1;
      ()
  | FEq (_lformula1, _lformula0), FEq (_lformula3, _lformula2) ->
      aeq_lformula () _lformula1 _lformula3;
      aeq_lformula () _lformula0 _lformula2;
      ()
  | FApp (_lformula1, _lformulas0), FApp (_lformula3, _lformulas2) ->
      aeq_lformula () _lformula1 _lformula3;
      List.fold_left2 aeq_lformula () _lformulas0 _lformulas2;
      ()
  | FKApp (_var1, _lformulas0), FKApp (_var3, _lformulas2) ->
      if not (Var.Atom.equal _var1 _var3) then raise (Invalid_argument "aeq_formula");
      List.fold_left2 aeq_lformula () _lformulas0 _lformulas2;
      ()
  | FProd (_lformulas0), FProd (_lformulas1) ->
      List.fold_left2 aeq_lformula () _lformulas0 _lformulas1;
      ()
  | FPrimitive (_formula_primitive0), FPrimitive (_formula_primitive1) ->
      aeq_formula_primitive () _formula_primitive0 _formula_primitive1;
      ()
  | FAnnot (_lformula1, _formula_type0), FAnnot (_lformula3, _formula_type2) ->
      aeq_lformula () _lformula1 _lformula3;
      aeq_formula_type () _formula_type0 _formula_type2;
      ()
  | FDeferred, FDeferred ->
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_formula")

and free_accu_formula = fun (var_fvars) -> function
  | FTrue ->
      (var_fvars)
  | FFalse ->
      (var_fvars)
  | FVar (_var0) ->
      let var_fvars = Var.AtomSet.add _var0 var_fvars in
      (var_fvars)
  | FLam (_lfun_abs0) ->
      let lfun_abs = open_lfun_abs _lfun_abs0 in
      let (var_bvars, var_ifvars, var_fvars) = bound_free_accu_lfun_abs (Var.AtomSet.empty, Var.AtomSet.empty, var_fvars) lfun_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)
  | FForallTys (_fforalltys_abs0) ->
      let fforalltys_abs = open_fforalltys_abs _fforalltys_abs0 in
      let (var_bvars, var_ifvars) = bound_free_accu_fforalltys_abs (Var.AtomSet.empty, Var.AtomSet.empty) fforalltys_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)
  | FExistsTys (_fexiststys_abs0) ->
      let fexiststys_abs = open_fexiststys_abs _fexiststys_abs0 in
      let (var_bvars, var_ifvars) = bound_free_accu_fexiststys_abs (Var.AtomSet.empty, Var.AtomSet.empty) fexiststys_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)
  | FForall (_lforall_abs0) ->
      let lforall_abs = open_lforall_abs _lforall_abs0 in
      let (var_bvars, var_ifvars, var_fvars) = bound_free_accu_lforall_abs (Var.AtomSet.empty, Var.AtomSet.empty, var_fvars) lforall_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)
  | FExists (_lexists_abs0) ->
      let lexists_abs = open_lexists_abs _lexists_abs0 in
      let (var_bvars, var_ifvars, var_fvars) = bound_free_accu_lexists_abs (Var.AtomSet.empty, Var.AtomSet.empty, var_fvars) lexists_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)
  | FEq (_lformula1, _lformula0) ->
      let (var_fvars) = free_accu_lformula (var_fvars) _lformula1 in 
      let (var_fvars) = free_accu_lformula (var_fvars) _lformula0 in 
      (var_fvars)
  | FApp (_lformula1, _lformulas0) ->
      let (var_fvars) = free_accu_lformula (var_fvars) _lformula1 in 
      let (var_fvars) = List.fold_left free_accu_lformula (var_fvars) _lformulas0 in 
      (var_fvars)
  | FKApp (_var1, _lformulas0) ->
      let var_fvars = Var.AtomSet.add _var1 var_fvars in
      let (var_fvars) = List.fold_left free_accu_lformula (var_fvars) _lformulas0 in 
      (var_fvars)
  | FProd (_lformulas0) ->
      let (var_fvars) = List.fold_left free_accu_lformula (var_fvars) _lformulas0 in 
      (var_fvars)
  | FPrimitive (_formula_primitive0) ->
      let () = free_accu_formula_primitive () _formula_primitive0 in 
      (var_fvars)
  | FAnnot (_lformula1, _formula_type0) ->
      let (var_fvars) = free_accu_lformula (var_fvars) _lformula1 in 
      let (var_fvars) = free_accu_formula_type (var_fvars) _formula_type0 in 
      (var_fvars)
  | FDeferred ->
      (var_fvars)

and import_formula_primitive : unit -> Raw.formula_primitive -> formula_primitive = fun () -> function
  | Raw.Pre ->
      Pre
  | Raw.Post ->
      Post
  | Raw.PAnd ->
      PAnd
  | Raw.POr ->
      POr
  | Raw.PEquiv ->
      PEquiv
  | Raw.PImply ->
      PImply
  | Raw.PNot ->
      PNot
  | Raw.PLessThan ->
      PLessThan
  | Raw.PGreaterThan ->
      PGreaterThan
  | Raw.PLessEqualThan ->
      PLessEqualThan
  | Raw.PGreaterEqualThan ->
      PGreaterEqualThan
  | Raw.PAdd ->
      PAdd
  | Raw.PSub ->
      PSub
  | Raw.PMult ->
      PMult
  | Raw.PDiv ->
      PDiv
  | Raw.PNeg ->
      PNeg
  | Raw.PEPrimitive (_primitive0) ->
      PEPrimitive ((import_primitive ()) _primitive0)

and subst_formula_primitive : unit -> formula_primitive -> formula_primitive = fun () -> function
  | Pre ->
      Pre
  | Post ->
      Post
  | PAnd ->
      PAnd
  | POr ->
      POr
  | PEquiv ->
      PEquiv
  | PImply ->
      PImply
  | PNot ->
      PNot
  | PLessThan ->
      PLessThan
  | PGreaterThan ->
      PGreaterThan
  | PLessEqualThan ->
      PLessEqualThan
  | PGreaterEqualThan ->
      PGreaterEqualThan
  | PAdd ->
      PAdd
  | PSub ->
      PSub
  | PMult ->
      PMult
  | PDiv ->
      PDiv
  | PNeg ->
      PNeg
  | PEPrimitive (_primitive0) ->
      PEPrimitive ((subst_primitive ()) _primitive0)

and export_formula_primitive : unit -> formula_primitive -> Raw.formula_primitive = fun () -> function
  | Pre ->
      Raw.Pre
  | Post ->
      Raw.Post
  | PAnd ->
      Raw.PAnd
  | POr ->
      Raw.POr
  | PEquiv ->
      Raw.PEquiv
  | PImply ->
      Raw.PImply
  | PNot ->
      Raw.PNot
  | PLessThan ->
      Raw.PLessThan
  | PGreaterThan ->
      Raw.PGreaterThan
  | PLessEqualThan ->
      Raw.PLessEqualThan
  | PGreaterEqualThan ->
      Raw.PGreaterEqualThan
  | PAdd ->
      Raw.PAdd
  | PSub ->
      Raw.PSub
  | PMult ->
      Raw.PMult
  | PDiv ->
      Raw.PDiv
  | PNeg ->
      Raw.PNeg
  | PEPrimitive (_primitive0) ->
      Raw.PEPrimitive ((export_primitive ()) _primitive0)

and flatten_formula_primitive : formula_primitive -> Flat.formula_primitive = function
  | Pre ->
      Flat.Pre
  | Post ->
      Flat.Post
  | PAnd ->
      Flat.PAnd
  | POr ->
      Flat.POr
  | PEquiv ->
      Flat.PEquiv
  | PImply ->
      Flat.PImply
  | PNot ->
      Flat.PNot
  | PLessThan ->
      Flat.PLessThan
  | PGreaterThan ->
      Flat.PGreaterThan
  | PLessEqualThan ->
      Flat.PLessEqualThan
  | PGreaterEqualThan ->
      Flat.PGreaterEqualThan
  | PAdd ->
      Flat.PAdd
  | PSub ->
      Flat.PSub
  | PMult ->
      Flat.PMult
  | PDiv ->
      Flat.PDiv
  | PNeg ->
      Flat.PNeg
  | PEPrimitive (_primitive0) ->
      Flat.PEPrimitive (flatten_primitive _primitive0)

and unflatten_formula_primitive : Flat.formula_primitive -> formula_primitive = function
  | Flat.Pre ->
      Pre
  | Flat.Post ->
      Post
  | Flat.PAnd ->
      PAnd
  | Flat.POr ->
      POr
  | Flat.PEquiv ->
      PEquiv
  | Flat.PImply ->
      PImply
  | Flat.PNot ->
      PNot
  | Flat.PLessThan ->
      PLessThan
  | Flat.PGreaterThan ->
      PGreaterThan
  | Flat.PLessEqualThan ->
      PLessEqualThan
  | Flat.PGreaterEqualThan ->
      PGreaterEqualThan
  | Flat.PAdd ->
      PAdd
  | Flat.PSub ->
      PSub
  | Flat.PMult ->
      PMult
  | Flat.PDiv ->
      PDiv
  | Flat.PNeg ->
      PNeg
  | Flat.PEPrimitive (_primitive0) ->
      PEPrimitive (unflatten_primitive _primitive0)

and free_formula_primitive : formula_primitive -> unit = 
  function formula_primitive -> free_accu_formula_primitive () formula_primitive

and equal_formula_primitive : formula_primitive -> formula_primitive -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_formula_primitive x1 x2

and aeq_formula_primitive : unit -> formula_primitive -> formula_primitive -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | Pre, Pre ->
      ()
  | Post, Post ->
      ()
  | PAnd, PAnd ->
      ()
  | POr, POr ->
      ()
  | PEquiv, PEquiv ->
      ()
  | PImply, PImply ->
      ()
  | PNot, PNot ->
      ()
  | PLessThan, PLessThan ->
      ()
  | PGreaterThan, PGreaterThan ->
      ()
  | PLessEqualThan, PLessEqualThan ->
      ()
  | PGreaterEqualThan, PGreaterEqualThan ->
      ()
  | PAdd, PAdd ->
      ()
  | PSub, PSub ->
      ()
  | PMult, PMult ->
      ()
  | PDiv, PDiv ->
      ()
  | PNeg, PNeg ->
      ()
  | PEPrimitive (_primitive0), PEPrimitive (_primitive1) ->
      aeq_primitive () _primitive0 _primitive1;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_formula_primitive")

and free_accu_formula_primitive = fun () -> function
  | Pre ->
      ()
  | Post ->
      ()
  | PAnd ->
      ()
  | POr ->
      ()
  | PEquiv ->
      ()
  | PImply ->
      ()
  | PNot ->
      ()
  | PLessThan ->
      ()
  | PGreaterThan ->
      ()
  | PLessEqualThan ->
      ()
  | PGreaterEqualThan ->
      ()
  | PAdd ->
      ()
  | PSub ->
      ()
  | PMult ->
      ()
  | PDiv ->
      ()
  | PNeg ->
      ()
  | PEPrimitive (_primitive0) ->
      let () = free_accu_primitive () _primitive0 in 
      ()

and subst_fbindings : Var.Subst.t * Var.Subst.t -> fbindings -> fbindings = fun (var_oenv, var_ienv) -> function
  (_fbindings0) ->
    (List.map (subst_fbinding (var_oenv, var_ienv)) _fbindings0)

and bound_fbindings : fbindings -> Var.AtomSet.t = 
  function fbindings -> bound_accu_fbindings (Var.AtomSet.empty) fbindings

and bound_free_fbindings : fbindings -> Var.AtomSet.t * Var.AtomSet.t = 
  function fbindings -> bound_free_accu_fbindings (Var.AtomSet.empty, Var.AtomSet.empty) fbindings

and equal_fbindings : fbindings -> fbindings -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_fbindings x1 x2

and import_fbindings = fun (var_oenv, var_ienv) -> function
  (_fbindings0) ->
    (List.map (import_fbinding (var_oenv, var_ienv)) _fbindings0)

and bvi_accu_fbindings = fun (var_bvars) -> function
  (_fbindings0) ->
      let (var_bvars) = List.fold_left bvi_accu_fbinding (var_bvars) _fbindings0 in
      (var_bvars)

and bvi_fbindings = 
  function fbindings -> bvi_accu_fbindings (Identifier.Map.empty) fbindings

and bound_accu_fbindings = fun (var_bvars) -> function
  (_fbindings0) ->
      let (var_bvars) = List.fold_left bound_accu_fbinding (var_bvars) _fbindings0 in
      (var_bvars)

and export_fbindings : Var.AtomIdMap.t * Var.AtomIdMap.t -> fbindings -> Raw.fbindings = fun (var_om, var_im) -> function
  (_fbindings0) ->
    (List.map (export_fbinding (var_om, var_im)) _fbindings0)

and flatten_fbindings : fbindings -> Flat.fbindings = function
  (_fbindings0) ->
    (List.map flatten_fbinding _fbindings0)

and unflatten_fbindings : Flat.fbindings -> fbindings = function
  (_fbindings0) ->
    (List.map unflatten_fbinding _fbindings0)

and bound_free_accu_fbindings = fun (var_bvars, var_ofvars) -> function
  (_fbindings0) ->
      let (var_bvars, var_ofvars) = List.fold_left bound_free_accu_fbinding (var_bvars, var_ofvars) _fbindings0 in
      (var_bvars, var_ofvars)

and aeq_fbindings : unit -> fbindings -> fbindings -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_fbindings0), (_fbindings1) ->
      List.fold_left2 aeq_fbinding () _fbindings0 _fbindings1;
      ()

and freshen2_fbindings : Var.Subst.t * Var.Subst.t -> fbindings -> fbindings -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_fbindings0), (_fbindings1) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_fbinding (var_env1, var_env2) _fbindings0 _fbindings1 in
      (var_env1, var_env2)

and subst_fbinding : Var.Subst.t * Var.Subst.t -> fbinding -> fbinding = fun (var_oenv, var_ienv) -> function
  (_var1, _formula_type0) ->
    (Var.Subst.lookup _var1 var_ienv, (subst_formula_type (var_oenv)) _formula_type0)

and bound_fbinding : fbinding -> Var.AtomSet.t = 
  function fbinding -> bound_accu_fbinding (Var.AtomSet.empty) fbinding

and bound_free_fbinding : fbinding -> Var.AtomSet.t * Var.AtomSet.t = 
  function fbinding -> bound_free_accu_fbinding (Var.AtomSet.empty, Var.AtomSet.empty) fbinding

and equal_fbinding : fbinding -> fbinding -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_fbinding x1 x2

and import_fbinding = fun (var_oenv, var_ienv) -> function
  (_var1, _formula_type0) ->
    (Var.find _var1 var_ienv, (import_formula_type (var_oenv)) _formula_type0)

and bvi_accu_fbinding = fun (var_bvars) -> function
  (_var1, _formula_type0) ->
      let var_bvars = Identifier.Map.add _var1 () var_bvars in
      (var_bvars)

and bvi_fbinding = 
  function fbinding -> bvi_accu_fbinding (Identifier.Map.empty) fbinding

and bound_accu_fbinding = fun (var_bvars) -> function
  (_var1, _formula_type0) ->
      let var_bvars = Var.AtomSet.add _var1 var_bvars in
      (var_bvars)

and export_fbinding : Var.AtomIdMap.t * Var.AtomIdMap.t -> fbinding -> Raw.fbinding = fun (var_om, var_im) -> function
  (_var1, _formula_type0) ->
    (Var.AtomIdMap.lookup _var1 var_im, (export_formula_type (var_om)) _formula_type0)

and flatten_fbinding : fbinding -> Flat.fbinding = function
  (_var1, _formula_type0) ->
    (_var1, flatten_formula_type _formula_type0)

and unflatten_fbinding : Flat.fbinding -> fbinding = function
  (_var1, _formula_type0) ->
    (_var1, unflatten_formula_type _formula_type0)

and bound_free_accu_fbinding = fun (var_bvars, var_ofvars) -> function
  (_var1, _formula_type0) ->
      let var_bvars = Var.AtomSet.add _var1 var_bvars in
      let (var_ofvars) = free_accu_formula_type (var_ofvars) _formula_type0 in
      (var_bvars, var_ofvars)

and aeq_fbinding : unit -> fbinding -> fbinding -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var1, _formula_type0), (_var3, _formula_type2) ->
      if not (Var.Atom.equal _var1 _var3) then raise (Invalid_argument "aeq_fbinding");
      aeq_formula_type () _formula_type0 _formula_type2;
      ()

and freshen2_fbinding : Var.Subst.t * Var.Subst.t -> fbinding -> fbinding -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var1, _formula_type0), (_var3, _formula_type2) ->
      let var_env1, var_env2 = Var.Subst.freshen2 _var1 var_env1 _var3 var_env2 in
      (var_env1, var_env2)

and import_trigger : var Identifier.Map.t -> Raw.trigger -> trigger = fun (var_env) -> function
  (_var0) ->
    (Var.find _var0 var_env)

and subst_trigger : Var.Subst.t -> trigger -> trigger = fun (var_env) -> function
  (_var0) ->
    (Var.Subst.lookup _var0 var_env)

and export_trigger : Var.AtomIdMap.t -> trigger -> Raw.trigger = fun (var_m) -> function
  (_var0) ->
    (Var.AtomIdMap.lookup _var0 var_m)

and flatten_trigger : trigger -> Flat.trigger = function
  (_var0) ->
    (_var0)

and unflatten_trigger : Flat.trigger -> trigger = function
  (_var0) ->
    (_var0)

and free_trigger : trigger -> Var.AtomSet.t = 
  function trigger -> free_accu_trigger (Var.AtomSet.empty) trigger

and equal_trigger : trigger -> trigger -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_trigger x1 x2

and aeq_trigger : unit -> trigger -> trigger -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var0), (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_trigger");
      ()

and free_accu_trigger = fun (var_fvars) -> function
  (_var0) ->
      let var_fvars = Var.AtomSet.add _var0 var_fvars in
      (var_fvars)

and import_formula_type : var Identifier.Map.t -> Raw.formula_type -> formula_type = fun (var_env) -> function
  | Raw.FTProp ->
      FTProp
  | Raw.FTVar (_var0) ->
      FTVar (Var.find _var0 var_env)
  | Raw.FTArrow (_formula_type1, _formula_type0) ->
      FTArrow ((import_formula_type (var_env)) _formula_type1, (import_formula_type (var_env)) _formula_type0)
  | Raw.FTCArrow (_formula_type1, _formula_type0) ->
      FTCArrow ((import_formula_type (var_env)) _formula_type1, (import_formula_type (var_env)) _formula_type0)
  | Raw.FTProd (_formula_types0) ->
      FTProd (List.map (import_formula_type (var_env)) _formula_types0)
  | Raw.FTPrimitive (_primitive_type0) ->
      FTPrimitive ((import_primitive_type ()) _primitive_type0)
  | Raw.FTApp (_var1, _formula_types0) ->
      FTApp (Var.find _var1 var_env, List.map (import_formula_type (var_env)) _formula_types0)

and subst_formula_type : Var.Subst.t -> formula_type -> formula_type = fun (var_env) -> function
  | FTProp ->
      FTProp
  | FTVar (_var0) ->
      FTVar (Var.Subst.lookup _var0 var_env)
  | FTArrow (_formula_type1, _formula_type0) ->
      FTArrow ((subst_formula_type (var_env)) _formula_type1, (subst_formula_type (var_env)) _formula_type0)
  | FTCArrow (_formula_type1, _formula_type0) ->
      FTCArrow ((subst_formula_type (var_env)) _formula_type1, (subst_formula_type (var_env)) _formula_type0)
  | FTProd (_formula_types0) ->
      FTProd (List.map (subst_formula_type (var_env)) _formula_types0)
  | FTPrimitive (_primitive_type0) ->
      FTPrimitive ((subst_primitive_type ()) _primitive_type0)
  | FTApp (_var1, _formula_types0) ->
      FTApp (Var.Subst.lookup _var1 var_env, List.map (subst_formula_type (var_env)) _formula_types0)

and export_formula_type : Var.AtomIdMap.t -> formula_type -> Raw.formula_type = fun (var_m) -> function
  | FTProp ->
      Raw.FTProp
  | FTVar (_var0) ->
      Raw.FTVar (Var.AtomIdMap.lookup _var0 var_m)
  | FTArrow (_formula_type1, _formula_type0) ->
      Raw.FTArrow ((export_formula_type (var_m)) _formula_type1, (export_formula_type (var_m)) _formula_type0)
  | FTCArrow (_formula_type1, _formula_type0) ->
      Raw.FTCArrow ((export_formula_type (var_m)) _formula_type1, (export_formula_type (var_m)) _formula_type0)
  | FTProd (_formula_types0) ->
      Raw.FTProd (List.map (export_formula_type (var_m)) _formula_types0)
  | FTPrimitive (_primitive_type0) ->
      Raw.FTPrimitive ((export_primitive_type ()) _primitive_type0)
  | FTApp (_var1, _formula_types0) ->
      Raw.FTApp (Var.AtomIdMap.lookup _var1 var_m, List.map (export_formula_type (var_m)) _formula_types0)

and flatten_formula_type : formula_type -> Flat.formula_type = function
  | FTProp ->
      Flat.FTProp
  | FTVar (_var0) ->
      Flat.FTVar (_var0)
  | FTArrow (_formula_type1, _formula_type0) ->
      Flat.FTArrow (flatten_formula_type _formula_type1, flatten_formula_type _formula_type0)
  | FTCArrow (_formula_type1, _formula_type0) ->
      Flat.FTCArrow (flatten_formula_type _formula_type1, flatten_formula_type _formula_type0)
  | FTProd (_formula_types0) ->
      Flat.FTProd (List.map flatten_formula_type _formula_types0)
  | FTPrimitive (_primitive_type0) ->
      Flat.FTPrimitive (flatten_primitive_type _primitive_type0)
  | FTApp (_var1, _formula_types0) ->
      Flat.FTApp (_var1, List.map flatten_formula_type _formula_types0)

and unflatten_formula_type : Flat.formula_type -> formula_type = function
  | Flat.FTProp ->
      FTProp
  | Flat.FTVar (_var0) ->
      FTVar (_var0)
  | Flat.FTArrow (_formula_type1, _formula_type0) ->
      FTArrow (unflatten_formula_type _formula_type1, unflatten_formula_type _formula_type0)
  | Flat.FTCArrow (_formula_type1, _formula_type0) ->
      FTCArrow (unflatten_formula_type _formula_type1, unflatten_formula_type _formula_type0)
  | Flat.FTProd (_formula_types0) ->
      FTProd (List.map unflatten_formula_type _formula_types0)
  | Flat.FTPrimitive (_primitive_type0) ->
      FTPrimitive (unflatten_primitive_type _primitive_type0)
  | Flat.FTApp (_var1, _formula_types0) ->
      FTApp (_var1, List.map unflatten_formula_type _formula_types0)

and free_formula_type : formula_type -> Var.AtomSet.t = 
  function formula_type -> free_accu_formula_type (Var.AtomSet.empty) formula_type

and equal_formula_type : formula_type -> formula_type -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_formula_type x1 x2

and aeq_formula_type : unit -> formula_type -> formula_type -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | FTProp, FTProp ->
      ()
  | FTVar (_var0), FTVar (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_formula_type");
      ()
  | FTArrow (_formula_type1, _formula_type0), FTArrow (_formula_type3, _formula_type2) ->
      aeq_formula_type () _formula_type1 _formula_type3;
      aeq_formula_type () _formula_type0 _formula_type2;
      ()
  | FTCArrow (_formula_type1, _formula_type0), FTCArrow (_formula_type3, _formula_type2) ->
      aeq_formula_type () _formula_type1 _formula_type3;
      aeq_formula_type () _formula_type0 _formula_type2;
      ()
  | FTProd (_formula_types0), FTProd (_formula_types1) ->
      List.fold_left2 aeq_formula_type () _formula_types0 _formula_types1;
      ()
  | FTPrimitive (_primitive_type0), FTPrimitive (_primitive_type1) ->
      aeq_primitive_type () _primitive_type0 _primitive_type1;
      ()
  | FTApp (_var1, _formula_types0), FTApp (_var3, _formula_types2) ->
      if not (Var.Atom.equal _var1 _var3) then raise (Invalid_argument "aeq_formula_type");
      List.fold_left2 aeq_formula_type () _formula_types0 _formula_types2;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_formula_type")

and free_accu_formula_type = fun (var_fvars) -> function
  | FTProp ->
      (var_fvars)
  | FTVar (_var0) ->
      let var_fvars = Var.AtomSet.add _var0 var_fvars in
      (var_fvars)
  | FTArrow (_formula_type1, _formula_type0) ->
      let (var_fvars) = free_accu_formula_type (var_fvars) _formula_type1 in 
      let (var_fvars) = free_accu_formula_type (var_fvars) _formula_type0 in 
      (var_fvars)
  | FTCArrow (_formula_type1, _formula_type0) ->
      let (var_fvars) = free_accu_formula_type (var_fvars) _formula_type1 in 
      let (var_fvars) = free_accu_formula_type (var_fvars) _formula_type0 in 
      (var_fvars)
  | FTProd (_formula_types0) ->
      let (var_fvars) = List.fold_left free_accu_formula_type (var_fvars) _formula_types0 in 
      (var_fvars)
  | FTPrimitive (_primitive_type0) ->
      let () = free_accu_primitive_type () _primitive_type0 in 
      (var_fvars)
  | FTApp (_var1, _formula_types0) ->
      let var_fvars = Var.AtomSet.add _var1 var_fvars in
      let (var_fvars) = List.fold_left free_accu_formula_type (var_fvars) _formula_types0 in 
      (var_fvars)

and import_formula_type_scheme : var Identifier.Map.t -> Raw.formula_type_scheme -> formula_type_scheme = fun (var_env) -> function
  | Raw.FTScheme (_formula_scheme_abs0) ->
      let (var_bvars) = bvi_formula_scheme_abs _formula_scheme_abs0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _formula_scheme_abs0 = import_formula_scheme_abs (var_ienv) _formula_scheme_abs0 in
      FTScheme (create_formula_scheme_abs _formula_scheme_abs0)

and subst_formula_type_scheme : Var.Subst.t -> formula_type_scheme -> formula_type_scheme = fun (var_env) -> function
  | FTScheme (_formula_scheme_abs0) ->
      FTScheme (apply_formula_scheme_abs (var_env) _formula_scheme_abs0)

and export_formula_type_scheme : Var.AtomIdMap.t -> formula_type_scheme -> Raw.formula_type_scheme = fun (var_m) -> function
  | FTScheme (_formula_scheme_abs0) ->
      let formula_scheme_abs = open_formula_scheme_abs _formula_scheme_abs0 in
      let (var_bvars) = bound_formula_scheme_abs formula_scheme_abs in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.FTScheme (export_formula_scheme_abs (var_im) formula_scheme_abs)

and flatten_formula_type_scheme : formula_type_scheme -> Flat.formula_type_scheme = function
  | FTScheme (_formula_scheme_abs0) ->
      let formula_scheme_abs = open_formula_scheme_abs _formula_scheme_abs0 in
      Flat.FTScheme (flatten_formula_scheme_abs formula_scheme_abs)

and unflatten_formula_type_scheme : Flat.formula_type_scheme -> formula_type_scheme = function
  | Flat.FTScheme (_formula_scheme_abs0) ->
      let formula_scheme_abs = unflatten_formula_scheme_abs _formula_scheme_abs0 in
      FTScheme (create_formula_scheme_abs formula_scheme_abs)

and free_formula_type_scheme : formula_type_scheme -> Var.AtomSet.t = 
  function formula_type_scheme -> free_accu_formula_type_scheme (Var.AtomSet.empty) formula_type_scheme

and equal_formula_type_scheme : formula_type_scheme -> formula_type_scheme -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_formula_type_scheme x1 x2

and aeq_formula_type_scheme : unit -> formula_type_scheme -> formula_type_scheme -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | FTScheme (_formula_scheme_abs0), FTScheme (_formula_scheme_abs1) ->
      let _formula_scheme_abs0, _formula_scheme_abs1 = open2i_formula_scheme_abs _formula_scheme_abs0 _formula_scheme_abs1 in
      aeq_formula_scheme_abs () _formula_scheme_abs0 _formula_scheme_abs1;
      ()

and free_accu_formula_type_scheme = fun (var_fvars) -> function
  | FTScheme (_formula_scheme_abs0) ->
      let formula_scheme_abs = open_formula_scheme_abs _formula_scheme_abs0 in
      let (var_bvars, var_ifvars) = bound_free_accu_formula_scheme_abs (Var.AtomSet.empty, Var.AtomSet.empty) formula_scheme_abs in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)

and import_predicate_definition : var Identifier.Map.t -> Raw.predicate_definition -> predicate_definition = fun (var_env) -> function
  | Raw.PDAbbrev (_lformula0) ->
      PDAbbrev ((import_lformula (var_env)) _lformula0)
  | Raw.PDInductive (_formula_type_schemes1, _lformulas0) ->
      PDInductive (option_map (import_formula_type_scheme (var_env)) _formula_type_schemes1, List.map (import_lformula (var_env)) _lformulas0)

and subst_predicate_definition : Var.Subst.t -> predicate_definition -> predicate_definition = fun (var_env) -> function
  | PDAbbrev (_lformula0) ->
      PDAbbrev ((subst_lformula (var_env)) _lformula0)
  | PDInductive (_formula_type_schemes1, _lformulas0) ->
      PDInductive (option_map (subst_formula_type_scheme (var_env)) _formula_type_schemes1, List.map (subst_lformula (var_env)) _lformulas0)

and export_predicate_definition : Var.AtomIdMap.t -> predicate_definition -> Raw.predicate_definition = fun (var_m) -> function
  | PDAbbrev (_lformula0) ->
      Raw.PDAbbrev ((export_lformula (var_m)) _lformula0)
  | PDInductive (_formula_type_schemes1, _lformulas0) ->
      Raw.PDInductive (option_map (export_formula_type_scheme (var_m)) _formula_type_schemes1, List.map (export_lformula (var_m)) _lformulas0)

and flatten_predicate_definition : predicate_definition -> Flat.predicate_definition = function
  | PDAbbrev (_lformula0) ->
      Flat.PDAbbrev (flatten_lformula _lformula0)
  | PDInductive (_formula_type_schemes1, _lformulas0) ->
      Flat.PDInductive (option_map flatten_formula_type_scheme _formula_type_schemes1, List.map flatten_lformula _lformulas0)

and unflatten_predicate_definition : Flat.predicate_definition -> predicate_definition = function
  | Flat.PDAbbrev (_lformula0) ->
      PDAbbrev (unflatten_lformula _lformula0)
  | Flat.PDInductive (_formula_type_schemes1, _lformulas0) ->
      PDInductive (option_map unflatten_formula_type_scheme _formula_type_schemes1, List.map unflatten_lformula _lformulas0)

and free_predicate_definition : predicate_definition -> Var.AtomSet.t = 
  function predicate_definition -> free_accu_predicate_definition (Var.AtomSet.empty) predicate_definition

and equal_predicate_definition : predicate_definition -> predicate_definition -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_predicate_definition x1 x2

and aeq_predicate_definition : unit -> predicate_definition -> predicate_definition -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | PDAbbrev (_lformula0), PDAbbrev (_lformula1) ->
      aeq_lformula () _lformula0 _lformula1;
      ()
  | PDInductive (_formula_type_schemes1, _lformulas0), PDInductive (_formula_type_schemes3, _lformulas2) ->
      option_fold2 aeq_formula_type_scheme () _formula_type_schemes1 _formula_type_schemes3;
      List.fold_left2 aeq_lformula () _lformulas0 _lformulas2;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_predicate_definition")

and free_accu_predicate_definition = fun (var_fvars) -> function
  | PDAbbrev (_lformula0) ->
      let (var_fvars) = free_accu_lformula (var_fvars) _lformula0 in 
      (var_fvars)
  | PDInductive (_formula_type_schemes1, _lformulas0) ->
      let (var_fvars) = option_fold free_accu_formula_type_scheme (var_fvars) _formula_type_schemes1 in 
      let (var_fvars) = List.fold_left free_accu_lformula (var_fvars) _lformulas0 in 
      (var_fvars)

and import_lterm : var Identifier.Map.t -> Raw.lterm -> lterm = fun (var_env) -> function
  { Raw.tpos = _x1; Raw.tvalue = _term0 } ->
    { tpos = _x1; tvalue = (import_term (var_env)) _term0 }

and subst_lterm : Var.Subst.t -> lterm -> lterm = fun (var_env) -> function
  { tpos = _x1; tvalue = _term0 } ->
    { tpos = _x1; tvalue = (subst_term (var_env)) _term0 }

and export_lterm : Var.AtomIdMap.t -> lterm -> Raw.lterm = fun (var_m) -> function
  { tpos = _x1; tvalue = _term0 } ->
    { Raw.tpos = _x1; Raw.tvalue = (export_term (var_m)) _term0 }

and flatten_lterm : lterm -> Flat.lterm = function
  { tpos = _x1; tvalue = _term0 } ->
    { Flat.tpos = _x1; Flat.tvalue = flatten_term _term0 }

and unflatten_lterm : Flat.lterm -> lterm = function
  { Flat.tpos = _x1; Flat.tvalue = _term0 } ->
    { tpos = _x1; tvalue = unflatten_term _term0 }

and free_lterm : lterm -> Var.AtomSet.t = 
  function lterm -> free_accu_lterm (Var.AtomSet.empty) lterm

and equal_lterm : lterm -> lterm -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_lterm x1 x2

and aeq_lterm : unit -> lterm -> lterm -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | { tpos = _x1; tvalue = _term0 }, { tpos = _x3; tvalue = _term2 } ->
      aeq_term () _term0 _term2;
      ()

and free_accu_lterm = fun (var_fvars) -> function
  { tpos = _x1; tvalue = _term0 } ->
      let (var_fvars) = free_accu_term (var_fvars) _term0 in 
      (var_fvars)

and import_lformula : var Identifier.Map.t -> Raw.lformula -> lformula = fun (var_env) -> function
  { Raw.fpos = _x1; Raw.fvalue = _formula0 } ->
    { fpos = _x1; fvalue = (import_formula (var_env)) _formula0 }

and subst_lformula : Var.Subst.t -> lformula -> lformula = fun (var_env) -> function
  { fpos = _x1; fvalue = _formula0 } ->
    { fpos = _x1; fvalue = (subst_formula (var_env)) _formula0 }

and export_lformula : Var.AtomIdMap.t -> lformula -> Raw.lformula = fun (var_m) -> function
  { fpos = _x1; fvalue = _formula0 } ->
    { Raw.fpos = _x1; Raw.fvalue = (export_formula (var_m)) _formula0 }

and flatten_lformula : lformula -> Flat.lformula = function
  { fpos = _x1; fvalue = _formula0 } ->
    { Flat.fpos = _x1; Flat.fvalue = flatten_formula _formula0 }

and unflatten_lformula : Flat.lformula -> lformula = function
  { Flat.fpos = _x1; Flat.fvalue = _formula0 } ->
    { fpos = _x1; fvalue = unflatten_formula _formula0 }

and free_lformula : lformula -> Var.AtomSet.t = 
  function lformula -> free_accu_lformula (Var.AtomSet.empty) lformula

and equal_lformula : lformula -> lformula -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_lformula x1 x2

and aeq_lformula : unit -> lformula -> lformula -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | { fpos = _x1; fvalue = _formula0 }, { fpos = _x3; fvalue = _formula2 } ->
      aeq_formula () _formula0 _formula2;
      ()

and free_accu_lformula = fun (var_fvars) -> function
  { fpos = _x1; fvalue = _formula0 } ->
      let (var_fvars) = free_accu_formula (var_fvars) _formula0 in 
      (var_fvars)

and subst_pcons_abs : Var.Subst.t * Var.Subst.t -> pcons_abs -> pcons_abs = fun (var_oenv, var_ienv) -> function
  (_x2, _component1, _program0) ->
    (_x2, (subst_component (var_oenv, var_ienv)) _component1, (subst_program (var_ienv)) _program0)

and bound_pcons_abs : pcons_abs -> Var.AtomSet.t = 
  function pcons_abs -> bound_accu_pcons_abs (Var.AtomSet.empty) pcons_abs

and bound_free_pcons_abs : pcons_abs -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t = 
  function pcons_abs -> bound_free_accu_pcons_abs (Var.AtomSet.empty, Var.AtomSet.empty, Var.AtomSet.empty) pcons_abs

and equal_pcons_abs : pcons_abs -> pcons_abs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_pcons_abs x1 x2

and import_pcons_abs = fun (var_oenv, var_ienv) -> function
  (_x2, _component1, _program0) ->
    (_x2, (import_component (var_oenv, var_ienv)) _component1, (import_program (var_ienv)) _program0)

and bvi_accu_pcons_abs = fun (var_bvars) -> function
  (_x2, _component1, _program0) ->
      let (var_bvars) = bvi_accu_component (var_bvars) _component1 in
      (var_bvars)

and bvi_pcons_abs = 
  function pcons_abs -> bvi_accu_pcons_abs (Identifier.Map.empty) pcons_abs

and bound_accu_pcons_abs = fun (var_bvars) -> function
  (_x2, _component1, _program0) ->
      let (var_bvars) = bound_accu_component (var_bvars) _component1 in
      (var_bvars)

and export_pcons_abs : Var.AtomIdMap.t * Var.AtomIdMap.t -> pcons_abs -> Raw.pcons_abs = fun (var_om, var_im) -> function
  (_x2, _component1, _program0) ->
    (_x2, (export_component (var_om, var_im)) _component1, (export_program (var_im)) _program0)

and flatten_pcons_abs : pcons_abs -> Flat.pcons_abs = function
  (_x2, _component1, _program0) ->
    (_x2, flatten_component _component1, flatten_program _program0)

and unflatten_pcons_abs : Flat.pcons_abs -> pcons_abs = function
  (_x2, _component1, _program0) ->
    (_x2, unflatten_component _component1, unflatten_program _program0)

and bound_free_accu_pcons_abs = fun (var_bvars, var_ifvars, var_ofvars) -> function
  (_x2, _component1, _program0) ->
      let (var_bvars, var_ifvars, var_ofvars) = bound_free_accu_component (var_bvars, var_ifvars, var_ofvars) _component1 in
      let (var_ifvars) = free_accu_program (var_ifvars) _program0 in
      (var_bvars, var_ifvars, var_ofvars)

and aeq_pcons_abs : unit -> pcons_abs -> pcons_abs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_x2, _component1, _program0), (_x5, _component4, _program3) ->
      aeq_component () _component1 _component4;
      aeq_program () _program0 _program3;
      ()

and freshen2_pcons_abs : Var.Subst.t * Var.Subst.t -> pcons_abs -> pcons_abs -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_x2, _component1, _program0), (_x5, _component4, _program3) ->
      let (var_env1, var_env2) = freshen2_component (var_env1, var_env2) _component1 _component4 in
      (var_env1, var_env2)

and create_pcons_abs : pcons_abs -> opaque_pcons_abs = 
  function body -> {
    pcons_abs_delayed = (Var.Subst.id);
    pcons_abs = body
  }

and open_pcons_abs : opaque_pcons_abs -> pcons_abs = function abstraction ->
  let (var_delayed) = abstraction.pcons_abs_delayed in
  let body = abstraction.pcons_abs in
  let (var_bvars) = bound_pcons_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_pcons_abs (var_delayed, var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.pcons_abs_delayed <- (Var.Subst.id);
    abstraction.pcons_abs <- body
  end;
  body

and open2_pcons_abs : opaque_pcons_abs -> opaque_pcons_abs -> pcons_abs * pcons_abs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_pcons_abs x1 x2

and open2i_pcons_abs : opaque_pcons_abs -> opaque_pcons_abs -> pcons_abs * pcons_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.pcons_abs_delayed in
  let body1 = abstraction1.pcons_abs in
  let (var_delayed2) = abstraction2.pcons_abs_delayed in
  let body2 = abstraction2.pcons_abs in
  let (var_env1, var_env2) = freshen2_pcons_abs (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_pcons_abs (var_delayed1, var_env1) body1 in
  let body2 = subst_pcons_abs (var_delayed2, var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.pcons_abs_delayed <- (Var.Subst.id);
    abstraction1.pcons_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.pcons_abs_delayed <- (Var.Subst.id);
    abstraction2.pcons_abs <- body2
  end;
  body1, body2

and apply_pcons_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.pcons_abs_delayed in {
      abstraction with pcons_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_lfi_abs : Var.Subst.t -> lfi_abs -> lfi_abs = fun (var_ienv) -> function
  (_ids1, _logic_clauses0) ->
    ((subst_ids (var_ienv)) _ids1, List.map (subst_logic_clause (var_ienv)) _logic_clauses0)

and bound_lfi_abs : lfi_abs -> Var.AtomSet.t = 
  function lfi_abs -> bound_accu_lfi_abs (Var.AtomSet.empty) lfi_abs

and bound_free_lfi_abs : lfi_abs -> Var.AtomSet.t * Var.AtomSet.t = 
  function lfi_abs -> bound_free_accu_lfi_abs (Var.AtomSet.empty, Var.AtomSet.empty) lfi_abs

and equal_lfi_abs : lfi_abs -> lfi_abs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_lfi_abs x1 x2

and import_lfi_abs = fun (var_ienv) -> function
  (_ids1, _logic_clauses0) ->
    ((import_ids (var_ienv)) _ids1, List.map (import_logic_clause (var_ienv)) _logic_clauses0)

and bvi_accu_lfi_abs = fun (var_bvars) -> function
  (_ids1, _logic_clauses0) ->
      let (var_bvars) = bvi_accu_ids (var_bvars) _ids1 in
      (var_bvars)

and bvi_lfi_abs = 
  function lfi_abs -> bvi_accu_lfi_abs (Identifier.Map.empty) lfi_abs

and bound_accu_lfi_abs = fun (var_bvars) -> function
  (_ids1, _logic_clauses0) ->
      let (var_bvars) = bound_accu_ids (var_bvars) _ids1 in
      (var_bvars)

and export_lfi_abs : Var.AtomIdMap.t -> lfi_abs -> Raw.lfi_abs = fun (var_im) -> function
  (_ids1, _logic_clauses0) ->
    ((export_ids (var_im)) _ids1, List.map (export_logic_clause (var_im)) _logic_clauses0)

and flatten_lfi_abs : lfi_abs -> Flat.lfi_abs = function
  (_ids1, _logic_clauses0) ->
    (flatten_ids _ids1, List.map flatten_logic_clause _logic_clauses0)

and unflatten_lfi_abs : Flat.lfi_abs -> lfi_abs = function
  (_ids1, _logic_clauses0) ->
    (unflatten_ids _ids1, List.map unflatten_logic_clause _logic_clauses0)

and bound_free_accu_lfi_abs = fun (var_bvars, var_ifvars) -> function
  (_ids1, _logic_clauses0) ->
      let (var_bvars) = bound_free_accu_ids (var_bvars) _ids1 in
      let (var_ifvars) = List.fold_left free_accu_logic_clause (var_ifvars) _logic_clauses0 in
      (var_bvars, var_ifvars)

and aeq_lfi_abs : unit -> lfi_abs -> lfi_abs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_ids1, _logic_clauses0), (_ids3, _logic_clauses2) ->
      aeq_ids () _ids1 _ids3;
      List.fold_left2 aeq_logic_clause () _logic_clauses0 _logic_clauses2;
      ()

and freshen2_lfi_abs : Var.Subst.t * Var.Subst.t -> lfi_abs -> lfi_abs -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_ids1, _logic_clauses0), (_ids3, _logic_clauses2) ->
      let (var_env1, var_env2) = freshen2_ids (var_env1, var_env2) _ids1 _ids3 in
      (var_env1, var_env2)

and create_lfi_abs : lfi_abs -> opaque_lfi_abs = 
  function body -> {
    lfi_abs_delayed = (Var.Subst.id);
    lfi_abs = body
  }

and open_lfi_abs : opaque_lfi_abs -> lfi_abs = function abstraction ->
  let (var_delayed) = abstraction.lfi_abs_delayed in
  let body = abstraction.lfi_abs in
  let (var_bvars) = bound_lfi_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_lfi_abs (var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.lfi_abs_delayed <- (Var.Subst.id);
    abstraction.lfi_abs <- body
  end;
  body

and open2_lfi_abs : opaque_lfi_abs -> opaque_lfi_abs -> lfi_abs * lfi_abs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_lfi_abs x1 x2

and open2i_lfi_abs : opaque_lfi_abs -> opaque_lfi_abs -> lfi_abs * lfi_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.lfi_abs_delayed in
  let body1 = abstraction1.lfi_abs in
  let (var_delayed2) = abstraction2.lfi_abs_delayed in
  let body2 = abstraction2.lfi_abs in
  let (var_env1, var_env2) = freshen2_lfi_abs (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_lfi_abs (var_env1) body1 in
  let body2 = subst_lfi_abs (var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.lfi_abs_delayed <- (Var.Subst.id);
    abstraction1.lfi_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.lfi_abs_delayed <- (Var.Subst.id);
    abstraction2.lfi_abs <- body2
  end;
  body1, body2

and apply_lfi_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.lfi_abs_delayed in {
      abstraction with lfi_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_logic_clause_abs : Var.Subst.t * Var.Subst.t -> logic_clause_abs -> logic_clause_abs = fun (var_oenv, var_ienv) -> function
  (_pattern1, _lformula0) ->
    ((subst_pattern (var_oenv, var_ienv)) _pattern1, (subst_lformula (var_ienv)) _lformula0)

and bound_logic_clause_abs : logic_clause_abs -> Var.AtomSet.t = 
  function logic_clause_abs -> bound_accu_logic_clause_abs (Var.AtomSet.empty) logic_clause_abs

and bound_free_logic_clause_abs : logic_clause_abs -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t = 
  function logic_clause_abs -> bound_free_accu_logic_clause_abs (Var.AtomSet.empty, Var.AtomSet.empty, Var.AtomSet.empty) logic_clause_abs

and equal_logic_clause_abs : logic_clause_abs -> logic_clause_abs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_logic_clause_abs x1 x2

and import_logic_clause_abs = fun (var_oenv, var_ienv) -> function
  (_pattern1, _lformula0) ->
    ((import_pattern (var_oenv, var_ienv)) _pattern1, (import_lformula (var_ienv)) _lformula0)

and bvi_accu_logic_clause_abs = fun (var_bvars) -> function
  (_pattern1, _lformula0) ->
      let (var_bvars) = bvi_accu_pattern (var_bvars) _pattern1 in
      (var_bvars)

and bvi_logic_clause_abs = 
  function logic_clause_abs -> bvi_accu_logic_clause_abs (Identifier.Map.empty) logic_clause_abs

and bound_accu_logic_clause_abs = fun (var_bvars) -> function
  (_pattern1, _lformula0) ->
      let (var_bvars) = bound_accu_pattern (var_bvars) _pattern1 in
      (var_bvars)

and export_logic_clause_abs : Var.AtomIdMap.t * Var.AtomIdMap.t -> logic_clause_abs -> Raw.logic_clause_abs = fun (var_om, var_im) -> function
  (_pattern1, _lformula0) ->
    ((export_pattern (var_om, var_im)) _pattern1, (export_lformula (var_im)) _lformula0)

and flatten_logic_clause_abs : logic_clause_abs -> Flat.logic_clause_abs = function
  (_pattern1, _lformula0) ->
    (flatten_pattern _pattern1, flatten_lformula _lformula0)

and unflatten_logic_clause_abs : Flat.logic_clause_abs -> logic_clause_abs = function
  (_pattern1, _lformula0) ->
    (unflatten_pattern _pattern1, unflatten_lformula _lformula0)

and bound_free_accu_logic_clause_abs = fun (var_bvars, var_ifvars, var_ofvars) -> function
  (_pattern1, _lformula0) ->
      let (var_bvars, var_ofvars) = bound_free_accu_pattern (var_bvars, var_ofvars) _pattern1 in
      let (var_ifvars) = free_accu_lformula (var_ifvars) _lformula0 in
      (var_bvars, var_ifvars, var_ofvars)

and aeq_logic_clause_abs : unit -> logic_clause_abs -> logic_clause_abs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_pattern1, _lformula0), (_pattern3, _lformula2) ->
      aeq_pattern () _pattern1 _pattern3;
      aeq_lformula () _lformula0 _lformula2;
      ()

and freshen2_logic_clause_abs : Var.Subst.t * Var.Subst.t -> logic_clause_abs -> logic_clause_abs -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_pattern1, _lformula0), (_pattern3, _lformula2) ->
      let (var_env1, var_env2) = freshen2_pattern (var_env1, var_env2) _pattern1 _pattern3 in
      (var_env1, var_env2)

and create_logic_clause_abs : logic_clause_abs -> opaque_logic_clause_abs = 
  function body -> {
    logic_clause_abs_delayed = (Var.Subst.id);
    logic_clause_abs = body
  }

and open_logic_clause_abs : opaque_logic_clause_abs -> logic_clause_abs = function abstraction ->
  let (var_delayed) = abstraction.logic_clause_abs_delayed in
  let body = abstraction.logic_clause_abs in
  let (var_bvars) = bound_logic_clause_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_logic_clause_abs (var_delayed, var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.logic_clause_abs_delayed <- (Var.Subst.id);
    abstraction.logic_clause_abs <- body
  end;
  body

and open2_logic_clause_abs : opaque_logic_clause_abs -> opaque_logic_clause_abs -> logic_clause_abs * logic_clause_abs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_logic_clause_abs x1 x2

and open2i_logic_clause_abs : opaque_logic_clause_abs -> opaque_logic_clause_abs -> logic_clause_abs * logic_clause_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.logic_clause_abs_delayed in
  let body1 = abstraction1.logic_clause_abs in
  let (var_delayed2) = abstraction2.logic_clause_abs_delayed in
  let body2 = abstraction2.logic_clause_abs in
  let (var_env1, var_env2) = freshen2_logic_clause_abs (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_logic_clause_abs (var_delayed1, var_env1) body1 in
  let body2 = subst_logic_clause_abs (var_delayed2, var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.logic_clause_abs_delayed <- (Var.Subst.id);
    abstraction1.logic_clause_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.logic_clause_abs_delayed <- (Var.Subst.id);
    abstraction2.logic_clause_abs <- body2
  end;
  body1, body2

and apply_logic_clause_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.logic_clause_abs_delayed in {
      abstraction with logic_clause_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_fun_abs : Var.Subst.t * Var.Subst.t -> fun_abs -> fun_abs = fun (var_oenv, var_ienv) -> function
  (_logic_arguments3, _logic_bindings2, _function_output1, _lterm0) ->
    ((subst_logic_arguments (var_oenv, var_ienv)) _logic_arguments3, (subst_logic_bindings (var_ienv)) _logic_bindings2, (subst_function_output (var_ienv)) _function_output1, (subst_lterm (var_ienv)) _lterm0)

and bound_fun_abs : fun_abs -> Var.AtomSet.t = 
  function fun_abs -> bound_accu_fun_abs (Var.AtomSet.empty) fun_abs

and bound_free_fun_abs : fun_abs -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t = 
  function fun_abs -> bound_free_accu_fun_abs (Var.AtomSet.empty, Var.AtomSet.empty, Var.AtomSet.empty) fun_abs

and equal_fun_abs : fun_abs -> fun_abs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_fun_abs x1 x2

and import_fun_abs = fun (var_oenv, var_ienv) -> function
  (_logic_arguments3, _logic_bindings2, _function_output1, _lterm0) ->
    ((import_logic_arguments (var_oenv, var_ienv)) _logic_arguments3, (import_logic_bindings (var_ienv)) _logic_bindings2, (import_function_output (var_ienv)) _function_output1, (import_lterm (var_ienv)) _lterm0)

and bvi_accu_fun_abs = fun (var_bvars) -> function
  (_logic_arguments3, _logic_bindings2, _function_output1, _lterm0) ->
      let (var_bvars) = bvi_accu_logic_arguments (var_bvars) _logic_arguments3 in
      let (var_bvars) = bvi_accu_logic_bindings (var_bvars) _logic_bindings2 in
      (var_bvars)

and bvi_fun_abs = 
  function fun_abs -> bvi_accu_fun_abs (Identifier.Map.empty) fun_abs

and bound_accu_fun_abs = fun (var_bvars) -> function
  (_logic_arguments3, _logic_bindings2, _function_output1, _lterm0) ->
      let (var_bvars) = bound_accu_logic_arguments (var_bvars) _logic_arguments3 in
      let (var_bvars) = bound_accu_logic_bindings (var_bvars) _logic_bindings2 in
      (var_bvars)

and export_fun_abs : Var.AtomIdMap.t * Var.AtomIdMap.t -> fun_abs -> Raw.fun_abs = fun (var_om, var_im) -> function
  (_logic_arguments3, _logic_bindings2, _function_output1, _lterm0) ->
    ((export_logic_arguments (var_om, var_im)) _logic_arguments3, (export_logic_bindings (var_im)) _logic_bindings2, (export_function_output (var_im)) _function_output1, (export_lterm (var_im)) _lterm0)

and flatten_fun_abs : fun_abs -> Flat.fun_abs = function
  (_logic_arguments3, _logic_bindings2, _function_output1, _lterm0) ->
    (flatten_logic_arguments _logic_arguments3, flatten_logic_bindings _logic_bindings2, flatten_function_output _function_output1, flatten_lterm _lterm0)

and unflatten_fun_abs : Flat.fun_abs -> fun_abs = function
  (_logic_arguments3, _logic_bindings2, _function_output1, _lterm0) ->
    (unflatten_logic_arguments _logic_arguments3, unflatten_logic_bindings _logic_bindings2, unflatten_function_output _function_output1, unflatten_lterm _lterm0)

and bound_free_accu_fun_abs = fun (var_bvars, var_ifvars, var_ofvars) -> function
  (_logic_arguments3, _logic_bindings2, _function_output1, _lterm0) ->
      let (var_bvars, var_ofvars) = bound_free_accu_logic_arguments (var_bvars, var_ofvars) _logic_arguments3 in
      let (var_bvars, var_ifvars) = bound_free_accu_logic_bindings (var_bvars, var_ifvars) _logic_bindings2 in
      let (var_ifvars) = free_accu_function_output (var_ifvars) _function_output1 in
      let (var_ifvars) = free_accu_lterm (var_ifvars) _lterm0 in
      (var_bvars, var_ifvars, var_ofvars)

and aeq_fun_abs : unit -> fun_abs -> fun_abs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_logic_arguments3, _logic_bindings2, _function_output1, _lterm0), (_logic_arguments7, _logic_bindings6, _function_output5, _lterm4) ->
      aeq_logic_arguments () _logic_arguments3 _logic_arguments7;
      aeq_logic_bindings () _logic_bindings2 _logic_bindings6;
      aeq_function_output () _function_output1 _function_output5;
      aeq_lterm () _lterm0 _lterm4;
      ()

and freshen2_fun_abs : Var.Subst.t * Var.Subst.t -> fun_abs -> fun_abs -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_logic_arguments3, _logic_bindings2, _function_output1, _lterm0), (_logic_arguments7, _logic_bindings6, _function_output5, _lterm4) ->
      let (var_env1, var_env2) = freshen2_logic_arguments (var_env1, var_env2) _logic_arguments3 _logic_arguments7 in
      let (var_env1, var_env2) = freshen2_logic_bindings (var_env1, var_env2) _logic_bindings2 _logic_bindings6 in
      (var_env1, var_env2)

and create_fun_abs : fun_abs -> opaque_fun_abs = 
  function body -> {
    fun_abs_delayed = (Var.Subst.id);
    fun_abs = body
  }

and open_fun_abs : opaque_fun_abs -> fun_abs = function abstraction ->
  let (var_delayed) = abstraction.fun_abs_delayed in
  let body = abstraction.fun_abs in
  let (var_bvars) = bound_fun_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_fun_abs (var_delayed, var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.fun_abs_delayed <- (Var.Subst.id);
    abstraction.fun_abs <- body
  end;
  body

and open2_fun_abs : opaque_fun_abs -> opaque_fun_abs -> fun_abs * fun_abs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_fun_abs x1 x2

and open2i_fun_abs : opaque_fun_abs -> opaque_fun_abs -> fun_abs * fun_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.fun_abs_delayed in
  let body1 = abstraction1.fun_abs in
  let (var_delayed2) = abstraction2.fun_abs_delayed in
  let body2 = abstraction2.fun_abs in
  let (var_env1, var_env2) = freshen2_fun_abs (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_fun_abs (var_delayed1, var_env1) body1 in
  let body2 = subst_fun_abs (var_delayed2, var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.fun_abs_delayed <- (Var.Subst.id);
    abstraction1.fun_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.fun_abs_delayed <- (Var.Subst.id);
    abstraction2.fun_abs <- body2
  end;
  body1, body2

and apply_fun_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.fun_abs_delayed in {
      abstraction with fun_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_let_abs : Var.Subst.t * Var.Subst.t -> let_abs -> let_abs = fun (var_oenv, var_ienv) -> function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
    ((subst_ids (var_ienv)) _ids3, (subst_optional_lformula (var_ienv)) _optional_lformula2, (subst_lterm (var_oenv)) _lterm1, (subst_lterm (var_ienv)) _lterm0)

and bound_let_abs : let_abs -> Var.AtomSet.t = 
  function let_abs -> bound_accu_let_abs (Var.AtomSet.empty) let_abs

and bound_free_let_abs : let_abs -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t = 
  function let_abs -> bound_free_accu_let_abs (Var.AtomSet.empty, Var.AtomSet.empty, Var.AtomSet.empty) let_abs

and equal_let_abs : let_abs -> let_abs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_let_abs x1 x2

and import_let_abs = fun (var_oenv, var_ienv) -> function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
    ((import_ids (var_ienv)) _ids3, (import_optional_lformula (var_ienv)) _optional_lformula2, (import_lterm (var_oenv)) _lterm1, (import_lterm (var_ienv)) _lterm0)

and bvi_accu_let_abs = fun (var_bvars) -> function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
      let (var_bvars) = bvi_accu_ids (var_bvars) _ids3 in
      (var_bvars)

and bvi_let_abs = 
  function let_abs -> bvi_accu_let_abs (Identifier.Map.empty) let_abs

and bound_accu_let_abs = fun (var_bvars) -> function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
      let (var_bvars) = bound_accu_ids (var_bvars) _ids3 in
      (var_bvars)

and export_let_abs : Var.AtomIdMap.t * Var.AtomIdMap.t -> let_abs -> Raw.let_abs = fun (var_om, var_im) -> function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
    ((export_ids (var_im)) _ids3, (export_optional_lformula (var_im)) _optional_lformula2, (export_lterm (var_om)) _lterm1, (export_lterm (var_im)) _lterm0)

and flatten_let_abs : let_abs -> Flat.let_abs = function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
    (flatten_ids _ids3, flatten_optional_lformula _optional_lformula2, flatten_lterm _lterm1, flatten_lterm _lterm0)

and unflatten_let_abs : Flat.let_abs -> let_abs = function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
    (unflatten_ids _ids3, unflatten_optional_lformula _optional_lformula2, unflatten_lterm _lterm1, unflatten_lterm _lterm0)

and bound_free_accu_let_abs = fun (var_bvars, var_ifvars, var_ofvars) -> function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
      let (var_bvars) = bound_free_accu_ids (var_bvars) _ids3 in
      let (var_ifvars) = free_accu_optional_lformula (var_ifvars) _optional_lformula2 in
      let (var_ofvars) = free_accu_lterm (var_ofvars) _lterm1 in
      let (var_ifvars) = free_accu_lterm (var_ifvars) _lterm0 in
      (var_bvars, var_ifvars, var_ofvars)

and aeq_let_abs : unit -> let_abs -> let_abs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_ids3, _optional_lformula2, _lterm1, _lterm0), (_ids7, _optional_lformula6, _lterm5, _lterm4) ->
      aeq_ids () _ids3 _ids7;
      aeq_optional_lformula () _optional_lformula2 _optional_lformula6;
      aeq_lterm () _lterm1 _lterm5;
      aeq_lterm () _lterm0 _lterm4;
      ()

and freshen2_let_abs : Var.Subst.t * Var.Subst.t -> let_abs -> let_abs -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_ids3, _optional_lformula2, _lterm1, _lterm0), (_ids7, _optional_lformula6, _lterm5, _lterm4) ->
      let (var_env1, var_env2) = freshen2_ids (var_env1, var_env2) _ids3 _ids7 in
      (var_env1, var_env2)

and create_let_abs : let_abs -> opaque_let_abs = 
  function body -> {
    let_abs_delayed = (Var.Subst.id);
    let_abs = body
  }

and open_let_abs : opaque_let_abs -> let_abs = function abstraction ->
  let (var_delayed) = abstraction.let_abs_delayed in
  let body = abstraction.let_abs in
  let (var_bvars) = bound_let_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_let_abs (var_delayed, var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.let_abs_delayed <- (Var.Subst.id);
    abstraction.let_abs <- body
  end;
  body

and open2_let_abs : opaque_let_abs -> opaque_let_abs -> let_abs * let_abs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_let_abs x1 x2

and open2i_let_abs : opaque_let_abs -> opaque_let_abs -> let_abs * let_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.let_abs_delayed in
  let body1 = abstraction1.let_abs in
  let (var_delayed2) = abstraction2.let_abs_delayed in
  let body2 = abstraction2.let_abs in
  let (var_env1, var_env2) = freshen2_let_abs (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_let_abs (var_delayed1, var_env1) body1 in
  let body2 = subst_let_abs (var_delayed2, var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.let_abs_delayed <- (Var.Subst.id);
    abstraction1.let_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.let_abs_delayed <- (Var.Subst.id);
    abstraction2.let_abs <- body2
  end;
  body1, body2

and apply_let_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.let_abs_delayed in {
      abstraction with let_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_letrec_abs : Var.Subst.t -> letrec_abs -> letrec_abs = fun (var_ienv) -> function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
    ((subst_ids (var_ienv)) _ids3, (subst_optional_lformula (var_ienv)) _optional_lformula2, (subst_lterm (var_ienv)) _lterm1, (subst_lterm (var_ienv)) _lterm0)

and bound_letrec_abs : letrec_abs -> Var.AtomSet.t = 
  function letrec_abs -> bound_accu_letrec_abs (Var.AtomSet.empty) letrec_abs

and bound_free_letrec_abs : letrec_abs -> Var.AtomSet.t * Var.AtomSet.t = 
  function letrec_abs -> bound_free_accu_letrec_abs (Var.AtomSet.empty, Var.AtomSet.empty) letrec_abs

and equal_letrec_abs : letrec_abs -> letrec_abs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_letrec_abs x1 x2

and import_letrec_abs = fun (var_ienv) -> function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
    ((import_ids (var_ienv)) _ids3, (import_optional_lformula (var_ienv)) _optional_lformula2, (import_lterm (var_ienv)) _lterm1, (import_lterm (var_ienv)) _lterm0)

and bvi_accu_letrec_abs = fun (var_bvars) -> function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
      let (var_bvars) = bvi_accu_ids (var_bvars) _ids3 in
      (var_bvars)

and bvi_letrec_abs = 
  function letrec_abs -> bvi_accu_letrec_abs (Identifier.Map.empty) letrec_abs

and bound_accu_letrec_abs = fun (var_bvars) -> function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
      let (var_bvars) = bound_accu_ids (var_bvars) _ids3 in
      (var_bvars)

and export_letrec_abs : Var.AtomIdMap.t -> letrec_abs -> Raw.letrec_abs = fun (var_im) -> function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
    ((export_ids (var_im)) _ids3, (export_optional_lformula (var_im)) _optional_lformula2, (export_lterm (var_im)) _lterm1, (export_lterm (var_im)) _lterm0)

and flatten_letrec_abs : letrec_abs -> Flat.letrec_abs = function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
    (flatten_ids _ids3, flatten_optional_lformula _optional_lformula2, flatten_lterm _lterm1, flatten_lterm _lterm0)

and unflatten_letrec_abs : Flat.letrec_abs -> letrec_abs = function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
    (unflatten_ids _ids3, unflatten_optional_lformula _optional_lformula2, unflatten_lterm _lterm1, unflatten_lterm _lterm0)

and bound_free_accu_letrec_abs = fun (var_bvars, var_ifvars) -> function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
      let (var_bvars) = bound_free_accu_ids (var_bvars) _ids3 in
      let (var_ifvars) = free_accu_optional_lformula (var_ifvars) _optional_lformula2 in
      let (var_ifvars) = free_accu_lterm (var_ifvars) _lterm1 in
      let (var_ifvars) = free_accu_lterm (var_ifvars) _lterm0 in
      (var_bvars, var_ifvars)

and aeq_letrec_abs : unit -> letrec_abs -> letrec_abs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_ids3, _optional_lformula2, _lterm1, _lterm0), (_ids7, _optional_lformula6, _lterm5, _lterm4) ->
      aeq_ids () _ids3 _ids7;
      aeq_optional_lformula () _optional_lformula2 _optional_lformula6;
      aeq_lterm () _lterm1 _lterm5;
      aeq_lterm () _lterm0 _lterm4;
      ()

and freshen2_letrec_abs : Var.Subst.t * Var.Subst.t -> letrec_abs -> letrec_abs -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_ids3, _optional_lformula2, _lterm1, _lterm0), (_ids7, _optional_lformula6, _lterm5, _lterm4) ->
      let (var_env1, var_env2) = freshen2_ids (var_env1, var_env2) _ids3 _ids7 in
      (var_env1, var_env2)

and create_letrec_abs : letrec_abs -> opaque_letrec_abs = 
  function body -> {
    letrec_abs_delayed = (Var.Subst.id);
    letrec_abs = body
  }

and open_letrec_abs : opaque_letrec_abs -> letrec_abs = function abstraction ->
  let (var_delayed) = abstraction.letrec_abs_delayed in
  let body = abstraction.letrec_abs in
  let (var_bvars) = bound_letrec_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_letrec_abs (var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.letrec_abs_delayed <- (Var.Subst.id);
    abstraction.letrec_abs <- body
  end;
  body

and open2_letrec_abs : opaque_letrec_abs -> opaque_letrec_abs -> letrec_abs * letrec_abs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_letrec_abs x1 x2

and open2i_letrec_abs : opaque_letrec_abs -> opaque_letrec_abs -> letrec_abs * letrec_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.letrec_abs_delayed in
  let body1 = abstraction1.letrec_abs in
  let (var_delayed2) = abstraction2.letrec_abs_delayed in
  let body2 = abstraction2.letrec_abs in
  let (var_env1, var_env2) = freshen2_letrec_abs (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_letrec_abs (var_env1) body1 in
  let body2 = subst_letrec_abs (var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.letrec_abs_delayed <- (Var.Subst.id);
    abstraction1.letrec_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.letrec_abs_delayed <- (Var.Subst.id);
    abstraction2.letrec_abs <- body2
  end;
  body1, body2

and apply_letrec_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.letrec_abs_delayed in {
      abstraction with letrec_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_eforalltys_abs : Var.Subst.t -> eforalltys_abs -> eforalltys_abs = fun (var_ienv) -> function
  (_type_parameters1, _lterm0) ->
    ((subst_type_parameters (var_ienv)) _type_parameters1, (subst_lterm (var_ienv)) _lterm0)

and bound_eforalltys_abs : eforalltys_abs -> Var.AtomSet.t = 
  function eforalltys_abs -> bound_accu_eforalltys_abs (Var.AtomSet.empty) eforalltys_abs

and bound_free_eforalltys_abs : eforalltys_abs -> Var.AtomSet.t * Var.AtomSet.t = 
  function eforalltys_abs -> bound_free_accu_eforalltys_abs (Var.AtomSet.empty, Var.AtomSet.empty) eforalltys_abs

and equal_eforalltys_abs : eforalltys_abs -> eforalltys_abs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_eforalltys_abs x1 x2

and import_eforalltys_abs = fun (var_ienv) -> function
  (_type_parameters1, _lterm0) ->
    ((import_type_parameters (var_ienv)) _type_parameters1, (import_lterm (var_ienv)) _lterm0)

and bvi_accu_eforalltys_abs = fun (var_bvars) -> function
  (_type_parameters1, _lterm0) ->
      let (var_bvars) = bvi_accu_type_parameters (var_bvars) _type_parameters1 in
      (var_bvars)

and bvi_eforalltys_abs = 
  function eforalltys_abs -> bvi_accu_eforalltys_abs (Identifier.Map.empty) eforalltys_abs

and bound_accu_eforalltys_abs = fun (var_bvars) -> function
  (_type_parameters1, _lterm0) ->
      let (var_bvars) = bound_accu_type_parameters (var_bvars) _type_parameters1 in
      (var_bvars)

and export_eforalltys_abs : Var.AtomIdMap.t -> eforalltys_abs -> Raw.eforalltys_abs = fun (var_im) -> function
  (_type_parameters1, _lterm0) ->
    ((export_type_parameters (var_im)) _type_parameters1, (export_lterm (var_im)) _lterm0)

and flatten_eforalltys_abs : eforalltys_abs -> Flat.eforalltys_abs = function
  (_type_parameters1, _lterm0) ->
    (flatten_type_parameters _type_parameters1, flatten_lterm _lterm0)

and unflatten_eforalltys_abs : Flat.eforalltys_abs -> eforalltys_abs = function
  (_type_parameters1, _lterm0) ->
    (unflatten_type_parameters _type_parameters1, unflatten_lterm _lterm0)

and bound_free_accu_eforalltys_abs = fun (var_bvars, var_ifvars) -> function
  (_type_parameters1, _lterm0) ->
      let (var_bvars) = bound_free_accu_type_parameters (var_bvars) _type_parameters1 in
      let (var_ifvars) = free_accu_lterm (var_ifvars) _lterm0 in
      (var_bvars, var_ifvars)

and aeq_eforalltys_abs : unit -> eforalltys_abs -> eforalltys_abs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_type_parameters1, _lterm0), (_type_parameters3, _lterm2) ->
      aeq_type_parameters () _type_parameters1 _type_parameters3;
      aeq_lterm () _lterm0 _lterm2;
      ()

and freshen2_eforalltys_abs : Var.Subst.t * Var.Subst.t -> eforalltys_abs -> eforalltys_abs -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_type_parameters1, _lterm0), (_type_parameters3, _lterm2) ->
      let (var_env1, var_env2) = freshen2_type_parameters (var_env1, var_env2) _type_parameters1 _type_parameters3 in
      (var_env1, var_env2)

and create_eforalltys_abs : eforalltys_abs -> opaque_eforalltys_abs = 
  function body -> {
    eforalltys_abs_delayed = (Var.Subst.id);
    eforalltys_abs = body
  }

and open_eforalltys_abs : opaque_eforalltys_abs -> eforalltys_abs = function abstraction ->
  let (var_delayed) = abstraction.eforalltys_abs_delayed in
  let body = abstraction.eforalltys_abs in
  let (var_bvars) = bound_eforalltys_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_eforalltys_abs (var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.eforalltys_abs_delayed <- (Var.Subst.id);
    abstraction.eforalltys_abs <- body
  end;
  body

and open2_eforalltys_abs : opaque_eforalltys_abs -> opaque_eforalltys_abs -> eforalltys_abs * eforalltys_abs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_eforalltys_abs x1 x2

and open2i_eforalltys_abs : opaque_eforalltys_abs -> opaque_eforalltys_abs -> eforalltys_abs * eforalltys_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.eforalltys_abs_delayed in
  let body1 = abstraction1.eforalltys_abs in
  let (var_delayed2) = abstraction2.eforalltys_abs_delayed in
  let body2 = abstraction2.eforalltys_abs in
  let (var_env1, var_env2) = freshen2_eforalltys_abs (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_eforalltys_abs (var_env1) body1 in
  let body2 = subst_eforalltys_abs (var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.eforalltys_abs_delayed <- (Var.Subst.id);
    abstraction1.eforalltys_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.eforalltys_abs_delayed <- (Var.Subst.id);
    abstraction2.eforalltys_abs <- body2
  end;
  body1, body2

and apply_eforalltys_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.eforalltys_abs_delayed in {
      abstraction with eforalltys_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_eexiststys_abs : Var.Subst.t -> eexiststys_abs -> eexiststys_abs = fun (var_ienv) -> function
  (_type_parameters1, _lterm0) ->
    ((subst_type_parameters (var_ienv)) _type_parameters1, (subst_lterm (var_ienv)) _lterm0)

and bound_eexiststys_abs : eexiststys_abs -> Var.AtomSet.t = 
  function eexiststys_abs -> bound_accu_eexiststys_abs (Var.AtomSet.empty) eexiststys_abs

and bound_free_eexiststys_abs : eexiststys_abs -> Var.AtomSet.t * Var.AtomSet.t = 
  function eexiststys_abs -> bound_free_accu_eexiststys_abs (Var.AtomSet.empty, Var.AtomSet.empty) eexiststys_abs

and equal_eexiststys_abs : eexiststys_abs -> eexiststys_abs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_eexiststys_abs x1 x2

and import_eexiststys_abs = fun (var_ienv) -> function
  (_type_parameters1, _lterm0) ->
    ((import_type_parameters (var_ienv)) _type_parameters1, (import_lterm (var_ienv)) _lterm0)

and bvi_accu_eexiststys_abs = fun (var_bvars) -> function
  (_type_parameters1, _lterm0) ->
      let (var_bvars) = bvi_accu_type_parameters (var_bvars) _type_parameters1 in
      (var_bvars)

and bvi_eexiststys_abs = 
  function eexiststys_abs -> bvi_accu_eexiststys_abs (Identifier.Map.empty) eexiststys_abs

and bound_accu_eexiststys_abs = fun (var_bvars) -> function
  (_type_parameters1, _lterm0) ->
      let (var_bvars) = bound_accu_type_parameters (var_bvars) _type_parameters1 in
      (var_bvars)

and export_eexiststys_abs : Var.AtomIdMap.t -> eexiststys_abs -> Raw.eexiststys_abs = fun (var_im) -> function
  (_type_parameters1, _lterm0) ->
    ((export_type_parameters (var_im)) _type_parameters1, (export_lterm (var_im)) _lterm0)

and flatten_eexiststys_abs : eexiststys_abs -> Flat.eexiststys_abs = function
  (_type_parameters1, _lterm0) ->
    (flatten_type_parameters _type_parameters1, flatten_lterm _lterm0)

and unflatten_eexiststys_abs : Flat.eexiststys_abs -> eexiststys_abs = function
  (_type_parameters1, _lterm0) ->
    (unflatten_type_parameters _type_parameters1, unflatten_lterm _lterm0)

and bound_free_accu_eexiststys_abs = fun (var_bvars, var_ifvars) -> function
  (_type_parameters1, _lterm0) ->
      let (var_bvars) = bound_free_accu_type_parameters (var_bvars) _type_parameters1 in
      let (var_ifvars) = free_accu_lterm (var_ifvars) _lterm0 in
      (var_bvars, var_ifvars)

and aeq_eexiststys_abs : unit -> eexiststys_abs -> eexiststys_abs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_type_parameters1, _lterm0), (_type_parameters3, _lterm2) ->
      aeq_type_parameters () _type_parameters1 _type_parameters3;
      aeq_lterm () _lterm0 _lterm2;
      ()

and freshen2_eexiststys_abs : Var.Subst.t * Var.Subst.t -> eexiststys_abs -> eexiststys_abs -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_type_parameters1, _lterm0), (_type_parameters3, _lterm2) ->
      let (var_env1, var_env2) = freshen2_type_parameters (var_env1, var_env2) _type_parameters1 _type_parameters3 in
      (var_env1, var_env2)

and create_eexiststys_abs : eexiststys_abs -> opaque_eexiststys_abs = 
  function body -> {
    eexiststys_abs_delayed = (Var.Subst.id);
    eexiststys_abs = body
  }

and open_eexiststys_abs : opaque_eexiststys_abs -> eexiststys_abs = function abstraction ->
  let (var_delayed) = abstraction.eexiststys_abs_delayed in
  let body = abstraction.eexiststys_abs in
  let (var_bvars) = bound_eexiststys_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_eexiststys_abs (var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.eexiststys_abs_delayed <- (Var.Subst.id);
    abstraction.eexiststys_abs <- body
  end;
  body

and open2_eexiststys_abs : opaque_eexiststys_abs -> opaque_eexiststys_abs -> eexiststys_abs * eexiststys_abs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_eexiststys_abs x1 x2

and open2i_eexiststys_abs : opaque_eexiststys_abs -> opaque_eexiststys_abs -> eexiststys_abs * eexiststys_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.eexiststys_abs_delayed in
  let body1 = abstraction1.eexiststys_abs in
  let (var_delayed2) = abstraction2.eexiststys_abs_delayed in
  let body2 = abstraction2.eexiststys_abs in
  let (var_env1, var_env2) = freshen2_eexiststys_abs (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_eexiststys_abs (var_env1) body1 in
  let body2 = subst_eexiststys_abs (var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.eexiststys_abs_delayed <- (Var.Subst.id);
    abstraction1.eexiststys_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.eexiststys_abs_delayed <- (Var.Subst.id);
    abstraction2.eexiststys_abs <- body2
  end;
  body1, body2

and apply_eexiststys_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.eexiststys_abs_delayed in {
      abstraction with eexiststys_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_let_logic_abs : Var.Subst.t * Var.Subst.t -> let_logic_abs -> let_logic_abs = fun (var_oenv, var_ienv) -> function
  (_fbindings2, _lformula1, _lterm0) ->
    ((subst_fbindings (var_oenv, var_ienv)) _fbindings2, (subst_lformula (var_ienv)) _lformula1, (subst_lterm (var_ienv)) _lterm0)

and bound_let_logic_abs : let_logic_abs -> Var.AtomSet.t = 
  function let_logic_abs -> bound_accu_let_logic_abs (Var.AtomSet.empty) let_logic_abs

and bound_free_let_logic_abs : let_logic_abs -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t = 
  function let_logic_abs -> bound_free_accu_let_logic_abs (Var.AtomSet.empty, Var.AtomSet.empty, Var.AtomSet.empty) let_logic_abs

and equal_let_logic_abs : let_logic_abs -> let_logic_abs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_let_logic_abs x1 x2

and import_let_logic_abs = fun (var_oenv, var_ienv) -> function
  (_fbindings2, _lformula1, _lterm0) ->
    ((import_fbindings (var_oenv, var_ienv)) _fbindings2, (import_lformula (var_ienv)) _lformula1, (import_lterm (var_ienv)) _lterm0)

and bvi_accu_let_logic_abs = fun (var_bvars) -> function
  (_fbindings2, _lformula1, _lterm0) ->
      let (var_bvars) = bvi_accu_fbindings (var_bvars) _fbindings2 in
      (var_bvars)

and bvi_let_logic_abs = 
  function let_logic_abs -> bvi_accu_let_logic_abs (Identifier.Map.empty) let_logic_abs

and bound_accu_let_logic_abs = fun (var_bvars) -> function
  (_fbindings2, _lformula1, _lterm0) ->
      let (var_bvars) = bound_accu_fbindings (var_bvars) _fbindings2 in
      (var_bvars)

and export_let_logic_abs : Var.AtomIdMap.t * Var.AtomIdMap.t -> let_logic_abs -> Raw.let_logic_abs = fun (var_om, var_im) -> function
  (_fbindings2, _lformula1, _lterm0) ->
    ((export_fbindings (var_om, var_im)) _fbindings2, (export_lformula (var_im)) _lformula1, (export_lterm (var_im)) _lterm0)

and flatten_let_logic_abs : let_logic_abs -> Flat.let_logic_abs = function
  (_fbindings2, _lformula1, _lterm0) ->
    (flatten_fbindings _fbindings2, flatten_lformula _lformula1, flatten_lterm _lterm0)

and unflatten_let_logic_abs : Flat.let_logic_abs -> let_logic_abs = function
  (_fbindings2, _lformula1, _lterm0) ->
    (unflatten_fbindings _fbindings2, unflatten_lformula _lformula1, unflatten_lterm _lterm0)

and bound_free_accu_let_logic_abs = fun (var_bvars, var_ifvars, var_ofvars) -> function
  (_fbindings2, _lformula1, _lterm0) ->
      let (var_bvars, var_ofvars) = bound_free_accu_fbindings (var_bvars, var_ofvars) _fbindings2 in
      let (var_ifvars) = free_accu_lformula (var_ifvars) _lformula1 in
      let (var_ifvars) = free_accu_lterm (var_ifvars) _lterm0 in
      (var_bvars, var_ifvars, var_ofvars)

and aeq_let_logic_abs : unit -> let_logic_abs -> let_logic_abs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_fbindings2, _lformula1, _lterm0), (_fbindings5, _lformula4, _lterm3) ->
      aeq_fbindings () _fbindings2 _fbindings5;
      aeq_lformula () _lformula1 _lformula4;
      aeq_lterm () _lterm0 _lterm3;
      ()

and freshen2_let_logic_abs : Var.Subst.t * Var.Subst.t -> let_logic_abs -> let_logic_abs -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_fbindings2, _lformula1, _lterm0), (_fbindings5, _lformula4, _lterm3) ->
      let (var_env1, var_env2) = freshen2_fbindings (var_env1, var_env2) _fbindings2 _fbindings5 in
      (var_env1, var_env2)

and create_let_logic_abs : let_logic_abs -> opaque_let_logic_abs = 
  function body -> {
    let_logic_abs_delayed = (Var.Subst.id);
    let_logic_abs = body
  }

and open_let_logic_abs : opaque_let_logic_abs -> let_logic_abs = function abstraction ->
  let (var_delayed) = abstraction.let_logic_abs_delayed in
  let body = abstraction.let_logic_abs in
  let (var_bvars) = bound_let_logic_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_let_logic_abs (var_delayed, var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.let_logic_abs_delayed <- (Var.Subst.id);
    abstraction.let_logic_abs <- body
  end;
  body

and open2_let_logic_abs : opaque_let_logic_abs -> opaque_let_logic_abs -> let_logic_abs * let_logic_abs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_let_logic_abs x1 x2

and open2i_let_logic_abs : opaque_let_logic_abs -> opaque_let_logic_abs -> let_logic_abs * let_logic_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.let_logic_abs_delayed in
  let body1 = abstraction1.let_logic_abs in
  let (var_delayed2) = abstraction2.let_logic_abs_delayed in
  let body2 = abstraction2.let_logic_abs in
  let (var_env1, var_env2) = freshen2_let_logic_abs (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_let_logic_abs (var_delayed1, var_env1) body1 in
  let body2 = subst_let_logic_abs (var_delayed2, var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.let_logic_abs_delayed <- (Var.Subst.id);
    abstraction1.let_logic_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.let_logic_abs_delayed <- (Var.Subst.id);
    abstraction2.let_logic_abs <- body2
  end;
  body1, body2

and apply_let_logic_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.let_logic_abs_delayed in {
      abstraction with let_logic_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_function_output_abs : Var.Subst.t -> function_output_abs -> function_output_abs = fun (var_ienv) -> function
  (_logic_bindings0) ->
    ((subst_logic_bindings (var_ienv)) _logic_bindings0)

and bound_function_output_abs : function_output_abs -> Var.AtomSet.t = 
  function function_output_abs -> bound_accu_function_output_abs (Var.AtomSet.empty) function_output_abs

and bound_free_function_output_abs : function_output_abs -> Var.AtomSet.t * Var.AtomSet.t = 
  function function_output_abs -> bound_free_accu_function_output_abs (Var.AtomSet.empty, Var.AtomSet.empty) function_output_abs

and equal_function_output_abs : function_output_abs -> function_output_abs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_function_output_abs x1 x2

and import_function_output_abs = fun (var_ienv) -> function
  (_logic_bindings0) ->
    ((import_logic_bindings (var_ienv)) _logic_bindings0)

and bvi_accu_function_output_abs = fun (var_bvars) -> function
  (_logic_bindings0) ->
      let (var_bvars) = bvi_accu_logic_bindings (var_bvars) _logic_bindings0 in
      (var_bvars)

and bvi_function_output_abs = 
  function function_output_abs -> bvi_accu_function_output_abs (Identifier.Map.empty) function_output_abs

and bound_accu_function_output_abs = fun (var_bvars) -> function
  (_logic_bindings0) ->
      let (var_bvars) = bound_accu_logic_bindings (var_bvars) _logic_bindings0 in
      (var_bvars)

and export_function_output_abs : Var.AtomIdMap.t -> function_output_abs -> Raw.function_output_abs = fun (var_im) -> function
  (_logic_bindings0) ->
    ((export_logic_bindings (var_im)) _logic_bindings0)

and flatten_function_output_abs : function_output_abs -> Flat.function_output_abs = function
  (_logic_bindings0) ->
    (flatten_logic_bindings _logic_bindings0)

and unflatten_function_output_abs : Flat.function_output_abs -> function_output_abs = function
  (_logic_bindings0) ->
    (unflatten_logic_bindings _logic_bindings0)

and bound_free_accu_function_output_abs = fun (var_bvars, var_ifvars) -> function
  (_logic_bindings0) ->
      let (var_bvars, var_ifvars) = bound_free_accu_logic_bindings (var_bvars, var_ifvars) _logic_bindings0 in
      (var_bvars, var_ifvars)

and aeq_function_output_abs : unit -> function_output_abs -> function_output_abs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_logic_bindings0), (_logic_bindings1) ->
      aeq_logic_bindings () _logic_bindings0 _logic_bindings1;
      ()

and freshen2_function_output_abs : Var.Subst.t * Var.Subst.t -> function_output_abs -> function_output_abs -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_logic_bindings0), (_logic_bindings1) ->
      let (var_env1, var_env2) = freshen2_logic_bindings (var_env1, var_env2) _logic_bindings0 _logic_bindings1 in
      (var_env1, var_env2)

and create_function_output_abs : function_output_abs -> opaque_function_output_abs = 
  function body -> {
    function_output_abs_delayed = (Var.Subst.id);
    function_output_abs = body
  }

and open_function_output_abs : opaque_function_output_abs -> function_output_abs = function abstraction ->
  let (var_delayed) = abstraction.function_output_abs_delayed in
  let body = abstraction.function_output_abs in
  let (var_bvars) = bound_function_output_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_function_output_abs (var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.function_output_abs_delayed <- (Var.Subst.id);
    abstraction.function_output_abs <- body
  end;
  body

and open2_function_output_abs : opaque_function_output_abs -> opaque_function_output_abs -> function_output_abs * function_output_abs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_function_output_abs x1 x2

and open2i_function_output_abs : opaque_function_output_abs -> opaque_function_output_abs -> function_output_abs * function_output_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.function_output_abs_delayed in
  let body1 = abstraction1.function_output_abs in
  let (var_delayed2) = abstraction2.function_output_abs_delayed in
  let body2 = abstraction2.function_output_abs in
  let (var_env1, var_env2) = freshen2_function_output_abs (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_function_output_abs (var_env1) body1 in
  let body2 = subst_function_output_abs (var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.function_output_abs_delayed <- (Var.Subst.id);
    abstraction1.function_output_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.function_output_abs_delayed <- (Var.Subst.id);
    abstraction2.function_output_abs <- body2
  end;
  body1, body2

and apply_function_output_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.function_output_abs_delayed in {
      abstraction with function_output_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_clause_abs : Var.Subst.t * Var.Subst.t -> clause_abs -> clause_abs = fun (var_oenv, var_ienv) -> function
  (_pattern1, _lterm0) ->
    ((subst_pattern (var_oenv, var_ienv)) _pattern1, (subst_lterm (var_ienv)) _lterm0)

and bound_clause_abs : clause_abs -> Var.AtomSet.t = 
  function clause_abs -> bound_accu_clause_abs (Var.AtomSet.empty) clause_abs

and bound_free_clause_abs : clause_abs -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t = 
  function clause_abs -> bound_free_accu_clause_abs (Var.AtomSet.empty, Var.AtomSet.empty, Var.AtomSet.empty) clause_abs

and equal_clause_abs : clause_abs -> clause_abs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_clause_abs x1 x2

and import_clause_abs = fun (var_oenv, var_ienv) -> function
  (_pattern1, _lterm0) ->
    ((import_pattern (var_oenv, var_ienv)) _pattern1, (import_lterm (var_ienv)) _lterm0)

and bvi_accu_clause_abs = fun (var_bvars) -> function
  (_pattern1, _lterm0) ->
      let (var_bvars) = bvi_accu_pattern (var_bvars) _pattern1 in
      (var_bvars)

and bvi_clause_abs = 
  function clause_abs -> bvi_accu_clause_abs (Identifier.Map.empty) clause_abs

and bound_accu_clause_abs = fun (var_bvars) -> function
  (_pattern1, _lterm0) ->
      let (var_bvars) = bound_accu_pattern (var_bvars) _pattern1 in
      (var_bvars)

and export_clause_abs : Var.AtomIdMap.t * Var.AtomIdMap.t -> clause_abs -> Raw.clause_abs = fun (var_om, var_im) -> function
  (_pattern1, _lterm0) ->
    ((export_pattern (var_om, var_im)) _pattern1, (export_lterm (var_im)) _lterm0)

and flatten_clause_abs : clause_abs -> Flat.clause_abs = function
  (_pattern1, _lterm0) ->
    (flatten_pattern _pattern1, flatten_lterm _lterm0)

and unflatten_clause_abs : Flat.clause_abs -> clause_abs = function
  (_pattern1, _lterm0) ->
    (unflatten_pattern _pattern1, unflatten_lterm _lterm0)

and bound_free_accu_clause_abs = fun (var_bvars, var_ifvars, var_ofvars) -> function
  (_pattern1, _lterm0) ->
      let (var_bvars, var_ofvars) = bound_free_accu_pattern (var_bvars, var_ofvars) _pattern1 in
      let (var_ifvars) = free_accu_lterm (var_ifvars) _lterm0 in
      (var_bvars, var_ifvars, var_ofvars)

and aeq_clause_abs : unit -> clause_abs -> clause_abs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_pattern1, _lterm0), (_pattern3, _lterm2) ->
      aeq_pattern () _pattern1 _pattern3;
      aeq_lterm () _lterm0 _lterm2;
      ()

and freshen2_clause_abs : Var.Subst.t * Var.Subst.t -> clause_abs -> clause_abs -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_pattern1, _lterm0), (_pattern3, _lterm2) ->
      let (var_env1, var_env2) = freshen2_pattern (var_env1, var_env2) _pattern1 _pattern3 in
      (var_env1, var_env2)

and create_clause_abs : clause_abs -> opaque_clause_abs = 
  function body -> {
    clause_abs_delayed = (Var.Subst.id);
    clause_abs = body
  }

and open_clause_abs : opaque_clause_abs -> clause_abs = function abstraction ->
  let (var_delayed) = abstraction.clause_abs_delayed in
  let body = abstraction.clause_abs in
  let (var_bvars) = bound_clause_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_clause_abs (var_delayed, var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.clause_abs_delayed <- (Var.Subst.id);
    abstraction.clause_abs <- body
  end;
  body

and open2_clause_abs : opaque_clause_abs -> opaque_clause_abs -> clause_abs * clause_abs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_clause_abs x1 x2

and open2i_clause_abs : opaque_clause_abs -> opaque_clause_abs -> clause_abs * clause_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.clause_abs_delayed in
  let body1 = abstraction1.clause_abs in
  let (var_delayed2) = abstraction2.clause_abs_delayed in
  let body2 = abstraction2.clause_abs in
  let (var_env1, var_env2) = freshen2_clause_abs (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_clause_abs (var_delayed1, var_env1) body1 in
  let body2 = subst_clause_abs (var_delayed2, var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.clause_abs_delayed <- (Var.Subst.id);
    abstraction1.clause_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.clause_abs_delayed <- (Var.Subst.id);
    abstraction2.clause_abs <- body2
  end;
  body1, body2

and apply_clause_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.clause_abs_delayed in {
      abstraction with clause_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_scheme_abs : Var.Subst.t -> scheme_abs -> scheme_abs = fun (var_ienv) -> function
  (_type_parameters1, _term_type0) ->
    ((subst_type_parameters (var_ienv)) _type_parameters1, (subst_term_type (var_ienv)) _term_type0)

and bound_scheme_abs : scheme_abs -> Var.AtomSet.t = 
  function scheme_abs -> bound_accu_scheme_abs (Var.AtomSet.empty) scheme_abs

and bound_free_scheme_abs : scheme_abs -> Var.AtomSet.t * Var.AtomSet.t = 
  function scheme_abs -> bound_free_accu_scheme_abs (Var.AtomSet.empty, Var.AtomSet.empty) scheme_abs

and equal_scheme_abs : scheme_abs -> scheme_abs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_scheme_abs x1 x2

and import_scheme_abs = fun (var_ienv) -> function
  (_type_parameters1, _term_type0) ->
    ((import_type_parameters (var_ienv)) _type_parameters1, (import_term_type (var_ienv)) _term_type0)

and bvi_accu_scheme_abs = fun (var_bvars) -> function
  (_type_parameters1, _term_type0) ->
      let (var_bvars) = bvi_accu_type_parameters (var_bvars) _type_parameters1 in
      (var_bvars)

and bvi_scheme_abs = 
  function scheme_abs -> bvi_accu_scheme_abs (Identifier.Map.empty) scheme_abs

and bound_accu_scheme_abs = fun (var_bvars) -> function
  (_type_parameters1, _term_type0) ->
      let (var_bvars) = bound_accu_type_parameters (var_bvars) _type_parameters1 in
      (var_bvars)

and export_scheme_abs : Var.AtomIdMap.t -> scheme_abs -> Raw.scheme_abs = fun (var_im) -> function
  (_type_parameters1, _term_type0) ->
    ((export_type_parameters (var_im)) _type_parameters1, (export_term_type (var_im)) _term_type0)

and flatten_scheme_abs : scheme_abs -> Flat.scheme_abs = function
  (_type_parameters1, _term_type0) ->
    (flatten_type_parameters _type_parameters1, flatten_term_type _term_type0)

and unflatten_scheme_abs : Flat.scheme_abs -> scheme_abs = function
  (_type_parameters1, _term_type0) ->
    (unflatten_type_parameters _type_parameters1, unflatten_term_type _term_type0)

and bound_free_accu_scheme_abs = fun (var_bvars, var_ifvars) -> function
  (_type_parameters1, _term_type0) ->
      let (var_bvars) = bound_free_accu_type_parameters (var_bvars) _type_parameters1 in
      let (var_ifvars) = free_accu_term_type (var_ifvars) _term_type0 in
      (var_bvars, var_ifvars)

and aeq_scheme_abs : unit -> scheme_abs -> scheme_abs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_type_parameters1, _term_type0), (_type_parameters3, _term_type2) ->
      aeq_type_parameters () _type_parameters1 _type_parameters3;
      aeq_term_type () _term_type0 _term_type2;
      ()

and freshen2_scheme_abs : Var.Subst.t * Var.Subst.t -> scheme_abs -> scheme_abs -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_type_parameters1, _term_type0), (_type_parameters3, _term_type2) ->
      let (var_env1, var_env2) = freshen2_type_parameters (var_env1, var_env2) _type_parameters1 _type_parameters3 in
      (var_env1, var_env2)

and create_scheme_abs : scheme_abs -> opaque_scheme_abs = 
  function body -> {
    scheme_abs_delayed = (Var.Subst.id);
    scheme_abs = body
  }

and open_scheme_abs : opaque_scheme_abs -> scheme_abs = function abstraction ->
  let (var_delayed) = abstraction.scheme_abs_delayed in
  let body = abstraction.scheme_abs in
  let (var_bvars) = bound_scheme_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_scheme_abs (var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.scheme_abs_delayed <- (Var.Subst.id);
    abstraction.scheme_abs <- body
  end;
  body

and open2_scheme_abs : opaque_scheme_abs -> opaque_scheme_abs -> scheme_abs * scheme_abs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_scheme_abs x1 x2

and open2i_scheme_abs : opaque_scheme_abs -> opaque_scheme_abs -> scheme_abs * scheme_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.scheme_abs_delayed in
  let body1 = abstraction1.scheme_abs in
  let (var_delayed2) = abstraction2.scheme_abs_delayed in
  let body2 = abstraction2.scheme_abs in
  let (var_env1, var_env2) = freshen2_scheme_abs (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_scheme_abs (var_env1) body1 in
  let body2 = subst_scheme_abs (var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.scheme_abs_delayed <- (Var.Subst.id);
    abstraction1.scheme_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.scheme_abs_delayed <- (Var.Subst.id);
    abstraction2.scheme_abs <- body2
  end;
  body1, body2

and apply_scheme_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.scheme_abs_delayed in {
      abstraction with scheme_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_lfun_abs : Var.Subst.t * Var.Subst.t -> lfun_abs -> lfun_abs = fun (var_oenv, var_ienv) -> function
  (_fbindings1, _lformula0) ->
    ((subst_fbindings (var_oenv, var_ienv)) _fbindings1, (subst_lformula (var_ienv)) _lformula0)

and bound_lfun_abs : lfun_abs -> Var.AtomSet.t = 
  function lfun_abs -> bound_accu_lfun_abs (Var.AtomSet.empty) lfun_abs

and bound_free_lfun_abs : lfun_abs -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t = 
  function lfun_abs -> bound_free_accu_lfun_abs (Var.AtomSet.empty, Var.AtomSet.empty, Var.AtomSet.empty) lfun_abs

and equal_lfun_abs : lfun_abs -> lfun_abs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_lfun_abs x1 x2

and import_lfun_abs = fun (var_oenv, var_ienv) -> function
  (_fbindings1, _lformula0) ->
    ((import_fbindings (var_oenv, var_ienv)) _fbindings1, (import_lformula (var_ienv)) _lformula0)

and bvi_accu_lfun_abs = fun (var_bvars) -> function
  (_fbindings1, _lformula0) ->
      let (var_bvars) = bvi_accu_fbindings (var_bvars) _fbindings1 in
      (var_bvars)

and bvi_lfun_abs = 
  function lfun_abs -> bvi_accu_lfun_abs (Identifier.Map.empty) lfun_abs

and bound_accu_lfun_abs = fun (var_bvars) -> function
  (_fbindings1, _lformula0) ->
      let (var_bvars) = bound_accu_fbindings (var_bvars) _fbindings1 in
      (var_bvars)

and export_lfun_abs : Var.AtomIdMap.t * Var.AtomIdMap.t -> lfun_abs -> Raw.lfun_abs = fun (var_om, var_im) -> function
  (_fbindings1, _lformula0) ->
    ((export_fbindings (var_om, var_im)) _fbindings1, (export_lformula (var_im)) _lformula0)

and flatten_lfun_abs : lfun_abs -> Flat.lfun_abs = function
  (_fbindings1, _lformula0) ->
    (flatten_fbindings _fbindings1, flatten_lformula _lformula0)

and unflatten_lfun_abs : Flat.lfun_abs -> lfun_abs = function
  (_fbindings1, _lformula0) ->
    (unflatten_fbindings _fbindings1, unflatten_lformula _lformula0)

and bound_free_accu_lfun_abs = fun (var_bvars, var_ifvars, var_ofvars) -> function
  (_fbindings1, _lformula0) ->
      let (var_bvars, var_ofvars) = bound_free_accu_fbindings (var_bvars, var_ofvars) _fbindings1 in
      let (var_ifvars) = free_accu_lformula (var_ifvars) _lformula0 in
      (var_bvars, var_ifvars, var_ofvars)

and aeq_lfun_abs : unit -> lfun_abs -> lfun_abs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_fbindings1, _lformula0), (_fbindings3, _lformula2) ->
      aeq_fbindings () _fbindings1 _fbindings3;
      aeq_lformula () _lformula0 _lformula2;
      ()

and freshen2_lfun_abs : Var.Subst.t * Var.Subst.t -> lfun_abs -> lfun_abs -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_fbindings1, _lformula0), (_fbindings3, _lformula2) ->
      let (var_env1, var_env2) = freshen2_fbindings (var_env1, var_env2) _fbindings1 _fbindings3 in
      (var_env1, var_env2)

and create_lfun_abs : lfun_abs -> opaque_lfun_abs = 
  function body -> {
    lfun_abs_delayed = (Var.Subst.id);
    lfun_abs = body
  }

and open_lfun_abs : opaque_lfun_abs -> lfun_abs = function abstraction ->
  let (var_delayed) = abstraction.lfun_abs_delayed in
  let body = abstraction.lfun_abs in
  let (var_bvars) = bound_lfun_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_lfun_abs (var_delayed, var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.lfun_abs_delayed <- (Var.Subst.id);
    abstraction.lfun_abs <- body
  end;
  body

and open2_lfun_abs : opaque_lfun_abs -> opaque_lfun_abs -> lfun_abs * lfun_abs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_lfun_abs x1 x2

and open2i_lfun_abs : opaque_lfun_abs -> opaque_lfun_abs -> lfun_abs * lfun_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.lfun_abs_delayed in
  let body1 = abstraction1.lfun_abs in
  let (var_delayed2) = abstraction2.lfun_abs_delayed in
  let body2 = abstraction2.lfun_abs in
  let (var_env1, var_env2) = freshen2_lfun_abs (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_lfun_abs (var_delayed1, var_env1) body1 in
  let body2 = subst_lfun_abs (var_delayed2, var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.lfun_abs_delayed <- (Var.Subst.id);
    abstraction1.lfun_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.lfun_abs_delayed <- (Var.Subst.id);
    abstraction2.lfun_abs <- body2
  end;
  body1, body2

and apply_lfun_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.lfun_abs_delayed in {
      abstraction with lfun_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_fforalltys_abs : Var.Subst.t -> fforalltys_abs -> fforalltys_abs = fun (var_ienv) -> function
  (_type_parameters1, _lformula0) ->
    ((subst_type_parameters (var_ienv)) _type_parameters1, (subst_lformula (var_ienv)) _lformula0)

and bound_fforalltys_abs : fforalltys_abs -> Var.AtomSet.t = 
  function fforalltys_abs -> bound_accu_fforalltys_abs (Var.AtomSet.empty) fforalltys_abs

and bound_free_fforalltys_abs : fforalltys_abs -> Var.AtomSet.t * Var.AtomSet.t = 
  function fforalltys_abs -> bound_free_accu_fforalltys_abs (Var.AtomSet.empty, Var.AtomSet.empty) fforalltys_abs

and equal_fforalltys_abs : fforalltys_abs -> fforalltys_abs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_fforalltys_abs x1 x2

and import_fforalltys_abs = fun (var_ienv) -> function
  (_type_parameters1, _lformula0) ->
    ((import_type_parameters (var_ienv)) _type_parameters1, (import_lformula (var_ienv)) _lformula0)

and bvi_accu_fforalltys_abs = fun (var_bvars) -> function
  (_type_parameters1, _lformula0) ->
      let (var_bvars) = bvi_accu_type_parameters (var_bvars) _type_parameters1 in
      (var_bvars)

and bvi_fforalltys_abs = 
  function fforalltys_abs -> bvi_accu_fforalltys_abs (Identifier.Map.empty) fforalltys_abs

and bound_accu_fforalltys_abs = fun (var_bvars) -> function
  (_type_parameters1, _lformula0) ->
      let (var_bvars) = bound_accu_type_parameters (var_bvars) _type_parameters1 in
      (var_bvars)

and export_fforalltys_abs : Var.AtomIdMap.t -> fforalltys_abs -> Raw.fforalltys_abs = fun (var_im) -> function
  (_type_parameters1, _lformula0) ->
    ((export_type_parameters (var_im)) _type_parameters1, (export_lformula (var_im)) _lformula0)

and flatten_fforalltys_abs : fforalltys_abs -> Flat.fforalltys_abs = function
  (_type_parameters1, _lformula0) ->
    (flatten_type_parameters _type_parameters1, flatten_lformula _lformula0)

and unflatten_fforalltys_abs : Flat.fforalltys_abs -> fforalltys_abs = function
  (_type_parameters1, _lformula0) ->
    (unflatten_type_parameters _type_parameters1, unflatten_lformula _lformula0)

and bound_free_accu_fforalltys_abs = fun (var_bvars, var_ifvars) -> function
  (_type_parameters1, _lformula0) ->
      let (var_bvars) = bound_free_accu_type_parameters (var_bvars) _type_parameters1 in
      let (var_ifvars) = free_accu_lformula (var_ifvars) _lformula0 in
      (var_bvars, var_ifvars)

and aeq_fforalltys_abs : unit -> fforalltys_abs -> fforalltys_abs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_type_parameters1, _lformula0), (_type_parameters3, _lformula2) ->
      aeq_type_parameters () _type_parameters1 _type_parameters3;
      aeq_lformula () _lformula0 _lformula2;
      ()

and freshen2_fforalltys_abs : Var.Subst.t * Var.Subst.t -> fforalltys_abs -> fforalltys_abs -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_type_parameters1, _lformula0), (_type_parameters3, _lformula2) ->
      let (var_env1, var_env2) = freshen2_type_parameters (var_env1, var_env2) _type_parameters1 _type_parameters3 in
      (var_env1, var_env2)

and create_fforalltys_abs : fforalltys_abs -> opaque_fforalltys_abs = 
  function body -> {
    fforalltys_abs_delayed = (Var.Subst.id);
    fforalltys_abs = body
  }

and open_fforalltys_abs : opaque_fforalltys_abs -> fforalltys_abs = function abstraction ->
  let (var_delayed) = abstraction.fforalltys_abs_delayed in
  let body = abstraction.fforalltys_abs in
  let (var_bvars) = bound_fforalltys_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_fforalltys_abs (var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.fforalltys_abs_delayed <- (Var.Subst.id);
    abstraction.fforalltys_abs <- body
  end;
  body

and open2_fforalltys_abs : opaque_fforalltys_abs -> opaque_fforalltys_abs -> fforalltys_abs * fforalltys_abs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_fforalltys_abs x1 x2

and open2i_fforalltys_abs : opaque_fforalltys_abs -> opaque_fforalltys_abs -> fforalltys_abs * fforalltys_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.fforalltys_abs_delayed in
  let body1 = abstraction1.fforalltys_abs in
  let (var_delayed2) = abstraction2.fforalltys_abs_delayed in
  let body2 = abstraction2.fforalltys_abs in
  let (var_env1, var_env2) = freshen2_fforalltys_abs (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_fforalltys_abs (var_env1) body1 in
  let body2 = subst_fforalltys_abs (var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.fforalltys_abs_delayed <- (Var.Subst.id);
    abstraction1.fforalltys_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.fforalltys_abs_delayed <- (Var.Subst.id);
    abstraction2.fforalltys_abs <- body2
  end;
  body1, body2

and apply_fforalltys_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.fforalltys_abs_delayed in {
      abstraction with fforalltys_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_fexiststys_abs : Var.Subst.t -> fexiststys_abs -> fexiststys_abs = fun (var_ienv) -> function
  (_type_parameters1, _lformula0) ->
    ((subst_type_parameters (var_ienv)) _type_parameters1, (subst_lformula (var_ienv)) _lformula0)

and bound_fexiststys_abs : fexiststys_abs -> Var.AtomSet.t = 
  function fexiststys_abs -> bound_accu_fexiststys_abs (Var.AtomSet.empty) fexiststys_abs

and bound_free_fexiststys_abs : fexiststys_abs -> Var.AtomSet.t * Var.AtomSet.t = 
  function fexiststys_abs -> bound_free_accu_fexiststys_abs (Var.AtomSet.empty, Var.AtomSet.empty) fexiststys_abs

and equal_fexiststys_abs : fexiststys_abs -> fexiststys_abs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_fexiststys_abs x1 x2

and import_fexiststys_abs = fun (var_ienv) -> function
  (_type_parameters1, _lformula0) ->
    ((import_type_parameters (var_ienv)) _type_parameters1, (import_lformula (var_ienv)) _lformula0)

and bvi_accu_fexiststys_abs = fun (var_bvars) -> function
  (_type_parameters1, _lformula0) ->
      let (var_bvars) = bvi_accu_type_parameters (var_bvars) _type_parameters1 in
      (var_bvars)

and bvi_fexiststys_abs = 
  function fexiststys_abs -> bvi_accu_fexiststys_abs (Identifier.Map.empty) fexiststys_abs

and bound_accu_fexiststys_abs = fun (var_bvars) -> function
  (_type_parameters1, _lformula0) ->
      let (var_bvars) = bound_accu_type_parameters (var_bvars) _type_parameters1 in
      (var_bvars)

and export_fexiststys_abs : Var.AtomIdMap.t -> fexiststys_abs -> Raw.fexiststys_abs = fun (var_im) -> function
  (_type_parameters1, _lformula0) ->
    ((export_type_parameters (var_im)) _type_parameters1, (export_lformula (var_im)) _lformula0)

and flatten_fexiststys_abs : fexiststys_abs -> Flat.fexiststys_abs = function
  (_type_parameters1, _lformula0) ->
    (flatten_type_parameters _type_parameters1, flatten_lformula _lformula0)

and unflatten_fexiststys_abs : Flat.fexiststys_abs -> fexiststys_abs = function
  (_type_parameters1, _lformula0) ->
    (unflatten_type_parameters _type_parameters1, unflatten_lformula _lformula0)

and bound_free_accu_fexiststys_abs = fun (var_bvars, var_ifvars) -> function
  (_type_parameters1, _lformula0) ->
      let (var_bvars) = bound_free_accu_type_parameters (var_bvars) _type_parameters1 in
      let (var_ifvars) = free_accu_lformula (var_ifvars) _lformula0 in
      (var_bvars, var_ifvars)

and aeq_fexiststys_abs : unit -> fexiststys_abs -> fexiststys_abs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_type_parameters1, _lformula0), (_type_parameters3, _lformula2) ->
      aeq_type_parameters () _type_parameters1 _type_parameters3;
      aeq_lformula () _lformula0 _lformula2;
      ()

and freshen2_fexiststys_abs : Var.Subst.t * Var.Subst.t -> fexiststys_abs -> fexiststys_abs -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_type_parameters1, _lformula0), (_type_parameters3, _lformula2) ->
      let (var_env1, var_env2) = freshen2_type_parameters (var_env1, var_env2) _type_parameters1 _type_parameters3 in
      (var_env1, var_env2)

and create_fexiststys_abs : fexiststys_abs -> opaque_fexiststys_abs = 
  function body -> {
    fexiststys_abs_delayed = (Var.Subst.id);
    fexiststys_abs = body
  }

and open_fexiststys_abs : opaque_fexiststys_abs -> fexiststys_abs = function abstraction ->
  let (var_delayed) = abstraction.fexiststys_abs_delayed in
  let body = abstraction.fexiststys_abs in
  let (var_bvars) = bound_fexiststys_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_fexiststys_abs (var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.fexiststys_abs_delayed <- (Var.Subst.id);
    abstraction.fexiststys_abs <- body
  end;
  body

and open2_fexiststys_abs : opaque_fexiststys_abs -> opaque_fexiststys_abs -> fexiststys_abs * fexiststys_abs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_fexiststys_abs x1 x2

and open2i_fexiststys_abs : opaque_fexiststys_abs -> opaque_fexiststys_abs -> fexiststys_abs * fexiststys_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.fexiststys_abs_delayed in
  let body1 = abstraction1.fexiststys_abs in
  let (var_delayed2) = abstraction2.fexiststys_abs_delayed in
  let body2 = abstraction2.fexiststys_abs in
  let (var_env1, var_env2) = freshen2_fexiststys_abs (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_fexiststys_abs (var_env1) body1 in
  let body2 = subst_fexiststys_abs (var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.fexiststys_abs_delayed <- (Var.Subst.id);
    abstraction1.fexiststys_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.fexiststys_abs_delayed <- (Var.Subst.id);
    abstraction2.fexiststys_abs <- body2
  end;
  body1, body2

and apply_fexiststys_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.fexiststys_abs_delayed in {
      abstraction with fexiststys_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_lforall_abs : Var.Subst.t * Var.Subst.t -> lforall_abs -> lforall_abs = fun (var_oenv, var_ienv) -> function
  (_fbindings2, _triggers1, _lformula0) ->
    ((subst_fbindings (var_oenv, var_ienv)) _fbindings2, List.map (subst_trigger (var_ienv)) _triggers1, (subst_lformula (var_ienv)) _lformula0)

and bound_lforall_abs : lforall_abs -> Var.AtomSet.t = 
  function lforall_abs -> bound_accu_lforall_abs (Var.AtomSet.empty) lforall_abs

and bound_free_lforall_abs : lforall_abs -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t = 
  function lforall_abs -> bound_free_accu_lforall_abs (Var.AtomSet.empty, Var.AtomSet.empty, Var.AtomSet.empty) lforall_abs

and equal_lforall_abs : lforall_abs -> lforall_abs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_lforall_abs x1 x2

and import_lforall_abs = fun (var_oenv, var_ienv) -> function
  (_fbindings2, _triggers1, _lformula0) ->
    ((import_fbindings (var_oenv, var_ienv)) _fbindings2, List.map (import_trigger (var_ienv)) _triggers1, (import_lformula (var_ienv)) _lformula0)

and bvi_accu_lforall_abs = fun (var_bvars) -> function
  (_fbindings2, _triggers1, _lformula0) ->
      let (var_bvars) = bvi_accu_fbindings (var_bvars) _fbindings2 in
      (var_bvars)

and bvi_lforall_abs = 
  function lforall_abs -> bvi_accu_lforall_abs (Identifier.Map.empty) lforall_abs

and bound_accu_lforall_abs = fun (var_bvars) -> function
  (_fbindings2, _triggers1, _lformula0) ->
      let (var_bvars) = bound_accu_fbindings (var_bvars) _fbindings2 in
      (var_bvars)

and export_lforall_abs : Var.AtomIdMap.t * Var.AtomIdMap.t -> lforall_abs -> Raw.lforall_abs = fun (var_om, var_im) -> function
  (_fbindings2, _triggers1, _lformula0) ->
    ((export_fbindings (var_om, var_im)) _fbindings2, List.map (export_trigger (var_im)) _triggers1, (export_lformula (var_im)) _lformula0)

and flatten_lforall_abs : lforall_abs -> Flat.lforall_abs = function
  (_fbindings2, _triggers1, _lformula0) ->
    (flatten_fbindings _fbindings2, List.map flatten_trigger _triggers1, flatten_lformula _lformula0)

and unflatten_lforall_abs : Flat.lforall_abs -> lforall_abs = function
  (_fbindings2, _triggers1, _lformula0) ->
    (unflatten_fbindings _fbindings2, List.map unflatten_trigger _triggers1, unflatten_lformula _lformula0)

and bound_free_accu_lforall_abs = fun (var_bvars, var_ifvars, var_ofvars) -> function
  (_fbindings2, _triggers1, _lformula0) ->
      let (var_bvars, var_ofvars) = bound_free_accu_fbindings (var_bvars, var_ofvars) _fbindings2 in
      let (var_ifvars) = List.fold_left free_accu_trigger (var_ifvars) _triggers1 in
      let (var_ifvars) = free_accu_lformula (var_ifvars) _lformula0 in
      (var_bvars, var_ifvars, var_ofvars)

and aeq_lforall_abs : unit -> lforall_abs -> lforall_abs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_fbindings2, _triggers1, _lformula0), (_fbindings5, _triggers4, _lformula3) ->
      aeq_fbindings () _fbindings2 _fbindings5;
      List.fold_left2 aeq_trigger () _triggers1 _triggers4;
      aeq_lformula () _lformula0 _lformula3;
      ()

and freshen2_lforall_abs : Var.Subst.t * Var.Subst.t -> lforall_abs -> lforall_abs -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_fbindings2, _triggers1, _lformula0), (_fbindings5, _triggers4, _lformula3) ->
      let (var_env1, var_env2) = freshen2_fbindings (var_env1, var_env2) _fbindings2 _fbindings5 in
      (var_env1, var_env2)

and create_lforall_abs : lforall_abs -> opaque_lforall_abs = 
  function body -> {
    lforall_abs_delayed = (Var.Subst.id);
    lforall_abs = body
  }

and open_lforall_abs : opaque_lforall_abs -> lforall_abs = function abstraction ->
  let (var_delayed) = abstraction.lforall_abs_delayed in
  let body = abstraction.lforall_abs in
  let (var_bvars) = bound_lforall_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_lforall_abs (var_delayed, var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.lforall_abs_delayed <- (Var.Subst.id);
    abstraction.lforall_abs <- body
  end;
  body

and open2_lforall_abs : opaque_lforall_abs -> opaque_lforall_abs -> lforall_abs * lforall_abs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_lforall_abs x1 x2

and open2i_lforall_abs : opaque_lforall_abs -> opaque_lforall_abs -> lforall_abs * lforall_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.lforall_abs_delayed in
  let body1 = abstraction1.lforall_abs in
  let (var_delayed2) = abstraction2.lforall_abs_delayed in
  let body2 = abstraction2.lforall_abs in
  let (var_env1, var_env2) = freshen2_lforall_abs (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_lforall_abs (var_delayed1, var_env1) body1 in
  let body2 = subst_lforall_abs (var_delayed2, var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.lforall_abs_delayed <- (Var.Subst.id);
    abstraction1.lforall_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.lforall_abs_delayed <- (Var.Subst.id);
    abstraction2.lforall_abs <- body2
  end;
  body1, body2

and apply_lforall_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.lforall_abs_delayed in {
      abstraction with lforall_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_lexists_abs : Var.Subst.t * Var.Subst.t -> lexists_abs -> lexists_abs = fun (var_oenv, var_ienv) -> function
  (_fbindings1, _lformula0) ->
    ((subst_fbindings (var_oenv, var_ienv)) _fbindings1, (subst_lformula (var_ienv)) _lformula0)

and bound_lexists_abs : lexists_abs -> Var.AtomSet.t = 
  function lexists_abs -> bound_accu_lexists_abs (Var.AtomSet.empty) lexists_abs

and bound_free_lexists_abs : lexists_abs -> Var.AtomSet.t * Var.AtomSet.t * Var.AtomSet.t = 
  function lexists_abs -> bound_free_accu_lexists_abs (Var.AtomSet.empty, Var.AtomSet.empty, Var.AtomSet.empty) lexists_abs

and equal_lexists_abs : lexists_abs -> lexists_abs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_lexists_abs x1 x2

and import_lexists_abs = fun (var_oenv, var_ienv) -> function
  (_fbindings1, _lformula0) ->
    ((import_fbindings (var_oenv, var_ienv)) _fbindings1, (import_lformula (var_ienv)) _lformula0)

and bvi_accu_lexists_abs = fun (var_bvars) -> function
  (_fbindings1, _lformula0) ->
      let (var_bvars) = bvi_accu_fbindings (var_bvars) _fbindings1 in
      (var_bvars)

and bvi_lexists_abs = 
  function lexists_abs -> bvi_accu_lexists_abs (Identifier.Map.empty) lexists_abs

and bound_accu_lexists_abs = fun (var_bvars) -> function
  (_fbindings1, _lformula0) ->
      let (var_bvars) = bound_accu_fbindings (var_bvars) _fbindings1 in
      (var_bvars)

and export_lexists_abs : Var.AtomIdMap.t * Var.AtomIdMap.t -> lexists_abs -> Raw.lexists_abs = fun (var_om, var_im) -> function
  (_fbindings1, _lformula0) ->
    ((export_fbindings (var_om, var_im)) _fbindings1, (export_lformula (var_im)) _lformula0)

and flatten_lexists_abs : lexists_abs -> Flat.lexists_abs = function
  (_fbindings1, _lformula0) ->
    (flatten_fbindings _fbindings1, flatten_lformula _lformula0)

and unflatten_lexists_abs : Flat.lexists_abs -> lexists_abs = function
  (_fbindings1, _lformula0) ->
    (unflatten_fbindings _fbindings1, unflatten_lformula _lformula0)

and bound_free_accu_lexists_abs = fun (var_bvars, var_ifvars, var_ofvars) -> function
  (_fbindings1, _lformula0) ->
      let (var_bvars, var_ofvars) = bound_free_accu_fbindings (var_bvars, var_ofvars) _fbindings1 in
      let (var_ifvars) = free_accu_lformula (var_ifvars) _lformula0 in
      (var_bvars, var_ifvars, var_ofvars)

and aeq_lexists_abs : unit -> lexists_abs -> lexists_abs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_fbindings1, _lformula0), (_fbindings3, _lformula2) ->
      aeq_fbindings () _fbindings1 _fbindings3;
      aeq_lformula () _lformula0 _lformula2;
      ()

and freshen2_lexists_abs : Var.Subst.t * Var.Subst.t -> lexists_abs -> lexists_abs -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_fbindings1, _lformula0), (_fbindings3, _lformula2) ->
      let (var_env1, var_env2) = freshen2_fbindings (var_env1, var_env2) _fbindings1 _fbindings3 in
      (var_env1, var_env2)

and create_lexists_abs : lexists_abs -> opaque_lexists_abs = 
  function body -> {
    lexists_abs_delayed = (Var.Subst.id);
    lexists_abs = body
  }

and open_lexists_abs : opaque_lexists_abs -> lexists_abs = function abstraction ->
  let (var_delayed) = abstraction.lexists_abs_delayed in
  let body = abstraction.lexists_abs in
  let (var_bvars) = bound_lexists_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_lexists_abs (var_delayed, var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.lexists_abs_delayed <- (Var.Subst.id);
    abstraction.lexists_abs <- body
  end;
  body

and open2_lexists_abs : opaque_lexists_abs -> opaque_lexists_abs -> lexists_abs * lexists_abs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_lexists_abs x1 x2

and open2i_lexists_abs : opaque_lexists_abs -> opaque_lexists_abs -> lexists_abs * lexists_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.lexists_abs_delayed in
  let body1 = abstraction1.lexists_abs in
  let (var_delayed2) = abstraction2.lexists_abs_delayed in
  let body2 = abstraction2.lexists_abs in
  let (var_env1, var_env2) = freshen2_lexists_abs (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_lexists_abs (var_delayed1, var_env1) body1 in
  let body2 = subst_lexists_abs (var_delayed2, var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.lexists_abs_delayed <- (Var.Subst.id);
    abstraction1.lexists_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.lexists_abs_delayed <- (Var.Subst.id);
    abstraction2.lexists_abs <- body2
  end;
  body1, body2

and apply_lexists_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.lexists_abs_delayed in {
      abstraction with lexists_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_formula_scheme_abs : Var.Subst.t -> formula_scheme_abs -> formula_scheme_abs = fun (var_ienv) -> function
  (_type_parameters1, _formula_type0) ->
    ((subst_type_parameters (var_ienv)) _type_parameters1, (subst_formula_type (var_ienv)) _formula_type0)

and bound_formula_scheme_abs : formula_scheme_abs -> Var.AtomSet.t = 
  function formula_scheme_abs -> bound_accu_formula_scheme_abs (Var.AtomSet.empty) formula_scheme_abs

and bound_free_formula_scheme_abs : formula_scheme_abs -> Var.AtomSet.t * Var.AtomSet.t = 
  function formula_scheme_abs -> bound_free_accu_formula_scheme_abs (Var.AtomSet.empty, Var.AtomSet.empty) formula_scheme_abs

and equal_formula_scheme_abs : formula_scheme_abs -> formula_scheme_abs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_formula_scheme_abs x1 x2

and import_formula_scheme_abs = fun (var_ienv) -> function
  (_type_parameters1, _formula_type0) ->
    ((import_type_parameters (var_ienv)) _type_parameters1, (import_formula_type (var_ienv)) _formula_type0)

and bvi_accu_formula_scheme_abs = fun (var_bvars) -> function
  (_type_parameters1, _formula_type0) ->
      let (var_bvars) = bvi_accu_type_parameters (var_bvars) _type_parameters1 in
      (var_bvars)

and bvi_formula_scheme_abs = 
  function formula_scheme_abs -> bvi_accu_formula_scheme_abs (Identifier.Map.empty) formula_scheme_abs

and bound_accu_formula_scheme_abs = fun (var_bvars) -> function
  (_type_parameters1, _formula_type0) ->
      let (var_bvars) = bound_accu_type_parameters (var_bvars) _type_parameters1 in
      (var_bvars)

and export_formula_scheme_abs : Var.AtomIdMap.t -> formula_scheme_abs -> Raw.formula_scheme_abs = fun (var_im) -> function
  (_type_parameters1, _formula_type0) ->
    ((export_type_parameters (var_im)) _type_parameters1, (export_formula_type (var_im)) _formula_type0)

and flatten_formula_scheme_abs : formula_scheme_abs -> Flat.formula_scheme_abs = function
  (_type_parameters1, _formula_type0) ->
    (flatten_type_parameters _type_parameters1, flatten_formula_type _formula_type0)

and unflatten_formula_scheme_abs : Flat.formula_scheme_abs -> formula_scheme_abs = function
  (_type_parameters1, _formula_type0) ->
    (unflatten_type_parameters _type_parameters1, unflatten_formula_type _formula_type0)

and bound_free_accu_formula_scheme_abs = fun (var_bvars, var_ifvars) -> function
  (_type_parameters1, _formula_type0) ->
      let (var_bvars) = bound_free_accu_type_parameters (var_bvars) _type_parameters1 in
      let (var_ifvars) = free_accu_formula_type (var_ifvars) _formula_type0 in
      (var_bvars, var_ifvars)

and aeq_formula_scheme_abs : unit -> formula_scheme_abs -> formula_scheme_abs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_type_parameters1, _formula_type0), (_type_parameters3, _formula_type2) ->
      aeq_type_parameters () _type_parameters1 _type_parameters3;
      aeq_formula_type () _formula_type0 _formula_type2;
      ()

and freshen2_formula_scheme_abs : Var.Subst.t * Var.Subst.t -> formula_scheme_abs -> formula_scheme_abs -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_type_parameters1, _formula_type0), (_type_parameters3, _formula_type2) ->
      let (var_env1, var_env2) = freshen2_type_parameters (var_env1, var_env2) _type_parameters1 _type_parameters3 in
      (var_env1, var_env2)

and create_formula_scheme_abs : formula_scheme_abs -> opaque_formula_scheme_abs = 
  function body -> {
    formula_scheme_abs_delayed = (Var.Subst.id);
    formula_scheme_abs = body
  }

and open_formula_scheme_abs : opaque_formula_scheme_abs -> formula_scheme_abs = function abstraction ->
  let (var_delayed) = abstraction.formula_scheme_abs_delayed in
  let body = abstraction.formula_scheme_abs in
  let (var_bvars) = bound_formula_scheme_abs body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_formula_scheme_abs (var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.formula_scheme_abs_delayed <- (Var.Subst.id);
    abstraction.formula_scheme_abs <- body
  end;
  body

and open2_formula_scheme_abs : opaque_formula_scheme_abs -> opaque_formula_scheme_abs -> formula_scheme_abs * formula_scheme_abs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_formula_scheme_abs x1 x2

and open2i_formula_scheme_abs : opaque_formula_scheme_abs -> opaque_formula_scheme_abs -> formula_scheme_abs * formula_scheme_abs = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.formula_scheme_abs_delayed in
  let body1 = abstraction1.formula_scheme_abs in
  let (var_delayed2) = abstraction2.formula_scheme_abs_delayed in
  let body2 = abstraction2.formula_scheme_abs in
  let (var_env1, var_env2) = freshen2_formula_scheme_abs (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_formula_scheme_abs (var_env1) body1 in
  let body2 = subst_formula_scheme_abs (var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.formula_scheme_abs_delayed <- (Var.Subst.id);
    abstraction1.formula_scheme_abs <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.formula_scheme_abs_delayed <- (Var.Subst.id);
    abstraction2.formula_scheme_abs <- body2
  end;
  body1, body2

and apply_formula_scheme_abs = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.formula_scheme_abs_delayed in {
      abstraction with formula_scheme_abs_delayed = (Var.Subst.compose var_env var_delayed)
    }

class map = object(self)

  method program : program -> program = function
  | PEmpty (_x0) ->
      self#pempty (_x0)
  | PConsComponent (_pcons_abs0) ->
      self#pconscomponent (_pcons_abs0)

  method pempty : ( Position.t ) -> program = 
  function (_x0) -> 
      PEmpty (_x0)

  method pconscomponent : opaque_pcons_abs -> program = 
  function (_pcons_abs0) -> 
      PConsComponent (create_pcons_abs (self#pcons_abs (open_pcons_abs _pcons_abs0)))

  method component : component -> component = function
  | CValue (_ids2, _optional_lformula1, _lterm0) ->
      self#cvalue (_ids2, _optional_lformula1, _lterm0)
  | CRecValue (_ids2, _optional_lformula1, _lterm0) ->
      self#crecvalue (_ids2, _optional_lformula1, _lterm0)
  | CTypeDef (_tid2, _kind1, _type_definition0) ->
      self#ctypedef (_tid2, _kind1, _type_definition0)
  | CFact (_fact_status1, _lformula0) ->
      self#cfact (_fact_status1, _lformula0)
  | CPredicate (_pid1, _predicate_definition0) ->
      self#cpredicate (_pid1, _predicate_definition0)
  | CLogicFunction (_pid1, _logic_function_definition0) ->
      self#clogicfunction (_pid1, _logic_function_definition0)

  method cvalue : ids * optional_lformula * lterm -> component = 
  function (_ids2, _optional_lformula1, _lterm0) -> 
      CValue ((self#ids) _ids2, (self#optional_lformula) _optional_lformula1, (self#lterm) _lterm0)

  method crecvalue : ids * optional_lformula * lterm -> component = 
  function (_ids2, _optional_lformula1, _lterm0) -> 
      CRecValue ((self#ids) _ids2, (self#optional_lformula) _optional_lformula1, (self#lterm) _lterm0)

  method ctypedef : tid * kind * type_definition -> component = 
  function (_tid2, _kind1, _type_definition0) -> 
      CTypeDef ((self#tid) _tid2, (self#kind) _kind1, (self#type_definition) _type_definition0)

  method cfact : fact_status * lformula -> component = 
  function (_fact_status1, _lformula0) -> 
      CFact ((self#fact_status) _fact_status1, (self#lformula) _lformula0)

  method cpredicate : pid * predicate_definition -> component = 
  function (_pid1, _predicate_definition0) -> 
      CPredicate ((self#pid) _pid1, (self#predicate_definition) _predicate_definition0)

  method clogicfunction : pid * logic_function_definition -> component = 
  function (_pid1, _logic_function_definition0) -> 
      CLogicFunction ((self#pid) _pid1, (self#logic_function_definition) _logic_function_definition0)

  method logic_function_definition : logic_function_definition -> logic_function_definition = function
  | LFInductive (_lfi_abs0) ->
      self#lfinductive (_lfi_abs0)
  | LFAbbrev (_lformula0) ->
      self#lfabbrev (_lformula0)

  method lfinductive : opaque_lfi_abs -> logic_function_definition = 
  function (_lfi_abs0) -> 
      LFInductive (create_lfi_abs (self#lfi_abs (open_lfi_abs _lfi_abs0)))

  method lfabbrev : lformula -> logic_function_definition = 
  function (_lformula0) -> 
      LFAbbrev ((self#lformula) _lformula0)

  method logic_clause : logic_clause -> logic_clause = function
  (_logic_clause_abs0) ->
    (create_logic_clause_abs (self#logic_clause_abs (open_logic_clause_abs _logic_clause_abs0)))

  method fact_status : fact_status -> fact_status = function
  | Lemma (_x0) ->
      self#lemma (_x0)
  | Axiom (_x0) ->
      self#axiom (_x0)

  method lemma : ( UniqueIdentifier.t ) -> fact_status = 
  function (_x0) -> 
      Lemma (_x0)

  method axiom : ( UniqueIdentifier.t ) -> fact_status = 
  function (_x0) -> 
      Axiom (_x0)

  method id : id -> id = function
  (_var0) ->
    (_var0)

  method ids : ids -> ids = function
  (_ids0) ->
    (List.map (self#id) _ids0)

  method tid : tid -> tid = function
  (_var0) ->
    (_var0)

  method pid : pid -> pid = function
  (_var0) ->
    (_var0)

  method term : term -> term = function
  | EId (_var0) ->
      self#eid (_var0)
  | EKApp (_var1, _lterms0) ->
      self#ekapp (_var1, _lterms0)
  | EApp (_lterm2, _lformulas1, _lterms0) ->
      self#eapp (_lterm2, _lformulas1, _lterms0)
  | ELam (_fun_abs0) ->
      self#elam (_fun_abs0)
  | ELet (_let_abs0) ->
      self#elet (_let_abs0)
  | ELetRec (_letrec_abs0) ->
      self#eletrec (_letrec_abs0)
  | ECase (_lterm1, _clauses0) ->
      self#ecase (_lterm1, _clauses0)
  | EPrimitive (_primitive0) ->
      self#eprimitive (_primitive0)
  | EAnnot (_lterm1, _term_type0) ->
      self#eannot (_lterm1, _term_type0)
  | EForallTys (_eforalltys_abs0) ->
      self#eforalltys (_eforalltys_abs0)
  | EExistsTys (_eexiststys_abs0) ->
      self#eexiststys (_eexiststys_abs0)
  | EProd (_lterms0) ->
      self#eprod (_lterms0)
  | EIf (_lterm2, _lterm1, _lterm0) ->
      self#eif (_lterm2, _lterm1, _lterm0)
  | EAbsurd ->
      self#eabsurd
  | EAssert (_lformula1, _lterm0) ->
      self#eassert (_lformula1, _lterm0)
  | ELetLogic (_let_logic_abs0) ->
      self#eletlogic (_let_logic_abs0)
  | EDeferred ->
      self#edeferred

  method eid : var -> term = 
  function (_var0) -> 
      EId (_var0)

  method ekapp : var * lterm list -> term = 
  function (_var1, _lterms0) -> 
      EKApp (_var1, List.map (self#lterm) _lterms0)

  method eapp : lterm * lformula list * lterm list -> term = 
  function (_lterm2, _lformulas1, _lterms0) -> 
      EApp ((self#lterm) _lterm2, List.map (self#lformula) _lformulas1, List.map (self#lterm) _lterms0)

  method elam : opaque_fun_abs -> term = 
  function (_fun_abs0) -> 
      ELam (create_fun_abs (self#fun_abs (open_fun_abs _fun_abs0)))

  method elet : opaque_let_abs -> term = 
  function (_let_abs0) -> 
      ELet (create_let_abs (self#let_abs (open_let_abs _let_abs0)))

  method eletrec : opaque_letrec_abs -> term = 
  function (_letrec_abs0) -> 
      ELetRec (create_letrec_abs (self#letrec_abs (open_letrec_abs _letrec_abs0)))

  method ecase : lterm * clause list -> term = 
  function (_lterm1, _clauses0) -> 
      ECase ((self#lterm) _lterm1, List.map (self#clause) _clauses0)

  method eprimitive : primitive -> term = 
  function (_primitive0) -> 
      EPrimitive ((self#primitive) _primitive0)

  method eannot : lterm * term_type -> term = 
  function (_lterm1, _term_type0) -> 
      EAnnot ((self#lterm) _lterm1, (self#term_type) _term_type0)

  method eforalltys : opaque_eforalltys_abs -> term = 
  function (_eforalltys_abs0) -> 
      EForallTys (create_eforalltys_abs (self#eforalltys_abs (open_eforalltys_abs _eforalltys_abs0)))

  method eexiststys : opaque_eexiststys_abs -> term = 
  function (_eexiststys_abs0) -> 
      EExistsTys (create_eexiststys_abs (self#eexiststys_abs (open_eexiststys_abs _eexiststys_abs0)))

  method eprod : lterm list -> term = 
  function (_lterms0) -> 
      EProd (List.map (self#lterm) _lterms0)

  method eif : lterm * lterm * lterm -> term = 
  function (_lterm2, _lterm1, _lterm0) -> 
      EIf ((self#lterm) _lterm2, (self#lterm) _lterm1, (self#lterm) _lterm0)

  method eabsurd : term = EAbsurd

  method eassert : lformula * lterm -> term = 
  function (_lformula1, _lterm0) -> 
      EAssert ((self#lformula) _lformula1, (self#lterm) _lterm0)

  method eletlogic : opaque_let_logic_abs -> term = 
  function (_let_logic_abs0) -> 
      ELetLogic (create_let_logic_abs (self#let_logic_abs (open_let_logic_abs _let_logic_abs0)))

  method edeferred : term = EDeferred

  method primitive : primitive -> primitive = function
  | PInt (_x0) ->
      self#pint (_x0)
  | PTrue ->
      self#ptrue
  | PFalse ->
      self#pfalse

  method pint : ( int ) -> primitive = 
  function (_x0) -> 
      PInt (_x0)

  method ptrue : primitive = PTrue

  method pfalse : primitive = PFalse

  method logic_bindings : logic_bindings -> logic_bindings = function
  (_bindings1, _optional_lformula0) ->
    ((self#bindings) _bindings1, (self#optional_lformula) _optional_lformula0)

  method logic_arguments : logic_arguments -> logic_arguments = function
  (_fbindings0) ->
    ((self#fbindings) _fbindings0)

  method bindings : bindings -> bindings = function
  (_bindings0) ->
    (List.map (self#binding) _bindings0)

  method binding : binding -> binding = function
  (_id1, _term_type0) ->
    ((self#id) _id1, (self#term_type) _term_type0)

  method function_output : function_output -> function_output = function
  (_function_output_abs0) ->
    (create_function_output_abs (self#function_output_abs (open_function_output_abs _function_output_abs0)))

  method clause : clause -> clause = function
  (_clause_abs0) ->
    (create_clause_abs (self#clause_abs (open_clause_abs _clause_abs0)))

  method pattern : pattern -> pattern = function
  | PVar (_id0) ->
      self#pvar (_id0)
  | PApp (_constructor1, _patterns0) ->
      self#papp (_constructor1, _patterns0)

  method pvar : id -> pattern = 
  function (_id0) -> 
      PVar ((self#id) _id0)

  method papp : constructor * pattern list -> pattern = 
  function (_constructor1, _patterns0) -> 
      PApp ((self#constructor) _constructor1, List.map (self#pattern) _patterns0)

  method constructor : constructor -> constructor = function
  (_var0) ->
    (_var0)

  method term_type : term_type -> term_type = function
  | TPrimitive (_primitive_type0) ->
      self#tprimitive (_primitive_type0)
  | TVar (_var0) ->
      self#tvar (_var0)
  | TArrow (_term_type1, _term_type0) ->
      self#tarrow (_term_type1, _term_type0)
  | TProd (_term_types0) ->
      self#tprod (_term_types0)
  | TApp (_var1, _term_types0) ->
      self#tapp (_var1, _term_types0)

  method tprimitive : primitive_type -> term_type = 
  function (_primitive_type0) -> 
      TPrimitive ((self#primitive_type) _primitive_type0)

  method tvar : var -> term_type = 
  function (_var0) -> 
      TVar (_var0)

  method tarrow : term_type * term_type -> term_type = 
  function (_term_type1, _term_type0) -> 
      TArrow ((self#term_type) _term_type1, (self#term_type) _term_type0)

  method tprod : term_type list -> term_type = 
  function (_term_types0) -> 
      TProd (List.map (self#term_type) _term_types0)

  method tapp : var * term_type list -> term_type = 
  function (_var1, _term_types0) -> 
      TApp (_var1, List.map (self#term_type) _term_types0)

  method primitive_type : primitive_type -> primitive_type = function
  | TInt ->
      self#tint
  | TBool ->
      self#tbool
  | TUnit ->
      self#tunit

  method tint : primitive_type = TInt

  method tbool : primitive_type = TBool

  method tunit : primitive_type = TUnit

  method type_scheme : type_scheme -> type_scheme = function
  | TScheme (_scheme_abs0) ->
      self#tscheme (_scheme_abs0)

  method tscheme : opaque_scheme_abs -> type_scheme = 
  function (_scheme_abs0) -> 
      TScheme (create_scheme_abs (self#scheme_abs (open_scheme_abs _scheme_abs0)))

  method type_parameters : type_parameters -> type_parameters = function
  (_type_parameters0) ->
    (List.map (self#type_parameter) _type_parameters0)

  method type_parameter : type_parameter -> type_parameter = function
  (_var0) ->
    (_var0)

  method kind : kind -> kind = function
  | KStar ->
      self#kstar
  | KArrow (_kinds0) ->
      self#karrow (_kinds0)

  method kstar : kind = KStar

  method karrow : kind list -> kind = 
  function (_kinds0) -> 
      KArrow (List.map (self#kind) _kinds0)

  method type_definition : type_definition -> type_definition = function
  | DAlgebraic (_type_parameters1, _dataconstructor_definitions0) ->
      self#dalgebraic (_type_parameters1, _dataconstructor_definitions0)
  | DeferredType ->
      self#deferredtype

  method dalgebraic : type_parameters * dataconstructor_definition list -> type_definition = 
  function (_type_parameters1, _dataconstructor_definitions0) -> 
      DAlgebraic ((self#type_parameters) _type_parameters1, List.map (self#dataconstructor_definition) _dataconstructor_definitions0)

  method deferredtype : type_definition = DeferredType

  method dataconstructor_definition : dataconstructor_definition -> dataconstructor_definition = function
  (_var1, _term_type0) ->
    (_var1, (self#term_type) _term_type0)

  method optional_lformula : optional_lformula -> optional_lformula = function
  | ImplicitFormula (_x0) ->
      self#implicitformula (_x0)
  | ExplicitFormula (_lformula0) ->
      self#explicitformula (_lformula0)

  method implicitformula : ( Position.t ) -> optional_lformula = 
  function (_x0) -> 
      ImplicitFormula (_x0)

  method explicitformula : lformula -> optional_lformula = 
  function (_lformula0) -> 
      ExplicitFormula ((self#lformula) _lformula0)

  method formula : formula -> formula = function
  | FTrue ->
      self#ftrue
  | FFalse ->
      self#ffalse
  | FVar (_var0) ->
      self#fvar (_var0)
  | FLam (_lfun_abs0) ->
      self#flam (_lfun_abs0)
  | FForallTys (_fforalltys_abs0) ->
      self#fforalltys (_fforalltys_abs0)
  | FExistsTys (_fexiststys_abs0) ->
      self#fexiststys (_fexiststys_abs0)
  | FForall (_lforall_abs0) ->
      self#fforall (_lforall_abs0)
  | FExists (_lexists_abs0) ->
      self#fexists (_lexists_abs0)
  | FEq (_lformula1, _lformula0) ->
      self#feq (_lformula1, _lformula0)
  | FApp (_lformula1, _lformulas0) ->
      self#fapp (_lformula1, _lformulas0)
  | FKApp (_var1, _lformulas0) ->
      self#fkapp (_var1, _lformulas0)
  | FProd (_lformulas0) ->
      self#fprod (_lformulas0)
  | FPrimitive (_formula_primitive0) ->
      self#fprimitive (_formula_primitive0)
  | FAnnot (_lformula1, _formula_type0) ->
      self#fannot (_lformula1, _formula_type0)
  | FDeferred ->
      self#fdeferred

  method ftrue : formula = FTrue

  method ffalse : formula = FFalse

  method fvar : var -> formula = 
  function (_var0) -> 
      FVar (_var0)

  method flam : opaque_lfun_abs -> formula = 
  function (_lfun_abs0) -> 
      FLam (create_lfun_abs (self#lfun_abs (open_lfun_abs _lfun_abs0)))

  method fforalltys : opaque_fforalltys_abs -> formula = 
  function (_fforalltys_abs0) -> 
      FForallTys (create_fforalltys_abs (self#fforalltys_abs (open_fforalltys_abs _fforalltys_abs0)))

  method fexiststys : opaque_fexiststys_abs -> formula = 
  function (_fexiststys_abs0) -> 
      FExistsTys (create_fexiststys_abs (self#fexiststys_abs (open_fexiststys_abs _fexiststys_abs0)))

  method fforall : opaque_lforall_abs -> formula = 
  function (_lforall_abs0) -> 
      FForall (create_lforall_abs (self#lforall_abs (open_lforall_abs _lforall_abs0)))

  method fexists : opaque_lexists_abs -> formula = 
  function (_lexists_abs0) -> 
      FExists (create_lexists_abs (self#lexists_abs (open_lexists_abs _lexists_abs0)))

  method feq : lformula * lformula -> formula = 
  function (_lformula1, _lformula0) -> 
      FEq ((self#lformula) _lformula1, (self#lformula) _lformula0)

  method fapp : lformula * lformula list -> formula = 
  function (_lformula1, _lformulas0) -> 
      FApp ((self#lformula) _lformula1, List.map (self#lformula) _lformulas0)

  method fkapp : var * lformula list -> formula = 
  function (_var1, _lformulas0) -> 
      FKApp (_var1, List.map (self#lformula) _lformulas0)

  method fprod : lformula list -> formula = 
  function (_lformulas0) -> 
      FProd (List.map (self#lformula) _lformulas0)

  method fprimitive : formula_primitive -> formula = 
  function (_formula_primitive0) -> 
      FPrimitive ((self#formula_primitive) _formula_primitive0)

  method fannot : lformula * formula_type -> formula = 
  function (_lformula1, _formula_type0) -> 
      FAnnot ((self#lformula) _lformula1, (self#formula_type) _formula_type0)

  method fdeferred : formula = FDeferred

  method formula_primitive : formula_primitive -> formula_primitive = function
  | Pre ->
      self#pre
  | Post ->
      self#post
  | PAnd ->
      self#pand
  | POr ->
      self#por
  | PEquiv ->
      self#pequiv
  | PImply ->
      self#pimply
  | PNot ->
      self#pnot
  | PLessThan ->
      self#plessthan
  | PGreaterThan ->
      self#pgreaterthan
  | PLessEqualThan ->
      self#plessequalthan
  | PGreaterEqualThan ->
      self#pgreaterequalthan
  | PAdd ->
      self#padd
  | PSub ->
      self#psub
  | PMult ->
      self#pmult
  | PDiv ->
      self#pdiv
  | PNeg ->
      self#pneg
  | PEPrimitive (_primitive0) ->
      self#peprimitive (_primitive0)

  method pre : formula_primitive = Pre

  method post : formula_primitive = Post

  method pand : formula_primitive = PAnd

  method por : formula_primitive = POr

  method pequiv : formula_primitive = PEquiv

  method pimply : formula_primitive = PImply

  method pnot : formula_primitive = PNot

  method plessthan : formula_primitive = PLessThan

  method pgreaterthan : formula_primitive = PGreaterThan

  method plessequalthan : formula_primitive = PLessEqualThan

  method pgreaterequalthan : formula_primitive = PGreaterEqualThan

  method padd : formula_primitive = PAdd

  method psub : formula_primitive = PSub

  method pmult : formula_primitive = PMult

  method pdiv : formula_primitive = PDiv

  method pneg : formula_primitive = PNeg

  method peprimitive : primitive -> formula_primitive = 
  function (_primitive0) -> 
      PEPrimitive ((self#primitive) _primitive0)

  method fbindings : fbindings -> fbindings = function
  (_fbindings0) ->
    (List.map (self#fbinding) _fbindings0)

  method fbinding : fbinding -> fbinding = function
  (_var1, _formula_type0) ->
    (_var1, (self#formula_type) _formula_type0)

  method trigger : trigger -> trigger = function
  (_var0) ->
    (_var0)

  method formula_type : formula_type -> formula_type = function
  | FTProp ->
      self#ftprop
  | FTVar (_var0) ->
      self#ftvar (_var0)
  | FTArrow (_formula_type1, _formula_type0) ->
      self#ftarrow (_formula_type1, _formula_type0)
  | FTCArrow (_formula_type1, _formula_type0) ->
      self#ftcarrow (_formula_type1, _formula_type0)
  | FTProd (_formula_types0) ->
      self#ftprod (_formula_types0)
  | FTPrimitive (_primitive_type0) ->
      self#ftprimitive (_primitive_type0)
  | FTApp (_var1, _formula_types0) ->
      self#ftapp (_var1, _formula_types0)

  method ftprop : formula_type = FTProp

  method ftvar : var -> formula_type = 
  function (_var0) -> 
      FTVar (_var0)

  method ftarrow : formula_type * formula_type -> formula_type = 
  function (_formula_type1, _formula_type0) -> 
      FTArrow ((self#formula_type) _formula_type1, (self#formula_type) _formula_type0)

  method ftcarrow : formula_type * formula_type -> formula_type = 
  function (_formula_type1, _formula_type0) -> 
      FTCArrow ((self#formula_type) _formula_type1, (self#formula_type) _formula_type0)

  method ftprod : formula_type list -> formula_type = 
  function (_formula_types0) -> 
      FTProd (List.map (self#formula_type) _formula_types0)

  method ftprimitive : primitive_type -> formula_type = 
  function (_primitive_type0) -> 
      FTPrimitive ((self#primitive_type) _primitive_type0)

  method ftapp : var * formula_type list -> formula_type = 
  function (_var1, _formula_types0) -> 
      FTApp (_var1, List.map (self#formula_type) _formula_types0)

  method formula_type_scheme : formula_type_scheme -> formula_type_scheme = function
  | FTScheme (_formula_scheme_abs0) ->
      self#ftscheme (_formula_scheme_abs0)

  method ftscheme : opaque_formula_scheme_abs -> formula_type_scheme = 
  function (_formula_scheme_abs0) -> 
      FTScheme (create_formula_scheme_abs (self#formula_scheme_abs (open_formula_scheme_abs _formula_scheme_abs0)))

  method predicate_definition : predicate_definition -> predicate_definition = function
  | PDAbbrev (_lformula0) ->
      self#pdabbrev (_lformula0)
  | PDInductive (_formula_type_schemes1, _lformulas0) ->
      self#pdinductive (_formula_type_schemes1, _lformulas0)

  method pdabbrev : lformula -> predicate_definition = 
  function (_lformula0) -> 
      PDAbbrev ((self#lformula) _lformula0)

  method pdinductive : formula_type_scheme option * lformula list -> predicate_definition = 
  function (_formula_type_schemes1, _lformulas0) -> 
      PDInductive (option_map (self#formula_type_scheme) _formula_type_schemes1, List.map (self#lformula) _lformulas0)

  method lterm : lterm -> lterm = function
  { tpos = _x1; tvalue = _term0 } ->
    { tpos = self#tpos _x1; tvalue = self#tvalue _term0 }

  method tpos : ( Position.t ) -> ( Position.t ) = 
  function _x0 -> _x0

  method tvalue : term -> term = 
  function _term0 -> (self#term) _term0

  method lformula : lformula -> lformula = function
  { fpos = _x1; fvalue = _formula0 } ->
    { fpos = self#fpos _x1; fvalue = self#fvalue _formula0 }

  method fpos : ( Position.t ) -> ( Position.t ) = 
  function _x0 -> _x0

  method fvalue : formula -> formula = 
  function _formula0 -> (self#formula) _formula0

  method pcons_abs : pcons_abs -> pcons_abs = function
  (_x2, _component1, _program0) ->
    (_x2, (self#component) _component1, (self#program) _program0)

  method lfi_abs : lfi_abs -> lfi_abs = function
  (_ids1, _logic_clauses0) ->
    ((self#ids) _ids1, List.map (self#logic_clause) _logic_clauses0)

  method logic_clause_abs : logic_clause_abs -> logic_clause_abs = function
  (_pattern1, _lformula0) ->
    ((self#pattern) _pattern1, (self#lformula) _lformula0)

  method fun_abs : fun_abs -> fun_abs = function
  (_logic_arguments3, _logic_bindings2, _function_output1, _lterm0) ->
    ((self#logic_arguments) _logic_arguments3, (self#logic_bindings) _logic_bindings2, (self#function_output) _function_output1, (self#lterm) _lterm0)

  method let_abs : let_abs -> let_abs = function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
    ((self#ids) _ids3, (self#optional_lformula) _optional_lformula2, (self#lterm) _lterm1, (self#lterm) _lterm0)

  method letrec_abs : letrec_abs -> letrec_abs = function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
    ((self#ids) _ids3, (self#optional_lformula) _optional_lformula2, (self#lterm) _lterm1, (self#lterm) _lterm0)

  method eforalltys_abs : eforalltys_abs -> eforalltys_abs = function
  (_type_parameters1, _lterm0) ->
    ((self#type_parameters) _type_parameters1, (self#lterm) _lterm0)

  method eexiststys_abs : eexiststys_abs -> eexiststys_abs = function
  (_type_parameters1, _lterm0) ->
    ((self#type_parameters) _type_parameters1, (self#lterm) _lterm0)

  method let_logic_abs : let_logic_abs -> let_logic_abs = function
  (_fbindings2, _lformula1, _lterm0) ->
    ((self#fbindings) _fbindings2, (self#lformula) _lformula1, (self#lterm) _lterm0)

  method function_output_abs : function_output_abs -> function_output_abs = function
  (_logic_bindings0) ->
    ((self#logic_bindings) _logic_bindings0)

  method clause_abs : clause_abs -> clause_abs = function
  (_pattern1, _lterm0) ->
    ((self#pattern) _pattern1, (self#lterm) _lterm0)

  method scheme_abs : scheme_abs -> scheme_abs = function
  (_type_parameters1, _term_type0) ->
    ((self#type_parameters) _type_parameters1, (self#term_type) _term_type0)

  method lfun_abs : lfun_abs -> lfun_abs = function
  (_fbindings1, _lformula0) ->
    ((self#fbindings) _fbindings1, (self#lformula) _lformula0)

  method fforalltys_abs : fforalltys_abs -> fforalltys_abs = function
  (_type_parameters1, _lformula0) ->
    ((self#type_parameters) _type_parameters1, (self#lformula) _lformula0)

  method fexiststys_abs : fexiststys_abs -> fexiststys_abs = function
  (_type_parameters1, _lformula0) ->
    ((self#type_parameters) _type_parameters1, (self#lformula) _lformula0)

  method lforall_abs : lforall_abs -> lforall_abs = function
  (_fbindings2, _triggers1, _lformula0) ->
    ((self#fbindings) _fbindings2, List.map (self#trigger) _triggers1, (self#lformula) _lformula0)

  method lexists_abs : lexists_abs -> lexists_abs = function
  (_fbindings1, _lformula0) ->
    ((self#fbindings) _fbindings1, (self#lformula) _lformula0)

  method formula_scheme_abs : formula_scheme_abs -> formula_scheme_abs = function
  (_type_parameters1, _formula_type0) ->
    ((self#type_parameters) _type_parameters1, (self#formula_type) _formula_type0)

end

class [ 'accumulator ] fold = object(self)

  method program : 'accumulator -> program -> 'accumulator = fun accu -> function
  | PEmpty (_x0) ->
      self#pempty accu (_x0)
  | PConsComponent (_pcons_abs0) ->
      self#pconscomponent accu (_pcons_abs0)

  method pempty : 'accumulator -> ( Position.t ) -> 'accumulator = fun accu -> 
  function (_x0) -> 
      accu

  method pconscomponent : 'accumulator -> opaque_pcons_abs -> 'accumulator = fun accu -> 
  function (_pcons_abs0) -> 
      let accu = self#pcons_abs accu (open_pcons_abs _pcons_abs0) in
      accu

  method component : 'accumulator -> component -> 'accumulator = fun accu -> function
  | CValue (_ids2, _optional_lformula1, _lterm0) ->
      self#cvalue accu (_ids2, _optional_lformula1, _lterm0)
  | CRecValue (_ids2, _optional_lformula1, _lterm0) ->
      self#crecvalue accu (_ids2, _optional_lformula1, _lterm0)
  | CTypeDef (_tid2, _kind1, _type_definition0) ->
      self#ctypedef accu (_tid2, _kind1, _type_definition0)
  | CFact (_fact_status1, _lformula0) ->
      self#cfact accu (_fact_status1, _lformula0)
  | CPredicate (_pid1, _predicate_definition0) ->
      self#cpredicate accu (_pid1, _predicate_definition0)
  | CLogicFunction (_pid1, _logic_function_definition0) ->
      self#clogicfunction accu (_pid1, _logic_function_definition0)

  method cvalue : 'accumulator -> ids * optional_lformula * lterm -> 'accumulator = fun accu -> 
  function (_ids2, _optional_lformula1, _lterm0) -> 
      let accu = (self#ids) accu _ids2 in
      let accu = (self#optional_lformula) accu _optional_lformula1 in
      let accu = (self#lterm) accu _lterm0 in
      accu

  method crecvalue : 'accumulator -> ids * optional_lformula * lterm -> 'accumulator = fun accu -> 
  function (_ids2, _optional_lformula1, _lterm0) -> 
      let accu = (self#ids) accu _ids2 in
      let accu = (self#optional_lformula) accu _optional_lformula1 in
      let accu = (self#lterm) accu _lterm0 in
      accu

  method ctypedef : 'accumulator -> tid * kind * type_definition -> 'accumulator = fun accu -> 
  function (_tid2, _kind1, _type_definition0) -> 
      let accu = (self#tid) accu _tid2 in
      let accu = (self#kind) accu _kind1 in
      let accu = (self#type_definition) accu _type_definition0 in
      accu

  method cfact : 'accumulator -> fact_status * lformula -> 'accumulator = fun accu -> 
  function (_fact_status1, _lformula0) -> 
      let accu = (self#fact_status) accu _fact_status1 in
      let accu = (self#lformula) accu _lformula0 in
      accu

  method cpredicate : 'accumulator -> pid * predicate_definition -> 'accumulator = fun accu -> 
  function (_pid1, _predicate_definition0) -> 
      let accu = (self#pid) accu _pid1 in
      let accu = (self#predicate_definition) accu _predicate_definition0 in
      accu

  method clogicfunction : 'accumulator -> pid * logic_function_definition -> 'accumulator = fun accu -> 
  function (_pid1, _logic_function_definition0) -> 
      let accu = (self#pid) accu _pid1 in
      let accu = (self#logic_function_definition) accu _logic_function_definition0 in
      accu

  method logic_function_definition : 'accumulator -> logic_function_definition -> 'accumulator = fun accu -> function
  | LFInductive (_lfi_abs0) ->
      self#lfinductive accu (_lfi_abs0)
  | LFAbbrev (_lformula0) ->
      self#lfabbrev accu (_lformula0)

  method lfinductive : 'accumulator -> opaque_lfi_abs -> 'accumulator = fun accu -> 
  function (_lfi_abs0) -> 
      let accu = self#lfi_abs accu (open_lfi_abs _lfi_abs0) in
      accu

  method lfabbrev : 'accumulator -> lformula -> 'accumulator = fun accu -> 
  function (_lformula0) -> 
      let accu = (self#lformula) accu _lformula0 in
      accu

  method logic_clause : 'accumulator -> logic_clause -> 'accumulator = fun accu -> function
  (_logic_clause_abs0) ->
      let accu = self#logic_clause_abs accu (open_logic_clause_abs _logic_clause_abs0) in
      accu

  method fact_status : 'accumulator -> fact_status -> 'accumulator = fun accu -> function
  | Lemma (_x0) ->
      self#lemma accu (_x0)
  | Axiom (_x0) ->
      self#axiom accu (_x0)

  method lemma : 'accumulator -> ( UniqueIdentifier.t ) -> 'accumulator = fun accu -> 
  function (_x0) -> 
      accu

  method axiom : 'accumulator -> ( UniqueIdentifier.t ) -> 'accumulator = fun accu -> 
  function (_x0) -> 
      accu

  method id : 'accumulator -> id -> 'accumulator = fun accu -> function
  (_var0) ->
      accu

  method ids : 'accumulator -> ids -> 'accumulator = fun accu -> function
  (_ids0) ->
      let accu = List.fold_left (self#id) accu _ids0 in
      accu

  method tid : 'accumulator -> tid -> 'accumulator = fun accu -> function
  (_var0) ->
      accu

  method pid : 'accumulator -> pid -> 'accumulator = fun accu -> function
  (_var0) ->
      accu

  method term : 'accumulator -> term -> 'accumulator = fun accu -> function
  | EId (_var0) ->
      self#eid accu (_var0)
  | EKApp (_var1, _lterms0) ->
      self#ekapp accu (_var1, _lterms0)
  | EApp (_lterm2, _lformulas1, _lterms0) ->
      self#eapp accu (_lterm2, _lformulas1, _lterms0)
  | ELam (_fun_abs0) ->
      self#elam accu (_fun_abs0)
  | ELet (_let_abs0) ->
      self#elet accu (_let_abs0)
  | ELetRec (_letrec_abs0) ->
      self#eletrec accu (_letrec_abs0)
  | ECase (_lterm1, _clauses0) ->
      self#ecase accu (_lterm1, _clauses0)
  | EPrimitive (_primitive0) ->
      self#eprimitive accu (_primitive0)
  | EAnnot (_lterm1, _term_type0) ->
      self#eannot accu (_lterm1, _term_type0)
  | EForallTys (_eforalltys_abs0) ->
      self#eforalltys accu (_eforalltys_abs0)
  | EExistsTys (_eexiststys_abs0) ->
      self#eexiststys accu (_eexiststys_abs0)
  | EProd (_lterms0) ->
      self#eprod accu (_lterms0)
  | EIf (_lterm2, _lterm1, _lterm0) ->
      self#eif accu (_lterm2, _lterm1, _lterm0)
  | EAbsurd ->
      self#eabsurd accu
  | EAssert (_lformula1, _lterm0) ->
      self#eassert accu (_lformula1, _lterm0)
  | ELetLogic (_let_logic_abs0) ->
      self#eletlogic accu (_let_logic_abs0)
  | EDeferred ->
      self#edeferred accu

  method eid : 'accumulator -> var -> 'accumulator = fun accu -> 
  function (_var0) -> 
      accu

  method ekapp : 'accumulator -> var * lterm list -> 'accumulator = fun accu -> 
  function (_var1, _lterms0) -> 
      let accu = List.fold_left (self#lterm) accu _lterms0 in
      accu

  method eapp : 'accumulator -> lterm * lformula list * lterm list -> 'accumulator = fun accu -> 
  function (_lterm2, _lformulas1, _lterms0) -> 
      let accu = (self#lterm) accu _lterm2 in
      let accu = List.fold_left (self#lformula) accu _lformulas1 in
      let accu = List.fold_left (self#lterm) accu _lterms0 in
      accu

  method elam : 'accumulator -> opaque_fun_abs -> 'accumulator = fun accu -> 
  function (_fun_abs0) -> 
      let accu = self#fun_abs accu (open_fun_abs _fun_abs0) in
      accu

  method elet : 'accumulator -> opaque_let_abs -> 'accumulator = fun accu -> 
  function (_let_abs0) -> 
      let accu = self#let_abs accu (open_let_abs _let_abs0) in
      accu

  method eletrec : 'accumulator -> opaque_letrec_abs -> 'accumulator = fun accu -> 
  function (_letrec_abs0) -> 
      let accu = self#letrec_abs accu (open_letrec_abs _letrec_abs0) in
      accu

  method ecase : 'accumulator -> lterm * clause list -> 'accumulator = fun accu -> 
  function (_lterm1, _clauses0) -> 
      let accu = (self#lterm) accu _lterm1 in
      let accu = List.fold_left (self#clause) accu _clauses0 in
      accu

  method eprimitive : 'accumulator -> primitive -> 'accumulator = fun accu -> 
  function (_primitive0) -> 
      let accu = (self#primitive) accu _primitive0 in
      accu

  method eannot : 'accumulator -> lterm * term_type -> 'accumulator = fun accu -> 
  function (_lterm1, _term_type0) -> 
      let accu = (self#lterm) accu _lterm1 in
      let accu = (self#term_type) accu _term_type0 in
      accu

  method eforalltys : 'accumulator -> opaque_eforalltys_abs -> 'accumulator = fun accu -> 
  function (_eforalltys_abs0) -> 
      let accu = self#eforalltys_abs accu (open_eforalltys_abs _eforalltys_abs0) in
      accu

  method eexiststys : 'accumulator -> opaque_eexiststys_abs -> 'accumulator = fun accu -> 
  function (_eexiststys_abs0) -> 
      let accu = self#eexiststys_abs accu (open_eexiststys_abs _eexiststys_abs0) in
      accu

  method eprod : 'accumulator -> lterm list -> 'accumulator = fun accu -> 
  function (_lterms0) -> 
      let accu = List.fold_left (self#lterm) accu _lterms0 in
      accu

  method eif : 'accumulator -> lterm * lterm * lterm -> 'accumulator = fun accu -> 
  function (_lterm2, _lterm1, _lterm0) -> 
      let accu = (self#lterm) accu _lterm2 in
      let accu = (self#lterm) accu _lterm1 in
      let accu = (self#lterm) accu _lterm0 in
      accu

  method eabsurd : 'accumulator -> 'accumulator = fun accu ->       accu

  method eassert : 'accumulator -> lformula * lterm -> 'accumulator = fun accu -> 
  function (_lformula1, _lterm0) -> 
      let accu = (self#lformula) accu _lformula1 in
      let accu = (self#lterm) accu _lterm0 in
      accu

  method eletlogic : 'accumulator -> opaque_let_logic_abs -> 'accumulator = fun accu -> 
  function (_let_logic_abs0) -> 
      let accu = self#let_logic_abs accu (open_let_logic_abs _let_logic_abs0) in
      accu

  method edeferred : 'accumulator -> 'accumulator = fun accu ->       accu

  method primitive : 'accumulator -> primitive -> 'accumulator = fun accu -> function
  | PInt (_x0) ->
      self#pint accu (_x0)
  | PTrue ->
      self#ptrue accu
  | PFalse ->
      self#pfalse accu

  method pint : 'accumulator -> ( int ) -> 'accumulator = fun accu -> 
  function (_x0) -> 
      accu

  method ptrue : 'accumulator -> 'accumulator = fun accu ->       accu

  method pfalse : 'accumulator -> 'accumulator = fun accu ->       accu

  method logic_bindings : 'accumulator -> logic_bindings -> 'accumulator = fun accu -> function
  (_bindings1, _optional_lformula0) ->
      let accu = (self#bindings) accu _bindings1 in
      let accu = (self#optional_lformula) accu _optional_lformula0 in
      accu

  method logic_arguments : 'accumulator -> logic_arguments -> 'accumulator = fun accu -> function
  (_fbindings0) ->
      let accu = (self#fbindings) accu _fbindings0 in
      accu

  method bindings : 'accumulator -> bindings -> 'accumulator = fun accu -> function
  (_bindings0) ->
      let accu = List.fold_left (self#binding) accu _bindings0 in
      accu

  method binding : 'accumulator -> binding -> 'accumulator = fun accu -> function
  (_id1, _term_type0) ->
      let accu = (self#id) accu _id1 in
      let accu = (self#term_type) accu _term_type0 in
      accu

  method function_output : 'accumulator -> function_output -> 'accumulator = fun accu -> function
  (_function_output_abs0) ->
      let accu = self#function_output_abs accu (open_function_output_abs _function_output_abs0) in
      accu

  method clause : 'accumulator -> clause -> 'accumulator = fun accu -> function
  (_clause_abs0) ->
      let accu = self#clause_abs accu (open_clause_abs _clause_abs0) in
      accu

  method pattern : 'accumulator -> pattern -> 'accumulator = fun accu -> function
  | PVar (_id0) ->
      self#pvar accu (_id0)
  | PApp (_constructor1, _patterns0) ->
      self#papp accu (_constructor1, _patterns0)

  method pvar : 'accumulator -> id -> 'accumulator = fun accu -> 
  function (_id0) -> 
      let accu = (self#id) accu _id0 in
      accu

  method papp : 'accumulator -> constructor * pattern list -> 'accumulator = fun accu -> 
  function (_constructor1, _patterns0) -> 
      let accu = (self#constructor) accu _constructor1 in
      let accu = List.fold_left (self#pattern) accu _patterns0 in
      accu

  method constructor : 'accumulator -> constructor -> 'accumulator = fun accu -> function
  (_var0) ->
      accu

  method term_type : 'accumulator -> term_type -> 'accumulator = fun accu -> function
  | TPrimitive (_primitive_type0) ->
      self#tprimitive accu (_primitive_type0)
  | TVar (_var0) ->
      self#tvar accu (_var0)
  | TArrow (_term_type1, _term_type0) ->
      self#tarrow accu (_term_type1, _term_type0)
  | TProd (_term_types0) ->
      self#tprod accu (_term_types0)
  | TApp (_var1, _term_types0) ->
      self#tapp accu (_var1, _term_types0)

  method tprimitive : 'accumulator -> primitive_type -> 'accumulator = fun accu -> 
  function (_primitive_type0) -> 
      let accu = (self#primitive_type) accu _primitive_type0 in
      accu

  method tvar : 'accumulator -> var -> 'accumulator = fun accu -> 
  function (_var0) -> 
      accu

  method tarrow : 'accumulator -> term_type * term_type -> 'accumulator = fun accu -> 
  function (_term_type1, _term_type0) -> 
      let accu = (self#term_type) accu _term_type1 in
      let accu = (self#term_type) accu _term_type0 in
      accu

  method tprod : 'accumulator -> term_type list -> 'accumulator = fun accu -> 
  function (_term_types0) -> 
      let accu = List.fold_left (self#term_type) accu _term_types0 in
      accu

  method tapp : 'accumulator -> var * term_type list -> 'accumulator = fun accu -> 
  function (_var1, _term_types0) -> 
      let accu = List.fold_left (self#term_type) accu _term_types0 in
      accu

  method primitive_type : 'accumulator -> primitive_type -> 'accumulator = fun accu -> function
  | TInt ->
      self#tint accu
  | TBool ->
      self#tbool accu
  | TUnit ->
      self#tunit accu

  method tint : 'accumulator -> 'accumulator = fun accu ->       accu

  method tbool : 'accumulator -> 'accumulator = fun accu ->       accu

  method tunit : 'accumulator -> 'accumulator = fun accu ->       accu

  method type_scheme : 'accumulator -> type_scheme -> 'accumulator = fun accu -> function
  | TScheme (_scheme_abs0) ->
      self#tscheme accu (_scheme_abs0)

  method tscheme : 'accumulator -> opaque_scheme_abs -> 'accumulator = fun accu -> 
  function (_scheme_abs0) -> 
      let accu = self#scheme_abs accu (open_scheme_abs _scheme_abs0) in
      accu

  method type_parameters : 'accumulator -> type_parameters -> 'accumulator = fun accu -> function
  (_type_parameters0) ->
      let accu = List.fold_left (self#type_parameter) accu _type_parameters0 in
      accu

  method type_parameter : 'accumulator -> type_parameter -> 'accumulator = fun accu -> function
  (_var0) ->
      accu

  method kind : 'accumulator -> kind -> 'accumulator = fun accu -> function
  | KStar ->
      self#kstar accu
  | KArrow (_kinds0) ->
      self#karrow accu (_kinds0)

  method kstar : 'accumulator -> 'accumulator = fun accu ->       accu

  method karrow : 'accumulator -> kind list -> 'accumulator = fun accu -> 
  function (_kinds0) -> 
      let accu = List.fold_left (self#kind) accu _kinds0 in
      accu

  method type_definition : 'accumulator -> type_definition -> 'accumulator = fun accu -> function
  | DAlgebraic (_type_parameters1, _dataconstructor_definitions0) ->
      self#dalgebraic accu (_type_parameters1, _dataconstructor_definitions0)
  | DeferredType ->
      self#deferredtype accu

  method dalgebraic : 'accumulator -> type_parameters * dataconstructor_definition list -> 'accumulator = fun accu -> 
  function (_type_parameters1, _dataconstructor_definitions0) -> 
      let accu = (self#type_parameters) accu _type_parameters1 in
      let accu = List.fold_left (self#dataconstructor_definition) accu _dataconstructor_definitions0 in
      accu

  method deferredtype : 'accumulator -> 'accumulator = fun accu ->       accu

  method dataconstructor_definition : 'accumulator -> dataconstructor_definition -> 'accumulator = fun accu -> function
  (_var1, _term_type0) ->
      let accu = (self#term_type) accu _term_type0 in
      accu

  method optional_lformula : 'accumulator -> optional_lformula -> 'accumulator = fun accu -> function
  | ImplicitFormula (_x0) ->
      self#implicitformula accu (_x0)
  | ExplicitFormula (_lformula0) ->
      self#explicitformula accu (_lformula0)

  method implicitformula : 'accumulator -> ( Position.t ) -> 'accumulator = fun accu -> 
  function (_x0) -> 
      accu

  method explicitformula : 'accumulator -> lformula -> 'accumulator = fun accu -> 
  function (_lformula0) -> 
      let accu = (self#lformula) accu _lformula0 in
      accu

  method formula : 'accumulator -> formula -> 'accumulator = fun accu -> function
  | FTrue ->
      self#ftrue accu
  | FFalse ->
      self#ffalse accu
  | FVar (_var0) ->
      self#fvar accu (_var0)
  | FLam (_lfun_abs0) ->
      self#flam accu (_lfun_abs0)
  | FForallTys (_fforalltys_abs0) ->
      self#fforalltys accu (_fforalltys_abs0)
  | FExistsTys (_fexiststys_abs0) ->
      self#fexiststys accu (_fexiststys_abs0)
  | FForall (_lforall_abs0) ->
      self#fforall accu (_lforall_abs0)
  | FExists (_lexists_abs0) ->
      self#fexists accu (_lexists_abs0)
  | FEq (_lformula1, _lformula0) ->
      self#feq accu (_lformula1, _lformula0)
  | FApp (_lformula1, _lformulas0) ->
      self#fapp accu (_lformula1, _lformulas0)
  | FKApp (_var1, _lformulas0) ->
      self#fkapp accu (_var1, _lformulas0)
  | FProd (_lformulas0) ->
      self#fprod accu (_lformulas0)
  | FPrimitive (_formula_primitive0) ->
      self#fprimitive accu (_formula_primitive0)
  | FAnnot (_lformula1, _formula_type0) ->
      self#fannot accu (_lformula1, _formula_type0)
  | FDeferred ->
      self#fdeferred accu

  method ftrue : 'accumulator -> 'accumulator = fun accu ->       accu

  method ffalse : 'accumulator -> 'accumulator = fun accu ->       accu

  method fvar : 'accumulator -> var -> 'accumulator = fun accu -> 
  function (_var0) -> 
      accu

  method flam : 'accumulator -> opaque_lfun_abs -> 'accumulator = fun accu -> 
  function (_lfun_abs0) -> 
      let accu = self#lfun_abs accu (open_lfun_abs _lfun_abs0) in
      accu

  method fforalltys : 'accumulator -> opaque_fforalltys_abs -> 'accumulator = fun accu -> 
  function (_fforalltys_abs0) -> 
      let accu = self#fforalltys_abs accu (open_fforalltys_abs _fforalltys_abs0) in
      accu

  method fexiststys : 'accumulator -> opaque_fexiststys_abs -> 'accumulator = fun accu -> 
  function (_fexiststys_abs0) -> 
      let accu = self#fexiststys_abs accu (open_fexiststys_abs _fexiststys_abs0) in
      accu

  method fforall : 'accumulator -> opaque_lforall_abs -> 'accumulator = fun accu -> 
  function (_lforall_abs0) -> 
      let accu = self#lforall_abs accu (open_lforall_abs _lforall_abs0) in
      accu

  method fexists : 'accumulator -> opaque_lexists_abs -> 'accumulator = fun accu -> 
  function (_lexists_abs0) -> 
      let accu = self#lexists_abs accu (open_lexists_abs _lexists_abs0) in
      accu

  method feq : 'accumulator -> lformula * lformula -> 'accumulator = fun accu -> 
  function (_lformula1, _lformula0) -> 
      let accu = (self#lformula) accu _lformula1 in
      let accu = (self#lformula) accu _lformula0 in
      accu

  method fapp : 'accumulator -> lformula * lformula list -> 'accumulator = fun accu -> 
  function (_lformula1, _lformulas0) -> 
      let accu = (self#lformula) accu _lformula1 in
      let accu = List.fold_left (self#lformula) accu _lformulas0 in
      accu

  method fkapp : 'accumulator -> var * lformula list -> 'accumulator = fun accu -> 
  function (_var1, _lformulas0) -> 
      let accu = List.fold_left (self#lformula) accu _lformulas0 in
      accu

  method fprod : 'accumulator -> lformula list -> 'accumulator = fun accu -> 
  function (_lformulas0) -> 
      let accu = List.fold_left (self#lformula) accu _lformulas0 in
      accu

  method fprimitive : 'accumulator -> formula_primitive -> 'accumulator = fun accu -> 
  function (_formula_primitive0) -> 
      let accu = (self#formula_primitive) accu _formula_primitive0 in
      accu

  method fannot : 'accumulator -> lformula * formula_type -> 'accumulator = fun accu -> 
  function (_lformula1, _formula_type0) -> 
      let accu = (self#lformula) accu _lformula1 in
      let accu = (self#formula_type) accu _formula_type0 in
      accu

  method fdeferred : 'accumulator -> 'accumulator = fun accu ->       accu

  method formula_primitive : 'accumulator -> formula_primitive -> 'accumulator = fun accu -> function
  | Pre ->
      self#pre accu
  | Post ->
      self#post accu
  | PAnd ->
      self#pand accu
  | POr ->
      self#por accu
  | PEquiv ->
      self#pequiv accu
  | PImply ->
      self#pimply accu
  | PNot ->
      self#pnot accu
  | PLessThan ->
      self#plessthan accu
  | PGreaterThan ->
      self#pgreaterthan accu
  | PLessEqualThan ->
      self#plessequalthan accu
  | PGreaterEqualThan ->
      self#pgreaterequalthan accu
  | PAdd ->
      self#padd accu
  | PSub ->
      self#psub accu
  | PMult ->
      self#pmult accu
  | PDiv ->
      self#pdiv accu
  | PNeg ->
      self#pneg accu
  | PEPrimitive (_primitive0) ->
      self#peprimitive accu (_primitive0)

  method pre : 'accumulator -> 'accumulator = fun accu ->       accu

  method post : 'accumulator -> 'accumulator = fun accu ->       accu

  method pand : 'accumulator -> 'accumulator = fun accu ->       accu

  method por : 'accumulator -> 'accumulator = fun accu ->       accu

  method pequiv : 'accumulator -> 'accumulator = fun accu ->       accu

  method pimply : 'accumulator -> 'accumulator = fun accu ->       accu

  method pnot : 'accumulator -> 'accumulator = fun accu ->       accu

  method plessthan : 'accumulator -> 'accumulator = fun accu ->       accu

  method pgreaterthan : 'accumulator -> 'accumulator = fun accu ->       accu

  method plessequalthan : 'accumulator -> 'accumulator = fun accu ->       accu

  method pgreaterequalthan : 'accumulator -> 'accumulator = fun accu ->       accu

  method padd : 'accumulator -> 'accumulator = fun accu ->       accu

  method psub : 'accumulator -> 'accumulator = fun accu ->       accu

  method pmult : 'accumulator -> 'accumulator = fun accu ->       accu

  method pdiv : 'accumulator -> 'accumulator = fun accu ->       accu

  method pneg : 'accumulator -> 'accumulator = fun accu ->       accu

  method peprimitive : 'accumulator -> primitive -> 'accumulator = fun accu -> 
  function (_primitive0) -> 
      let accu = (self#primitive) accu _primitive0 in
      accu

  method fbindings : 'accumulator -> fbindings -> 'accumulator = fun accu -> function
  (_fbindings0) ->
      let accu = List.fold_left (self#fbinding) accu _fbindings0 in
      accu

  method fbinding : 'accumulator -> fbinding -> 'accumulator = fun accu -> function
  (_var1, _formula_type0) ->
      let accu = (self#formula_type) accu _formula_type0 in
      accu

  method trigger : 'accumulator -> trigger -> 'accumulator = fun accu -> function
  (_var0) ->
      accu

  method formula_type : 'accumulator -> formula_type -> 'accumulator = fun accu -> function
  | FTProp ->
      self#ftprop accu
  | FTVar (_var0) ->
      self#ftvar accu (_var0)
  | FTArrow (_formula_type1, _formula_type0) ->
      self#ftarrow accu (_formula_type1, _formula_type0)
  | FTCArrow (_formula_type1, _formula_type0) ->
      self#ftcarrow accu (_formula_type1, _formula_type0)
  | FTProd (_formula_types0) ->
      self#ftprod accu (_formula_types0)
  | FTPrimitive (_primitive_type0) ->
      self#ftprimitive accu (_primitive_type0)
  | FTApp (_var1, _formula_types0) ->
      self#ftapp accu (_var1, _formula_types0)

  method ftprop : 'accumulator -> 'accumulator = fun accu ->       accu

  method ftvar : 'accumulator -> var -> 'accumulator = fun accu -> 
  function (_var0) -> 
      accu

  method ftarrow : 'accumulator -> formula_type * formula_type -> 'accumulator = fun accu -> 
  function (_formula_type1, _formula_type0) -> 
      let accu = (self#formula_type) accu _formula_type1 in
      let accu = (self#formula_type) accu _formula_type0 in
      accu

  method ftcarrow : 'accumulator -> formula_type * formula_type -> 'accumulator = fun accu -> 
  function (_formula_type1, _formula_type0) -> 
      let accu = (self#formula_type) accu _formula_type1 in
      let accu = (self#formula_type) accu _formula_type0 in
      accu

  method ftprod : 'accumulator -> formula_type list -> 'accumulator = fun accu -> 
  function (_formula_types0) -> 
      let accu = List.fold_left (self#formula_type) accu _formula_types0 in
      accu

  method ftprimitive : 'accumulator -> primitive_type -> 'accumulator = fun accu -> 
  function (_primitive_type0) -> 
      let accu = (self#primitive_type) accu _primitive_type0 in
      accu

  method ftapp : 'accumulator -> var * formula_type list -> 'accumulator = fun accu -> 
  function (_var1, _formula_types0) -> 
      let accu = List.fold_left (self#formula_type) accu _formula_types0 in
      accu

  method formula_type_scheme : 'accumulator -> formula_type_scheme -> 'accumulator = fun accu -> function
  | FTScheme (_formula_scheme_abs0) ->
      self#ftscheme accu (_formula_scheme_abs0)

  method ftscheme : 'accumulator -> opaque_formula_scheme_abs -> 'accumulator = fun accu -> 
  function (_formula_scheme_abs0) -> 
      let accu = self#formula_scheme_abs accu (open_formula_scheme_abs _formula_scheme_abs0) in
      accu

  method predicate_definition : 'accumulator -> predicate_definition -> 'accumulator = fun accu -> function
  | PDAbbrev (_lformula0) ->
      self#pdabbrev accu (_lformula0)
  | PDInductive (_formula_type_schemes1, _lformulas0) ->
      self#pdinductive accu (_formula_type_schemes1, _lformulas0)

  method pdabbrev : 'accumulator -> lformula -> 'accumulator = fun accu -> 
  function (_lformula0) -> 
      let accu = (self#lformula) accu _lformula0 in
      accu

  method pdinductive : 'accumulator -> formula_type_scheme option * lformula list -> 'accumulator = fun accu -> 
  function (_formula_type_schemes1, _lformulas0) -> 
      let accu = option_fold (self#formula_type_scheme) accu _formula_type_schemes1 in
      let accu = List.fold_left (self#lformula) accu _lformulas0 in
      accu

  method lterm : 'accumulator -> lterm -> 'accumulator = fun accu -> function
  { tpos = _x1; tvalue = _term0 } ->
      let accu = self#tpos accu _x1 in
      let accu = self#tvalue accu _term0 in
      accu

  method tpos : 'accumulator -> ( Position.t ) -> 'accumulator = fun accu -> 
  function _x0 ->
      accu

  method tvalue : 'accumulator -> term -> 'accumulator = fun accu -> 
  function _term0 ->
      let accu = (self#term) accu _term0 in
      accu

  method lformula : 'accumulator -> lformula -> 'accumulator = fun accu -> function
  { fpos = _x1; fvalue = _formula0 } ->
      let accu = self#fpos accu _x1 in
      let accu = self#fvalue accu _formula0 in
      accu

  method fpos : 'accumulator -> ( Position.t ) -> 'accumulator = fun accu -> 
  function _x0 ->
      accu

  method fvalue : 'accumulator -> formula -> 'accumulator = fun accu -> 
  function _formula0 ->
      let accu = (self#formula) accu _formula0 in
      accu

  method pcons_abs : 'accumulator -> pcons_abs -> 'accumulator = fun accu -> function
  (_x2, _component1, _program0) ->
      let accu = (self#component) accu _component1 in
      let accu = (self#program) accu _program0 in
      accu

  method lfi_abs : 'accumulator -> lfi_abs -> 'accumulator = fun accu -> function
  (_ids1, _logic_clauses0) ->
      let accu = (self#ids) accu _ids1 in
      let accu = List.fold_left (self#logic_clause) accu _logic_clauses0 in
      accu

  method logic_clause_abs : 'accumulator -> logic_clause_abs -> 'accumulator = fun accu -> function
  (_pattern1, _lformula0) ->
      let accu = (self#pattern) accu _pattern1 in
      let accu = (self#lformula) accu _lformula0 in
      accu

  method fun_abs : 'accumulator -> fun_abs -> 'accumulator = fun accu -> function
  (_logic_arguments3, _logic_bindings2, _function_output1, _lterm0) ->
      let accu = (self#logic_arguments) accu _logic_arguments3 in
      let accu = (self#logic_bindings) accu _logic_bindings2 in
      let accu = (self#function_output) accu _function_output1 in
      let accu = (self#lterm) accu _lterm0 in
      accu

  method let_abs : 'accumulator -> let_abs -> 'accumulator = fun accu -> function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
      let accu = (self#ids) accu _ids3 in
      let accu = (self#optional_lformula) accu _optional_lformula2 in
      let accu = (self#lterm) accu _lterm1 in
      let accu = (self#lterm) accu _lterm0 in
      accu

  method letrec_abs : 'accumulator -> letrec_abs -> 'accumulator = fun accu -> function
  (_ids3, _optional_lformula2, _lterm1, _lterm0) ->
      let accu = (self#ids) accu _ids3 in
      let accu = (self#optional_lformula) accu _optional_lformula2 in
      let accu = (self#lterm) accu _lterm1 in
      let accu = (self#lterm) accu _lterm0 in
      accu

  method eforalltys_abs : 'accumulator -> eforalltys_abs -> 'accumulator = fun accu -> function
  (_type_parameters1, _lterm0) ->
      let accu = (self#type_parameters) accu _type_parameters1 in
      let accu = (self#lterm) accu _lterm0 in
      accu

  method eexiststys_abs : 'accumulator -> eexiststys_abs -> 'accumulator = fun accu -> function
  (_type_parameters1, _lterm0) ->
      let accu = (self#type_parameters) accu _type_parameters1 in
      let accu = (self#lterm) accu _lterm0 in
      accu

  method let_logic_abs : 'accumulator -> let_logic_abs -> 'accumulator = fun accu -> function
  (_fbindings2, _lformula1, _lterm0) ->
      let accu = (self#fbindings) accu _fbindings2 in
      let accu = (self#lformula) accu _lformula1 in
      let accu = (self#lterm) accu _lterm0 in
      accu

  method function_output_abs : 'accumulator -> function_output_abs -> 'accumulator = fun accu -> function
  (_logic_bindings0) ->
      let accu = (self#logic_bindings) accu _logic_bindings0 in
      accu

  method clause_abs : 'accumulator -> clause_abs -> 'accumulator = fun accu -> function
  (_pattern1, _lterm0) ->
      let accu = (self#pattern) accu _pattern1 in
      let accu = (self#lterm) accu _lterm0 in
      accu

  method scheme_abs : 'accumulator -> scheme_abs -> 'accumulator = fun accu -> function
  (_type_parameters1, _term_type0) ->
      let accu = (self#type_parameters) accu _type_parameters1 in
      let accu = (self#term_type) accu _term_type0 in
      accu

  method lfun_abs : 'accumulator -> lfun_abs -> 'accumulator = fun accu -> function
  (_fbindings1, _lformula0) ->
      let accu = (self#fbindings) accu _fbindings1 in
      let accu = (self#lformula) accu _lformula0 in
      accu

  method fforalltys_abs : 'accumulator -> fforalltys_abs -> 'accumulator = fun accu -> function
  (_type_parameters1, _lformula0) ->
      let accu = (self#type_parameters) accu _type_parameters1 in
      let accu = (self#lformula) accu _lformula0 in
      accu

  method fexiststys_abs : 'accumulator -> fexiststys_abs -> 'accumulator = fun accu -> function
  (_type_parameters1, _lformula0) ->
      let accu = (self#type_parameters) accu _type_parameters1 in
      let accu = (self#lformula) accu _lformula0 in
      accu

  method lforall_abs : 'accumulator -> lforall_abs -> 'accumulator = fun accu -> function
  (_fbindings2, _triggers1, _lformula0) ->
      let accu = (self#fbindings) accu _fbindings2 in
      let accu = List.fold_left (self#trigger) accu _triggers1 in
      let accu = (self#lformula) accu _lformula0 in
      accu

  method lexists_abs : 'accumulator -> lexists_abs -> 'accumulator = fun accu -> function
  (_fbindings1, _lformula0) ->
      let accu = (self#fbindings) accu _fbindings1 in
      let accu = (self#lformula) accu _lformula0 in
      accu

  method formula_scheme_abs : 'accumulator -> formula_scheme_abs -> 'accumulator = fun accu -> function
  (_type_parameters1, _formula_type0) ->
      let accu = (self#type_parameters) accu _type_parameters1 in
      let accu = (self#formula_type) accu _formula_type0 in
      accu

end
