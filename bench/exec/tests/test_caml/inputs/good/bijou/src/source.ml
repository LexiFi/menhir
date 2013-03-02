(* This file was generated from source.mla. Do not edit! *)

module Identifier = Identifier

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

module Datacon = AlphaLib.Atom.Make(Identifier)

module Datatype = AlphaLib.Atom.Make(Identifier)

module Exccon = AlphaLib.Atom.Make(Identifier)

module Func = AlphaLib.Atom.Make(Identifier)

module Sorte = AlphaLib.Atom.Make(Identifier)

module Var = AlphaLib.Atom.Make(Identifier)

module Raw = struct

type sorte =
  Identifier.t

 and asorte = 
  sorte

 and sorts = 
  asorte list

 and datatype =
  Identifier.t

 and adatatype = 
  datatype

 and datacon =
  Identifier.t

 and adatacon = 
  datacon

 and exccon =
  Identifier.t

 and aexccon = 
  exccon

 and var =
  Identifier.t

 and avar = 
  var

 and avaro = 
  avar option

 and func =
  Identifier.t

 and set_function = 
  | SFSupport
  | SFOuter
  | SFInner
  | SFBound

 and value = 
  | VVar of var
  | VBool of ( bool )
  | VData of datacon * value list

 and value_tuple = 
  | VTComponent of value
  | VTInner of sorts * value_tuple
  | VTOuter of sorts * value_tuple
  | VTAbstraction of sorts * value_tuple
  | VTTuple of value_tuple list

 and setsetset_operator = 
  | OpUnion
  | OpIntersection
  | OpDifference

 and raw_set_expression = 
  | SEmpty
  | SUniverse
  | SSort of sorte
  | SApp of set_function * asorte option * value_tuple
  | SAssocOp of setsetset_operator * set_expression list
  | SConditional of contrainte * set_expression * set_expression

 and set_expression = 
  raw_set_expression Location.t

 and setsetbool_operator = 
  | OpSubset
  | OpEqual
  | OpNotEqual
  | OpDisjoint

 and boolboolbool_operator = 
  | OpConjunction
  | OpDisjunction
  | OpImplication
  | OpEquivalence

 and raw_constraint = 
  | FTrue
  | FFalse
  | FBoolVar of var
  | FNot of contrainte
  | FBoolAssocOp of boolboolbool_operator * contrainte list
  | FSetBinOp of set_expression * setsetbool_operator * set_expression

 and contrainte = 
  raw_constraint Location.t

 and raw_type = 
  | TAtom of sorte
  | TAtomSet of sorte
  | TBool
  | TData of datatype

 and typ = 
  raw_type Location.t

 and raw_tuple = 
  | TComponent of typ
  | TInner of sorts * tuple
  | TOuter of sorts * tuple
  | TAbstraction of sorts * tuple
  | TTuple of tuple list
  | TName of var * tuple

 and tuple = 
  raw_tuple Location.t

 and guarded_tuple = 
  tuple * contrainte

 and guarded_params = 
  avaro list * contrainte

 and datatypedef = 
  { datatype_name: datatype; datatype_sorts: sorts; datatype_constructors: datacondef list }

 and datacondef = 
  datacon * dataconparams

 and dataconparams = 
  guarded_tuple

 and lemmadef = 
  lemma

 and lemma = 
  var * adatatype * contrainte

 and excdef = 
  exccon * dataconparams

 and raw_pattern = 
  | PZero
  | POne
  | PVar of var
  | PBool of ( bool )
  | PData of adatacon * pattern list
  | PConjunction of pattern list
  | PDisjunction of pattern list
  | PTuple of pattern list

 and pattern = 
  raw_pattern Location.t

 and raw_exception_pattern = 
  | EPData of aexccon * avaro list
  | EPDisjunction of exception_pattern list

 and exception_pattern = 
  raw_exception_pattern Location.t

 and time = 
  | CompileTime
  | RunTime

 and raw_expression = 
  | EVar of var
  | EBool of ( bool )
  | EData of datacon * expression list
  | EFresh of fresh_binding
  | ECase of expression * branch list
  | ECall of func * expression
  | ETuple of expression list
  | ERaise of exccon * expression list
  | ELetWhereUnless of expression * handler list * letw_binding
  | ETryUnless of expression * handler list
  | EAssert of time * contrainte * expression
  | EAssertFalse of time

 and expression = 
  raw_expression Location.t

 and fresh_binding = 
  asorte * avar list * expression

 and letw_binding = 
  avaro list * contrainte * expression

 and pat_binding = 
  pattern * expression

 and branch = 
  pat_binding

 and exc_pat_binding = 
  exception_pattern * expression

 and handler = 
  exc_pat_binding

 and normal_postcondition = 
  guarded_tuple

 and exceptional_postcondition = 
  exccon * guarded_params

 and postcondition = 
  { post_normal: normal_postcondition; post_exceptional: exceptional_postcondition list; post_other_exceptions: ( bool ) }

 and fundef = 
  { fun_params: tuple; fun_precondition: contrainte; fun_postcondition: postcondition; fun_body: expression }

 and fundef1 = 
  fundef

 and def = 
  | DefSort of sorte
  | DefDataType of datatypedef
  | DefLemma of lemmadef
  | DefException of excdef
  | DefFun of func * fundef1
  | DefBuiltin of func

 and defs = 
  def list

 and program = 
  defs

end

module Flat = struct

type sorte =
  Sorte.Atom.t

 and asorte = 
  sorte

 and sorts = 
  asorte list

 and datatype =
  Datatype.Atom.t

 and adatatype = 
  datatype

 and datacon =
  Datacon.Atom.t

 and adatacon = 
  datacon

 and exccon =
  Exccon.Atom.t

 and aexccon = 
  exccon

 and var =
  Var.Atom.t

 and avar = 
  var

 and avaro = 
  avar option

 and func =
  Func.Atom.t

 and set_function = 
  | SFSupport
  | SFOuter
  | SFInner
  | SFBound

 and value = 
  | VVar of var
  | VBool of ( bool )
  | VData of datacon * value list

 and value_tuple = 
  | VTComponent of value
  | VTInner of sorts * value_tuple
  | VTOuter of sorts * value_tuple
  | VTAbstraction of sorts * value_tuple
  | VTTuple of value_tuple list

 and setsetset_operator = 
  | OpUnion
  | OpIntersection
  | OpDifference

 and raw_set_expression = 
  | SEmpty
  | SUniverse
  | SSort of sorte
  | SApp of set_function * asorte option * value_tuple
  | SAssocOp of setsetset_operator * set_expression list
  | SConditional of contrainte * set_expression * set_expression

 and set_expression = 
  raw_set_expression Location.t

 and setsetbool_operator = 
  | OpSubset
  | OpEqual
  | OpNotEqual
  | OpDisjoint

 and boolboolbool_operator = 
  | OpConjunction
  | OpDisjunction
  | OpImplication
  | OpEquivalence

 and raw_constraint = 
  | FTrue
  | FFalse
  | FBoolVar of var
  | FNot of contrainte
  | FBoolAssocOp of boolboolbool_operator * contrainte list
  | FSetBinOp of set_expression * setsetbool_operator * set_expression

 and contrainte = 
  raw_constraint Location.t

 and raw_type = 
  | TAtom of sorte
  | TAtomSet of sorte
  | TBool
  | TData of datatype

 and typ = 
  raw_type Location.t

 and raw_tuple = 
  | TComponent of typ
  | TInner of sorts * tuple
  | TOuter of sorts * tuple
  | TAbstraction of sorts * tuple
  | TTuple of tuple list
  | TName of var * tuple

 and tuple = 
  raw_tuple Location.t

 and guarded_tuple = 
  tuple * contrainte

 and guarded_params = 
  avaro list * contrainte

 and datatypedef = 
  { datatype_name: datatype; datatype_sorts: sorts; datatype_constructors: datacondef list }

 and datacondef = 
  datacon * dataconparams

 and dataconparams = 
  guarded_tuple

 and lemmadef = 
  lemma

 and lemma = 
  var * adatatype * contrainte

 and excdef = 
  exccon * dataconparams

 and raw_pattern = 
  | PZero
  | POne
  | PVar of var
  | PBool of ( bool )
  | PData of adatacon * pattern list
  | PConjunction of pattern list
  | PDisjunction of pattern list
  | PTuple of pattern list

 and pattern = 
  raw_pattern Location.t

 and raw_exception_pattern = 
  | EPData of aexccon * avaro list
  | EPDisjunction of exception_pattern list

 and exception_pattern = 
  raw_exception_pattern Location.t

 and time = 
  | CompileTime
  | RunTime

 and raw_expression = 
  | EVar of var
  | EBool of ( bool )
  | EData of datacon * expression list
  | EFresh of fresh_binding
  | ECase of expression * branch list
  | ECall of func * expression
  | ETuple of expression list
  | ERaise of exccon * expression list
  | ELetWhereUnless of expression * handler list * letw_binding
  | ETryUnless of expression * handler list
  | EAssert of time * contrainte * expression
  | EAssertFalse of time

 and expression = 
  raw_expression Location.t

 and fresh_binding = 
  asorte * avar list * expression

 and letw_binding = 
  avaro list * contrainte * expression

 and pat_binding = 
  pattern * expression

 and branch = 
  pat_binding

 and exc_pat_binding = 
  exception_pattern * expression

 and handler = 
  exc_pat_binding

 and normal_postcondition = 
  guarded_tuple

 and exceptional_postcondition = 
  exccon * guarded_params

 and postcondition = 
  { post_normal: normal_postcondition; post_exceptional: exceptional_postcondition list; post_other_exceptions: ( bool ) }

 and fundef = 
  { fun_params: tuple; fun_precondition: contrainte; fun_postcondition: postcondition; fun_body: expression }

 and fundef1 = 
  fundef

 and def = 
  | DefSort of sorte
  | DefDataType of datatypedef
  | DefLemma of lemmadef
  | DefException of excdef
  | DefFun of func * fundef1
  | DefBuiltin of func

 and defs = 
  def list

 and program = 
  defs

end

type sorte =
  Sorte.Atom.t

 and asorte = 
  sorte

 and sorts = 
  asorte list

 and datatype =
  Datatype.Atom.t

 and adatatype = 
  datatype

 and datacon =
  Datacon.Atom.t

 and adatacon = 
  datacon

 and exccon =
  Exccon.Atom.t

 and aexccon = 
  exccon

 and var =
  Var.Atom.t

 and avar = 
  var

 and avaro = 
  avar option

 and func =
  Func.Atom.t

 and set_function = 
  | SFSupport
  | SFOuter
  | SFInner
  | SFBound

 and value = 
  | VVar of var
  | VBool of ( bool )
  | VData of datacon * value list

 and value_tuple = 
  | VTComponent of value
  | VTInner of sorts * value_tuple
  | VTOuter of sorts * value_tuple
  | VTAbstraction of sorts * value_tuple
  | VTTuple of value_tuple list

 and setsetset_operator = 
  | OpUnion
  | OpIntersection
  | OpDifference

 and raw_set_expression = 
  | SEmpty
  | SUniverse
  | SSort of sorte
  | SApp of set_function * asorte option * value_tuple
  | SAssocOp of setsetset_operator * set_expression list
  | SConditional of contrainte * set_expression * set_expression

 and set_expression = 
  raw_set_expression Location.t

 and setsetbool_operator = 
  | OpSubset
  | OpEqual
  | OpNotEqual
  | OpDisjoint

 and boolboolbool_operator = 
  | OpConjunction
  | OpDisjunction
  | OpImplication
  | OpEquivalence

 and raw_constraint = 
  | FTrue
  | FFalse
  | FBoolVar of var
  | FNot of contrainte
  | FBoolAssocOp of boolboolbool_operator * contrainte list
  | FSetBinOp of set_expression * setsetbool_operator * set_expression

 and contrainte = 
  raw_constraint Location.t

 and raw_type = 
  | TAtom of sorte
  | TAtomSet of sorte
  | TBool
  | TData of datatype

 and typ = 
  raw_type Location.t

 and raw_tuple = 
  | TComponent of typ
  | TInner of sorts * tuple
  | TOuter of sorts * tuple
  | TAbstraction of sorts * tuple
  | TTuple of tuple list
  | TName of var * tuple

 and tuple = 
  raw_tuple Location.t

 and guarded_tuple = 
  tuple * contrainte

 and opaque_guarded_tuple = {
    mutable guarded_tuple_delayed: Datacon.Subst.t * Datatype.Subst.t * Sorte.Subst.t * Var.Subst.t;
    mutable guarded_tuple: guarded_tuple
  }

 and guarded_params = 
  avaro list * contrainte

 and opaque_guarded_params = {
    mutable guarded_params_delayed: Datacon.Subst.t * Sorte.Subst.t * Var.Subst.t;
    mutable guarded_params: guarded_params
  }

 and datatypedef = 
  { datatype_name: datatype; datatype_sorts: sorts; datatype_constructors: datacondef list }

 and datacondef = 
  datacon * dataconparams

 and dataconparams = 
  opaque_guarded_tuple

 and lemmadef = 
  opaque_lemma

 and lemma = 
  var * adatatype * contrainte

 and opaque_lemma = {
    mutable lemma_delayed: Datacon.Subst.t * Datatype.Subst.t * Sorte.Subst.t * Var.Subst.t;
    mutable lemma: lemma
  }

 and excdef = 
  exccon * dataconparams

 and raw_pattern = 
  | PZero
  | POne
  | PVar of var
  | PBool of ( bool )
  | PData of adatacon * pattern list
  | PConjunction of pattern list
  | PDisjunction of pattern list
  | PTuple of pattern list

 and pattern = 
  raw_pattern Location.t

 and raw_exception_pattern = 
  | EPData of aexccon * avaro list
  | EPDisjunction of exception_pattern list

 and exception_pattern = 
  raw_exception_pattern Location.t

 and time = 
  | CompileTime
  | RunTime

 and raw_expression = 
  | EVar of var
  | EBool of ( bool )
  | EData of datacon * expression list
  | EFresh of opaque_fresh_binding
  | ECase of expression * branch list
  | ECall of func * expression
  | ETuple of expression list
  | ERaise of exccon * expression list
  | ELetWhereUnless of expression * handler list * opaque_letw_binding
  | ETryUnless of expression * handler list
  | EAssert of time * contrainte * expression
  | EAssertFalse of time

 and expression = 
  raw_expression Location.t

 and fresh_binding = 
  asorte * avar list * expression

 and opaque_fresh_binding = {
    mutable fresh_binding_delayed: Datacon.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Var.Subst.t;
    mutable fresh_binding: fresh_binding
  }

 and letw_binding = 
  avaro list * contrainte * expression

 and opaque_letw_binding = {
    mutable letw_binding_delayed: Datacon.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Var.Subst.t;
    mutable letw_binding: letw_binding
  }

 and pat_binding = 
  pattern * expression

 and opaque_pat_binding = {
    mutable pat_binding_delayed: Datacon.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Var.Subst.t;
    mutable pat_binding: pat_binding
  }

 and branch = 
  opaque_pat_binding

 and exc_pat_binding = 
  exception_pattern * expression

 and opaque_exc_pat_binding = {
    mutable exc_pat_binding_delayed: Datacon.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Var.Subst.t;
    mutable exc_pat_binding: exc_pat_binding
  }

 and handler = 
  opaque_exc_pat_binding

 and normal_postcondition = 
  opaque_guarded_tuple

 and exceptional_postcondition = 
  exccon * opaque_guarded_params

 and postcondition = 
  { post_normal: normal_postcondition; post_exceptional: exceptional_postcondition list; post_other_exceptions: ( bool ) }

 and fundef = 
  { fun_params: tuple; fun_precondition: contrainte; fun_postcondition: postcondition; fun_body: expression }

 and opaque_fundef = {
    mutable fundef_delayed: Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Var.Subst.t;
    mutable fundef: fundef
  }

 and fundef1 = 
  opaque_fundef

 and def = 
  | DefSort of sorte
  | DefDataType of datatypedef
  | DefLemma of lemmadef
  | DefException of excdef
  | DefFun of func * fundef1
  | DefBuiltin of func

 and defs = 
  def list

 and opaque_defs = {
    mutable defs_delayed: Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Var.Subst.t;
    mutable defs: defs
  }

 and program = 
  opaque_defs

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

and import_asorte : sorte Identifier.Map.t -> Raw.asorte -> asorte = fun (sorte_env) -> function
  (_sorte0) ->
    (Sorte.find _sorte0 sorte_env)

and subst_asorte : Sorte.Subst.t -> asorte -> asorte = fun (sorte_env) -> function
  (_sorte0) ->
    (Sorte.Subst.lookup _sorte0 sorte_env)

and export_asorte : Sorte.AtomIdMap.t -> asorte -> Raw.asorte = fun (sorte_m) -> function
  (_sorte0) ->
    (Sorte.AtomIdMap.lookup _sorte0 sorte_m)

and flatten_asorte : asorte -> Flat.asorte = function
  (_sorte0) ->
    (_sorte0)

and unflatten_asorte : Flat.asorte -> asorte = function
  (_sorte0) ->
    (_sorte0)

and free_asorte : asorte -> Sorte.AtomSet.t = 
  function asorte -> free_accu_asorte (Sorte.AtomSet.empty) asorte

and equal_asorte : asorte -> asorte -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_asorte x1 x2

and aeq_asorte : unit -> asorte -> asorte -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_sorte0), (_sorte1) ->
      if not (Sorte.Atom.equal _sorte0 _sorte1) then raise (Invalid_argument "aeq_asorte");
      ()

and free_accu_asorte = fun (sorte_fvars) -> function
  (_sorte0) ->
      let sorte_fvars = Sorte.AtomSet.add _sorte0 sorte_fvars in
      (sorte_fvars)

and import_sorts : sorte Identifier.Map.t -> Raw.sorts -> sorts = fun (sorte_env) -> function
  (_asortes0) ->
    (List.map (import_asorte (sorte_env)) _asortes0)

and subst_sorts : Sorte.Subst.t -> sorts -> sorts = fun (sorte_env) -> function
  (_asortes0) ->
    (List.map (subst_asorte (sorte_env)) _asortes0)

and export_sorts : Sorte.AtomIdMap.t -> sorts -> Raw.sorts = fun (sorte_m) -> function
  (_asortes0) ->
    (List.map (export_asorte (sorte_m)) _asortes0)

and flatten_sorts : sorts -> Flat.sorts = function
  (_asortes0) ->
    (List.map flatten_asorte _asortes0)

and unflatten_sorts : Flat.sorts -> sorts = function
  (_asortes0) ->
    (List.map unflatten_asorte _asortes0)

and free_sorts : sorts -> Sorte.AtomSet.t = 
  function sorts -> free_accu_sorts (Sorte.AtomSet.empty) sorts

and equal_sorts : sorts -> sorts -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_sorts x1 x2

and aeq_sorts : unit -> sorts -> sorts -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_asortes0), (_asortes1) ->
      List.fold_left2 aeq_asorte () _asortes0 _asortes1;
      ()

and free_accu_sorts = fun (sorte_fvars) -> function
  (_asortes0) ->
      let (sorte_fvars) = List.fold_left free_accu_asorte (sorte_fvars) _asortes0 in 
      (sorte_fvars)

and import_adatatype : datatype Identifier.Map.t -> Raw.adatatype -> adatatype = fun (datatype_env) -> function
  (_datatype0) ->
    (Datatype.find _datatype0 datatype_env)

and subst_adatatype : Datatype.Subst.t -> adatatype -> adatatype = fun (datatype_env) -> function
  (_datatype0) ->
    (Datatype.Subst.lookup _datatype0 datatype_env)

and export_adatatype : Datatype.AtomIdMap.t -> adatatype -> Raw.adatatype = fun (datatype_m) -> function
  (_datatype0) ->
    (Datatype.AtomIdMap.lookup _datatype0 datatype_m)

and flatten_adatatype : adatatype -> Flat.adatatype = function
  (_datatype0) ->
    (_datatype0)

and unflatten_adatatype : Flat.adatatype -> adatatype = function
  (_datatype0) ->
    (_datatype0)

and free_adatatype : adatatype -> Datatype.AtomSet.t = 
  function adatatype -> free_accu_adatatype (Datatype.AtomSet.empty) adatatype

and equal_adatatype : adatatype -> adatatype -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_adatatype x1 x2

and aeq_adatatype : unit -> adatatype -> adatatype -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_datatype0), (_datatype1) ->
      if not (Datatype.Atom.equal _datatype0 _datatype1) then raise (Invalid_argument "aeq_adatatype");
      ()

and free_accu_adatatype = fun (datatype_fvars) -> function
  (_datatype0) ->
      let datatype_fvars = Datatype.AtomSet.add _datatype0 datatype_fvars in
      (datatype_fvars)

and import_adatacon : datacon Identifier.Map.t -> Raw.adatacon -> adatacon = fun (datacon_env) -> function
  (_datacon0) ->
    (Datacon.find _datacon0 datacon_env)

and subst_adatacon : Datacon.Subst.t -> adatacon -> adatacon = fun (datacon_env) -> function
  (_datacon0) ->
    (Datacon.Subst.lookup _datacon0 datacon_env)

and export_adatacon : Datacon.AtomIdMap.t -> adatacon -> Raw.adatacon = fun (datacon_m) -> function
  (_datacon0) ->
    (Datacon.AtomIdMap.lookup _datacon0 datacon_m)

and flatten_adatacon : adatacon -> Flat.adatacon = function
  (_datacon0) ->
    (_datacon0)

and unflatten_adatacon : Flat.adatacon -> adatacon = function
  (_datacon0) ->
    (_datacon0)

and free_adatacon : adatacon -> Datacon.AtomSet.t = 
  function adatacon -> free_accu_adatacon (Datacon.AtomSet.empty) adatacon

and equal_adatacon : adatacon -> adatacon -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_adatacon x1 x2

and aeq_adatacon : unit -> adatacon -> adatacon -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_datacon0), (_datacon1) ->
      if not (Datacon.Atom.equal _datacon0 _datacon1) then raise (Invalid_argument "aeq_adatacon");
      ()

and free_accu_adatacon = fun (datacon_fvars) -> function
  (_datacon0) ->
      let datacon_fvars = Datacon.AtomSet.add _datacon0 datacon_fvars in
      (datacon_fvars)

and import_aexccon : exccon Identifier.Map.t -> Raw.aexccon -> aexccon = fun (exccon_env) -> function
  (_exccon0) ->
    (Exccon.find _exccon0 exccon_env)

and subst_aexccon : Exccon.Subst.t -> aexccon -> aexccon = fun (exccon_env) -> function
  (_exccon0) ->
    (Exccon.Subst.lookup _exccon0 exccon_env)

and export_aexccon : Exccon.AtomIdMap.t -> aexccon -> Raw.aexccon = fun (exccon_m) -> function
  (_exccon0) ->
    (Exccon.AtomIdMap.lookup _exccon0 exccon_m)

and flatten_aexccon : aexccon -> Flat.aexccon = function
  (_exccon0) ->
    (_exccon0)

and unflatten_aexccon : Flat.aexccon -> aexccon = function
  (_exccon0) ->
    (_exccon0)

and free_aexccon : aexccon -> Exccon.AtomSet.t = 
  function aexccon -> free_accu_aexccon (Exccon.AtomSet.empty) aexccon

and equal_aexccon : aexccon -> aexccon -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_aexccon x1 x2

and aeq_aexccon : unit -> aexccon -> aexccon -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_exccon0), (_exccon1) ->
      if not (Exccon.Atom.equal _exccon0 _exccon1) then raise (Invalid_argument "aeq_aexccon");
      ()

and free_accu_aexccon = fun (exccon_fvars) -> function
  (_exccon0) ->
      let exccon_fvars = Exccon.AtomSet.add _exccon0 exccon_fvars in
      (exccon_fvars)

and subst_avar : Var.Subst.t -> avar -> avar = fun (var_ienv) -> function
  (_var0) ->
    (Var.Subst.lookup _var0 var_ienv)

and bound_avar : avar -> Var.AtomSet.t = 
  function avar -> bound_accu_avar (Var.AtomSet.empty) avar

and bound_free_avar : avar -> Var.AtomSet.t = 
  function avar -> bound_free_accu_avar (Var.AtomSet.empty) avar

and equal_avar : avar -> avar -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_avar x1 x2

and import_avar = fun (var_ienv) -> function
  (_var0) ->
    (Var.find _var0 var_ienv)

and bvi_accu_avar = fun (var_bvars) -> function
  (_var0) ->
      let var_bvars = Identifier.Map.add _var0 () var_bvars in
      (var_bvars)

and bvi_avar = 
  function avar -> bvi_accu_avar (Identifier.Map.empty) avar

and bound_accu_avar = fun (var_bvars) -> function
  (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars)

and export_avar : Var.AtomIdMap.t -> avar -> Raw.avar = fun (var_im) -> function
  (_var0) ->
    (Var.AtomIdMap.lookup _var0 var_im)

and flatten_avar : avar -> Flat.avar = function
  (_var0) ->
    (_var0)

and unflatten_avar : Flat.avar -> avar = function
  (_var0) ->
    (_var0)

and bound_free_accu_avar = fun (var_bvars) -> function
  (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars)

and aeq_avar : unit -> avar -> avar -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var0), (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_avar");
      ()

and freshen2_avar : Var.Subst.t * Var.Subst.t -> avar -> avar -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var0), (_var1) ->
      let var_env1, var_env2 = Var.Subst.freshen2 _var0 var_env1 _var1 var_env2 in
      (var_env1, var_env2)

and subst_avaro : Var.Subst.t -> avaro -> avaro = fun (var_ienv) -> function
  (_avars0) ->
    (option_map (subst_avar (var_ienv)) _avars0)

and bound_avaro : avaro -> Var.AtomSet.t = 
  function avaro -> bound_accu_avaro (Var.AtomSet.empty) avaro

and bound_free_avaro : avaro -> Var.AtomSet.t = 
  function avaro -> bound_free_accu_avaro (Var.AtomSet.empty) avaro

and equal_avaro : avaro -> avaro -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_avaro x1 x2

and import_avaro = fun (var_ienv) -> function
  (_avars0) ->
    (option_map (import_avar (var_ienv)) _avars0)

and bvi_accu_avaro = fun (var_bvars) -> function
  (_avars0) ->
      let (var_bvars) = option_fold bvi_accu_avar (var_bvars) _avars0 in
      (var_bvars)

and bvi_avaro = 
  function avaro -> bvi_accu_avaro (Identifier.Map.empty) avaro

and bound_accu_avaro = fun (var_bvars) -> function
  (_avars0) ->
      let (var_bvars) = option_fold bound_accu_avar (var_bvars) _avars0 in
      (var_bvars)

and export_avaro : Var.AtomIdMap.t -> avaro -> Raw.avaro = fun (var_im) -> function
  (_avars0) ->
    (option_map (export_avar (var_im)) _avars0)

and flatten_avaro : avaro -> Flat.avaro = function
  (_avars0) ->
    (option_map flatten_avar _avars0)

and unflatten_avaro : Flat.avaro -> avaro = function
  (_avars0) ->
    (option_map unflatten_avar _avars0)

and bound_free_accu_avaro = fun (var_bvars) -> function
  (_avars0) ->
      let (var_bvars) = option_fold bound_free_accu_avar (var_bvars) _avars0 in
      (var_bvars)

and aeq_avaro : unit -> avaro -> avaro -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_avars0), (_avars1) ->
      option_fold2 aeq_avar () _avars0 _avars1;
      ()

and freshen2_avaro : Var.Subst.t * Var.Subst.t -> avaro -> avaro -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_avars0), (_avars1) ->
      let (var_env1, var_env2) = option_fold2 freshen2_avar (var_env1, var_env2) _avars0 _avars1 in
      (var_env1, var_env2)

and import_set_function : unit -> Raw.set_function -> set_function = fun () -> function
  | Raw.SFSupport ->
      SFSupport
  | Raw.SFOuter ->
      SFOuter
  | Raw.SFInner ->
      SFInner
  | Raw.SFBound ->
      SFBound

and subst_set_function : unit -> set_function -> set_function = fun () -> function
  | SFSupport ->
      SFSupport
  | SFOuter ->
      SFOuter
  | SFInner ->
      SFInner
  | SFBound ->
      SFBound

and export_set_function : unit -> set_function -> Raw.set_function = fun () -> function
  | SFSupport ->
      Raw.SFSupport
  | SFOuter ->
      Raw.SFOuter
  | SFInner ->
      Raw.SFInner
  | SFBound ->
      Raw.SFBound

and flatten_set_function : set_function -> Flat.set_function = function
  | SFSupport ->
      Flat.SFSupport
  | SFOuter ->
      Flat.SFOuter
  | SFInner ->
      Flat.SFInner
  | SFBound ->
      Flat.SFBound

and unflatten_set_function : Flat.set_function -> set_function = function
  | Flat.SFSupport ->
      SFSupport
  | Flat.SFOuter ->
      SFOuter
  | Flat.SFInner ->
      SFInner
  | Flat.SFBound ->
      SFBound

and free_set_function : set_function -> unit = 
  function set_function -> free_accu_set_function () set_function

and equal_set_function : set_function -> set_function -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_set_function x1 x2

and aeq_set_function : unit -> set_function -> set_function -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | SFSupport, SFSupport ->
      ()
  | SFOuter, SFOuter ->
      ()
  | SFInner, SFInner ->
      ()
  | SFBound, SFBound ->
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_set_function")

and free_accu_set_function = fun () -> function
  | SFSupport ->
      ()
  | SFOuter ->
      ()
  | SFInner ->
      ()
  | SFBound ->
      ()

and import_value : datacon Identifier.Map.t * var Identifier.Map.t -> Raw.value -> value = fun (datacon_env, var_env) -> function
  | Raw.VVar (_var0) ->
      VVar (Var.find _var0 var_env)
  | Raw.VBool (_x0) ->
      VBool (_x0)
  | Raw.VData (_datacon1, _values0) ->
      VData (Datacon.find _datacon1 datacon_env, List.map (import_value (datacon_env, var_env)) _values0)

and subst_value : Datacon.Subst.t * Var.Subst.t -> value -> value = fun (datacon_env, var_env) -> function
  | VVar (_var0) ->
      VVar (Var.Subst.lookup _var0 var_env)
  | VBool (_x0) ->
      VBool (_x0)
  | VData (_datacon1, _values0) ->
      VData (Datacon.Subst.lookup _datacon1 datacon_env, List.map (subst_value (datacon_env, var_env)) _values0)

and export_value : Datacon.AtomIdMap.t * Var.AtomIdMap.t -> value -> Raw.value = fun (datacon_m, var_m) -> function
  | VVar (_var0) ->
      Raw.VVar (Var.AtomIdMap.lookup _var0 var_m)
  | VBool (_x0) ->
      Raw.VBool (_x0)
  | VData (_datacon1, _values0) ->
      Raw.VData (Datacon.AtomIdMap.lookup _datacon1 datacon_m, List.map (export_value (datacon_m, var_m)) _values0)

and flatten_value : value -> Flat.value = function
  | VVar (_var0) ->
      Flat.VVar (_var0)
  | VBool (_x0) ->
      Flat.VBool (_x0)
  | VData (_datacon1, _values0) ->
      Flat.VData (_datacon1, List.map flatten_value _values0)

and unflatten_value : Flat.value -> value = function
  | Flat.VVar (_var0) ->
      VVar (_var0)
  | Flat.VBool (_x0) ->
      VBool (_x0)
  | Flat.VData (_datacon1, _values0) ->
      VData (_datacon1, List.map unflatten_value _values0)

and free_value : value -> Datacon.AtomSet.t * Var.AtomSet.t = 
  function value -> free_accu_value (Datacon.AtomSet.empty, Var.AtomSet.empty) value

and equal_value : value -> value -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_value x1 x2

and aeq_value : unit -> value -> value -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | VVar (_var0), VVar (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_value");
      ()
  | VBool (_x0), VBool (_x1) ->
      ()
  | VData (_datacon1, _values0), VData (_datacon3, _values2) ->
      if not (Datacon.Atom.equal _datacon1 _datacon3) then raise (Invalid_argument "aeq_value");
      List.fold_left2 aeq_value () _values0 _values2;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_value")

and free_accu_value = fun (datacon_fvars, var_fvars) -> function
  | VVar (_var0) ->
      let var_fvars = Var.AtomSet.add _var0 var_fvars in
      (datacon_fvars, var_fvars)
  | VBool (_x0) ->
      (datacon_fvars, var_fvars)
  | VData (_datacon1, _values0) ->
      let datacon_fvars = Datacon.AtomSet.add _datacon1 datacon_fvars in
      let (datacon_fvars, var_fvars) = List.fold_left free_accu_value (datacon_fvars, var_fvars) _values0 in 
      (datacon_fvars, var_fvars)

and import_value_tuple : datacon Identifier.Map.t * sorte Identifier.Map.t * var Identifier.Map.t -> Raw.value_tuple -> value_tuple = fun (datacon_env, sorte_env, var_env) -> function
  | Raw.VTComponent (_value0) ->
      VTComponent ((import_value (datacon_env, var_env)) _value0)
  | Raw.VTInner (_sorts1, _value_tuple0) ->
      VTInner ((import_sorts (sorte_env)) _sorts1, (import_value_tuple (datacon_env, sorte_env, var_env)) _value_tuple0)
  | Raw.VTOuter (_sorts1, _value_tuple0) ->
      VTOuter ((import_sorts (sorte_env)) _sorts1, (import_value_tuple (datacon_env, sorte_env, var_env)) _value_tuple0)
  | Raw.VTAbstraction (_sorts1, _value_tuple0) ->
      VTAbstraction ((import_sorts (sorte_env)) _sorts1, (import_value_tuple (datacon_env, sorte_env, var_env)) _value_tuple0)
  | Raw.VTTuple (_value_tuples0) ->
      VTTuple (List.map (import_value_tuple (datacon_env, sorte_env, var_env)) _value_tuples0)

and subst_value_tuple : Datacon.Subst.t * Sorte.Subst.t * Var.Subst.t -> value_tuple -> value_tuple = fun (datacon_env, sorte_env, var_env) -> function
  | VTComponent (_value0) ->
      VTComponent ((subst_value (datacon_env, var_env)) _value0)
  | VTInner (_sorts1, _value_tuple0) ->
      VTInner ((subst_sorts (sorte_env)) _sorts1, (subst_value_tuple (datacon_env, sorte_env, var_env)) _value_tuple0)
  | VTOuter (_sorts1, _value_tuple0) ->
      VTOuter ((subst_sorts (sorte_env)) _sorts1, (subst_value_tuple (datacon_env, sorte_env, var_env)) _value_tuple0)
  | VTAbstraction (_sorts1, _value_tuple0) ->
      VTAbstraction ((subst_sorts (sorte_env)) _sorts1, (subst_value_tuple (datacon_env, sorte_env, var_env)) _value_tuple0)
  | VTTuple (_value_tuples0) ->
      VTTuple (List.map (subst_value_tuple (datacon_env, sorte_env, var_env)) _value_tuples0)

and export_value_tuple : Datacon.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> value_tuple -> Raw.value_tuple = fun (datacon_m, sorte_m, var_m) -> function
  | VTComponent (_value0) ->
      Raw.VTComponent ((export_value (datacon_m, var_m)) _value0)
  | VTInner (_sorts1, _value_tuple0) ->
      Raw.VTInner ((export_sorts (sorte_m)) _sorts1, (export_value_tuple (datacon_m, sorte_m, var_m)) _value_tuple0)
  | VTOuter (_sorts1, _value_tuple0) ->
      Raw.VTOuter ((export_sorts (sorte_m)) _sorts1, (export_value_tuple (datacon_m, sorte_m, var_m)) _value_tuple0)
  | VTAbstraction (_sorts1, _value_tuple0) ->
      Raw.VTAbstraction ((export_sorts (sorte_m)) _sorts1, (export_value_tuple (datacon_m, sorte_m, var_m)) _value_tuple0)
  | VTTuple (_value_tuples0) ->
      Raw.VTTuple (List.map (export_value_tuple (datacon_m, sorte_m, var_m)) _value_tuples0)

and flatten_value_tuple : value_tuple -> Flat.value_tuple = function
  | VTComponent (_value0) ->
      Flat.VTComponent (flatten_value _value0)
  | VTInner (_sorts1, _value_tuple0) ->
      Flat.VTInner (flatten_sorts _sorts1, flatten_value_tuple _value_tuple0)
  | VTOuter (_sorts1, _value_tuple0) ->
      Flat.VTOuter (flatten_sorts _sorts1, flatten_value_tuple _value_tuple0)
  | VTAbstraction (_sorts1, _value_tuple0) ->
      Flat.VTAbstraction (flatten_sorts _sorts1, flatten_value_tuple _value_tuple0)
  | VTTuple (_value_tuples0) ->
      Flat.VTTuple (List.map flatten_value_tuple _value_tuples0)

and unflatten_value_tuple : Flat.value_tuple -> value_tuple = function
  | Flat.VTComponent (_value0) ->
      VTComponent (unflatten_value _value0)
  | Flat.VTInner (_sorts1, _value_tuple0) ->
      VTInner (unflatten_sorts _sorts1, unflatten_value_tuple _value_tuple0)
  | Flat.VTOuter (_sorts1, _value_tuple0) ->
      VTOuter (unflatten_sorts _sorts1, unflatten_value_tuple _value_tuple0)
  | Flat.VTAbstraction (_sorts1, _value_tuple0) ->
      VTAbstraction (unflatten_sorts _sorts1, unflatten_value_tuple _value_tuple0)
  | Flat.VTTuple (_value_tuples0) ->
      VTTuple (List.map unflatten_value_tuple _value_tuples0)

and free_value_tuple : value_tuple -> Datacon.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function value_tuple -> free_accu_value_tuple (Datacon.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) value_tuple

and equal_value_tuple : value_tuple -> value_tuple -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_value_tuple x1 x2

and aeq_value_tuple : unit -> value_tuple -> value_tuple -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | VTComponent (_value0), VTComponent (_value1) ->
      aeq_value () _value0 _value1;
      ()
  | VTInner (_sorts1, _value_tuple0), VTInner (_sorts3, _value_tuple2) ->
      aeq_sorts () _sorts1 _sorts3;
      aeq_value_tuple () _value_tuple0 _value_tuple2;
      ()
  | VTOuter (_sorts1, _value_tuple0), VTOuter (_sorts3, _value_tuple2) ->
      aeq_sorts () _sorts1 _sorts3;
      aeq_value_tuple () _value_tuple0 _value_tuple2;
      ()
  | VTAbstraction (_sorts1, _value_tuple0), VTAbstraction (_sorts3, _value_tuple2) ->
      aeq_sorts () _sorts1 _sorts3;
      aeq_value_tuple () _value_tuple0 _value_tuple2;
      ()
  | VTTuple (_value_tuples0), VTTuple (_value_tuples1) ->
      List.fold_left2 aeq_value_tuple () _value_tuples0 _value_tuples1;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_value_tuple")

and free_accu_value_tuple = fun (datacon_fvars, sorte_fvars, var_fvars) -> function
  | VTComponent (_value0) ->
      let (datacon_fvars, var_fvars) = free_accu_value (datacon_fvars, var_fvars) _value0 in 
      (datacon_fvars, sorte_fvars, var_fvars)
  | VTInner (_sorts1, _value_tuple0) ->
      let (sorte_fvars) = free_accu_sorts (sorte_fvars) _sorts1 in 
      let (datacon_fvars, sorte_fvars, var_fvars) = free_accu_value_tuple (datacon_fvars, sorte_fvars, var_fvars) _value_tuple0 in 
      (datacon_fvars, sorte_fvars, var_fvars)
  | VTOuter (_sorts1, _value_tuple0) ->
      let (sorte_fvars) = free_accu_sorts (sorte_fvars) _sorts1 in 
      let (datacon_fvars, sorte_fvars, var_fvars) = free_accu_value_tuple (datacon_fvars, sorte_fvars, var_fvars) _value_tuple0 in 
      (datacon_fvars, sorte_fvars, var_fvars)
  | VTAbstraction (_sorts1, _value_tuple0) ->
      let (sorte_fvars) = free_accu_sorts (sorte_fvars) _sorts1 in 
      let (datacon_fvars, sorte_fvars, var_fvars) = free_accu_value_tuple (datacon_fvars, sorte_fvars, var_fvars) _value_tuple0 in 
      (datacon_fvars, sorte_fvars, var_fvars)
  | VTTuple (_value_tuples0) ->
      let (datacon_fvars, sorte_fvars, var_fvars) = List.fold_left free_accu_value_tuple (datacon_fvars, sorte_fvars, var_fvars) _value_tuples0 in 
      (datacon_fvars, sorte_fvars, var_fvars)

and import_setsetset_operator : unit -> Raw.setsetset_operator -> setsetset_operator = fun () -> function
  | Raw.OpUnion ->
      OpUnion
  | Raw.OpIntersection ->
      OpIntersection
  | Raw.OpDifference ->
      OpDifference

and subst_setsetset_operator : unit -> setsetset_operator -> setsetset_operator = fun () -> function
  | OpUnion ->
      OpUnion
  | OpIntersection ->
      OpIntersection
  | OpDifference ->
      OpDifference

and export_setsetset_operator : unit -> setsetset_operator -> Raw.setsetset_operator = fun () -> function
  | OpUnion ->
      Raw.OpUnion
  | OpIntersection ->
      Raw.OpIntersection
  | OpDifference ->
      Raw.OpDifference

and flatten_setsetset_operator : setsetset_operator -> Flat.setsetset_operator = function
  | OpUnion ->
      Flat.OpUnion
  | OpIntersection ->
      Flat.OpIntersection
  | OpDifference ->
      Flat.OpDifference

and unflatten_setsetset_operator : Flat.setsetset_operator -> setsetset_operator = function
  | Flat.OpUnion ->
      OpUnion
  | Flat.OpIntersection ->
      OpIntersection
  | Flat.OpDifference ->
      OpDifference

and free_setsetset_operator : setsetset_operator -> unit = 
  function setsetset_operator -> free_accu_setsetset_operator () setsetset_operator

and equal_setsetset_operator : setsetset_operator -> setsetset_operator -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_setsetset_operator x1 x2

and aeq_setsetset_operator : unit -> setsetset_operator -> setsetset_operator -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | OpUnion, OpUnion ->
      ()
  | OpIntersection, OpIntersection ->
      ()
  | OpDifference, OpDifference ->
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_setsetset_operator")

and free_accu_setsetset_operator = fun () -> function
  | OpUnion ->
      ()
  | OpIntersection ->
      ()
  | OpDifference ->
      ()

and import_raw_set_expression : datacon Identifier.Map.t * sorte Identifier.Map.t * var Identifier.Map.t -> Raw.raw_set_expression -> raw_set_expression = fun (datacon_env, sorte_env, var_env) -> function
  | Raw.SEmpty ->
      SEmpty
  | Raw.SUniverse ->
      SUniverse
  | Raw.SSort (_sorte0) ->
      SSort (Sorte.find _sorte0 sorte_env)
  | Raw.SApp (_set_function2, _asortes1, _value_tuple0) ->
      SApp ((import_set_function ()) _set_function2, option_map (import_asorte (sorte_env)) _asortes1, (import_value_tuple (datacon_env, sorte_env, var_env)) _value_tuple0)
  | Raw.SAssocOp (_setsetset_operator1, _set_expressions0) ->
      SAssocOp ((import_setsetset_operator ()) _setsetset_operator1, List.map (import_set_expression (datacon_env, sorte_env, var_env)) _set_expressions0)
  | Raw.SConditional (_contrainte2, _set_expression1, _set_expression0) ->
      SConditional ((import_contrainte (datacon_env, sorte_env, var_env)) _contrainte2, (import_set_expression (datacon_env, sorte_env, var_env)) _set_expression1, (import_set_expression (datacon_env, sorte_env, var_env)) _set_expression0)

and subst_raw_set_expression : Datacon.Subst.t * Sorte.Subst.t * Var.Subst.t -> raw_set_expression -> raw_set_expression = fun (datacon_env, sorte_env, var_env) -> function
  | SEmpty ->
      SEmpty
  | SUniverse ->
      SUniverse
  | SSort (_sorte0) ->
      SSort (Sorte.Subst.lookup _sorte0 sorte_env)
  | SApp (_set_function2, _asortes1, _value_tuple0) ->
      SApp ((subst_set_function ()) _set_function2, option_map (subst_asorte (sorte_env)) _asortes1, (subst_value_tuple (datacon_env, sorte_env, var_env)) _value_tuple0)
  | SAssocOp (_setsetset_operator1, _set_expressions0) ->
      SAssocOp ((subst_setsetset_operator ()) _setsetset_operator1, List.map (subst_set_expression (datacon_env, sorte_env, var_env)) _set_expressions0)
  | SConditional (_contrainte2, _set_expression1, _set_expression0) ->
      SConditional ((subst_contrainte (datacon_env, sorte_env, var_env)) _contrainte2, (subst_set_expression (datacon_env, sorte_env, var_env)) _set_expression1, (subst_set_expression (datacon_env, sorte_env, var_env)) _set_expression0)

and export_raw_set_expression : Datacon.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> raw_set_expression -> Raw.raw_set_expression = fun (datacon_m, sorte_m, var_m) -> function
  | SEmpty ->
      Raw.SEmpty
  | SUniverse ->
      Raw.SUniverse
  | SSort (_sorte0) ->
      Raw.SSort (Sorte.AtomIdMap.lookup _sorte0 sorte_m)
  | SApp (_set_function2, _asortes1, _value_tuple0) ->
      Raw.SApp ((export_set_function ()) _set_function2, option_map (export_asorte (sorte_m)) _asortes1, (export_value_tuple (datacon_m, sorte_m, var_m)) _value_tuple0)
  | SAssocOp (_setsetset_operator1, _set_expressions0) ->
      Raw.SAssocOp ((export_setsetset_operator ()) _setsetset_operator1, List.map (export_set_expression (datacon_m, sorte_m, var_m)) _set_expressions0)
  | SConditional (_contrainte2, _set_expression1, _set_expression0) ->
      Raw.SConditional ((export_contrainte (datacon_m, sorte_m, var_m)) _contrainte2, (export_set_expression (datacon_m, sorte_m, var_m)) _set_expression1, (export_set_expression (datacon_m, sorte_m, var_m)) _set_expression0)

and flatten_raw_set_expression : raw_set_expression -> Flat.raw_set_expression = function
  | SEmpty ->
      Flat.SEmpty
  | SUniverse ->
      Flat.SUniverse
  | SSort (_sorte0) ->
      Flat.SSort (_sorte0)
  | SApp (_set_function2, _asortes1, _value_tuple0) ->
      Flat.SApp (flatten_set_function _set_function2, option_map flatten_asorte _asortes1, flatten_value_tuple _value_tuple0)
  | SAssocOp (_setsetset_operator1, _set_expressions0) ->
      Flat.SAssocOp (flatten_setsetset_operator _setsetset_operator1, List.map flatten_set_expression _set_expressions0)
  | SConditional (_contrainte2, _set_expression1, _set_expression0) ->
      Flat.SConditional (flatten_contrainte _contrainte2, flatten_set_expression _set_expression1, flatten_set_expression _set_expression0)

and unflatten_raw_set_expression : Flat.raw_set_expression -> raw_set_expression = function
  | Flat.SEmpty ->
      SEmpty
  | Flat.SUniverse ->
      SUniverse
  | Flat.SSort (_sorte0) ->
      SSort (_sorte0)
  | Flat.SApp (_set_function2, _asortes1, _value_tuple0) ->
      SApp (unflatten_set_function _set_function2, option_map unflatten_asorte _asortes1, unflatten_value_tuple _value_tuple0)
  | Flat.SAssocOp (_setsetset_operator1, _set_expressions0) ->
      SAssocOp (unflatten_setsetset_operator _setsetset_operator1, List.map unflatten_set_expression _set_expressions0)
  | Flat.SConditional (_contrainte2, _set_expression1, _set_expression0) ->
      SConditional (unflatten_contrainte _contrainte2, unflatten_set_expression _set_expression1, unflatten_set_expression _set_expression0)

and free_raw_set_expression : raw_set_expression -> Datacon.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function raw_set_expression -> free_accu_raw_set_expression (Datacon.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) raw_set_expression

and equal_raw_set_expression : raw_set_expression -> raw_set_expression -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_raw_set_expression x1 x2

and aeq_raw_set_expression : unit -> raw_set_expression -> raw_set_expression -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | SEmpty, SEmpty ->
      ()
  | SUniverse, SUniverse ->
      ()
  | SSort (_sorte0), SSort (_sorte1) ->
      if not (Sorte.Atom.equal _sorte0 _sorte1) then raise (Invalid_argument "aeq_raw_set_expression");
      ()
  | SApp (_set_function2, _asortes1, _value_tuple0), SApp (_set_function5, _asortes4, _value_tuple3) ->
      aeq_set_function () _set_function2 _set_function5;
      option_fold2 aeq_asorte () _asortes1 _asortes4;
      aeq_value_tuple () _value_tuple0 _value_tuple3;
      ()
  | SAssocOp (_setsetset_operator1, _set_expressions0), SAssocOp (_setsetset_operator3, _set_expressions2) ->
      aeq_setsetset_operator () _setsetset_operator1 _setsetset_operator3;
      List.fold_left2 aeq_set_expression () _set_expressions0 _set_expressions2;
      ()
  | SConditional (_contrainte2, _set_expression1, _set_expression0), SConditional (_contrainte5, _set_expression4, _set_expression3) ->
      aeq_contrainte () _contrainte2 _contrainte5;
      aeq_set_expression () _set_expression1 _set_expression4;
      aeq_set_expression () _set_expression0 _set_expression3;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_raw_set_expression")

and free_accu_raw_set_expression = fun (datacon_fvars, sorte_fvars, var_fvars) -> function
  | SEmpty ->
      (datacon_fvars, sorte_fvars, var_fvars)
  | SUniverse ->
      (datacon_fvars, sorte_fvars, var_fvars)
  | SSort (_sorte0) ->
      let sorte_fvars = Sorte.AtomSet.add _sorte0 sorte_fvars in
      (datacon_fvars, sorte_fvars, var_fvars)
  | SApp (_set_function2, _asortes1, _value_tuple0) ->
      let () = free_accu_set_function () _set_function2 in 
      let (sorte_fvars) = option_fold free_accu_asorte (sorte_fvars) _asortes1 in 
      let (datacon_fvars, sorte_fvars, var_fvars) = free_accu_value_tuple (datacon_fvars, sorte_fvars, var_fvars) _value_tuple0 in 
      (datacon_fvars, sorte_fvars, var_fvars)
  | SAssocOp (_setsetset_operator1, _set_expressions0) ->
      let () = free_accu_setsetset_operator () _setsetset_operator1 in 
      let (datacon_fvars, sorte_fvars, var_fvars) = List.fold_left free_accu_set_expression (datacon_fvars, sorte_fvars, var_fvars) _set_expressions0 in 
      (datacon_fvars, sorte_fvars, var_fvars)
  | SConditional (_contrainte2, _set_expression1, _set_expression0) ->
      let (datacon_fvars, sorte_fvars, var_fvars) = free_accu_contrainte (datacon_fvars, sorte_fvars, var_fvars) _contrainte2 in 
      let (datacon_fvars, sorte_fvars, var_fvars) = free_accu_set_expression (datacon_fvars, sorte_fvars, var_fvars) _set_expression1 in 
      let (datacon_fvars, sorte_fvars, var_fvars) = free_accu_set_expression (datacon_fvars, sorte_fvars, var_fvars) _set_expression0 in 
      (datacon_fvars, sorte_fvars, var_fvars)

and import_set_expression : datacon Identifier.Map.t * sorte Identifier.Map.t * var Identifier.Map.t -> Raw.set_expression -> set_expression = fun (datacon_env, sorte_env, var_env) -> function
  (_raw_set_expressions0) ->
    (Annotation.map (import_raw_set_expression (datacon_env, sorte_env, var_env)) _raw_set_expressions0)

and subst_set_expression : Datacon.Subst.t * Sorte.Subst.t * Var.Subst.t -> set_expression -> set_expression = fun (datacon_env, sorte_env, var_env) -> function
  (_raw_set_expressions0) ->
    (Annotation.map (subst_raw_set_expression (datacon_env, sorte_env, var_env)) _raw_set_expressions0)

and export_set_expression : Datacon.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> set_expression -> Raw.set_expression = fun (datacon_m, sorte_m, var_m) -> function
  (_raw_set_expressions0) ->
    (Annotation.map (export_raw_set_expression (datacon_m, sorte_m, var_m)) _raw_set_expressions0)

and flatten_set_expression : set_expression -> Flat.set_expression = function
  (_raw_set_expressions0) ->
    (Annotation.map flatten_raw_set_expression _raw_set_expressions0)

and unflatten_set_expression : Flat.set_expression -> set_expression = function
  (_raw_set_expressions0) ->
    (Annotation.map unflatten_raw_set_expression _raw_set_expressions0)

and free_set_expression : set_expression -> Datacon.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function set_expression -> free_accu_set_expression (Datacon.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) set_expression

and equal_set_expression : set_expression -> set_expression -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_set_expression x1 x2

and aeq_set_expression : unit -> set_expression -> set_expression -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_raw_set_expressions0), (_raw_set_expressions1) ->
      Annotation.fold2 aeq_raw_set_expression () _raw_set_expressions0 _raw_set_expressions1;
      ()

and free_accu_set_expression = fun (datacon_fvars, sorte_fvars, var_fvars) -> function
  (_raw_set_expressions0) ->
      let (datacon_fvars, sorte_fvars, var_fvars) = Annotation.fold free_accu_raw_set_expression (datacon_fvars, sorte_fvars, var_fvars) _raw_set_expressions0 in 
      (datacon_fvars, sorte_fvars, var_fvars)

and import_setsetbool_operator : unit -> Raw.setsetbool_operator -> setsetbool_operator = fun () -> function
  | Raw.OpSubset ->
      OpSubset
  | Raw.OpEqual ->
      OpEqual
  | Raw.OpNotEqual ->
      OpNotEqual
  | Raw.OpDisjoint ->
      OpDisjoint

and subst_setsetbool_operator : unit -> setsetbool_operator -> setsetbool_operator = fun () -> function
  | OpSubset ->
      OpSubset
  | OpEqual ->
      OpEqual
  | OpNotEqual ->
      OpNotEqual
  | OpDisjoint ->
      OpDisjoint

and export_setsetbool_operator : unit -> setsetbool_operator -> Raw.setsetbool_operator = fun () -> function
  | OpSubset ->
      Raw.OpSubset
  | OpEqual ->
      Raw.OpEqual
  | OpNotEqual ->
      Raw.OpNotEqual
  | OpDisjoint ->
      Raw.OpDisjoint

and flatten_setsetbool_operator : setsetbool_operator -> Flat.setsetbool_operator = function
  | OpSubset ->
      Flat.OpSubset
  | OpEqual ->
      Flat.OpEqual
  | OpNotEqual ->
      Flat.OpNotEqual
  | OpDisjoint ->
      Flat.OpDisjoint

and unflatten_setsetbool_operator : Flat.setsetbool_operator -> setsetbool_operator = function
  | Flat.OpSubset ->
      OpSubset
  | Flat.OpEqual ->
      OpEqual
  | Flat.OpNotEqual ->
      OpNotEqual
  | Flat.OpDisjoint ->
      OpDisjoint

and free_setsetbool_operator : setsetbool_operator -> unit = 
  function setsetbool_operator -> free_accu_setsetbool_operator () setsetbool_operator

and equal_setsetbool_operator : setsetbool_operator -> setsetbool_operator -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_setsetbool_operator x1 x2

and aeq_setsetbool_operator : unit -> setsetbool_operator -> setsetbool_operator -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | OpSubset, OpSubset ->
      ()
  | OpEqual, OpEqual ->
      ()
  | OpNotEqual, OpNotEqual ->
      ()
  | OpDisjoint, OpDisjoint ->
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_setsetbool_operator")

and free_accu_setsetbool_operator = fun () -> function
  | OpSubset ->
      ()
  | OpEqual ->
      ()
  | OpNotEqual ->
      ()
  | OpDisjoint ->
      ()

and import_boolboolbool_operator : unit -> Raw.boolboolbool_operator -> boolboolbool_operator = fun () -> function
  | Raw.OpConjunction ->
      OpConjunction
  | Raw.OpDisjunction ->
      OpDisjunction
  | Raw.OpImplication ->
      OpImplication
  | Raw.OpEquivalence ->
      OpEquivalence

and subst_boolboolbool_operator : unit -> boolboolbool_operator -> boolboolbool_operator = fun () -> function
  | OpConjunction ->
      OpConjunction
  | OpDisjunction ->
      OpDisjunction
  | OpImplication ->
      OpImplication
  | OpEquivalence ->
      OpEquivalence

and export_boolboolbool_operator : unit -> boolboolbool_operator -> Raw.boolboolbool_operator = fun () -> function
  | OpConjunction ->
      Raw.OpConjunction
  | OpDisjunction ->
      Raw.OpDisjunction
  | OpImplication ->
      Raw.OpImplication
  | OpEquivalence ->
      Raw.OpEquivalence

and flatten_boolboolbool_operator : boolboolbool_operator -> Flat.boolboolbool_operator = function
  | OpConjunction ->
      Flat.OpConjunction
  | OpDisjunction ->
      Flat.OpDisjunction
  | OpImplication ->
      Flat.OpImplication
  | OpEquivalence ->
      Flat.OpEquivalence

and unflatten_boolboolbool_operator : Flat.boolboolbool_operator -> boolboolbool_operator = function
  | Flat.OpConjunction ->
      OpConjunction
  | Flat.OpDisjunction ->
      OpDisjunction
  | Flat.OpImplication ->
      OpImplication
  | Flat.OpEquivalence ->
      OpEquivalence

and free_boolboolbool_operator : boolboolbool_operator -> unit = 
  function boolboolbool_operator -> free_accu_boolboolbool_operator () boolboolbool_operator

and equal_boolboolbool_operator : boolboolbool_operator -> boolboolbool_operator -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_boolboolbool_operator x1 x2

and aeq_boolboolbool_operator : unit -> boolboolbool_operator -> boolboolbool_operator -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | OpConjunction, OpConjunction ->
      ()
  | OpDisjunction, OpDisjunction ->
      ()
  | OpImplication, OpImplication ->
      ()
  | OpEquivalence, OpEquivalence ->
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_boolboolbool_operator")

and free_accu_boolboolbool_operator = fun () -> function
  | OpConjunction ->
      ()
  | OpDisjunction ->
      ()
  | OpImplication ->
      ()
  | OpEquivalence ->
      ()

and import_raw_constraint : datacon Identifier.Map.t * sorte Identifier.Map.t * var Identifier.Map.t -> Raw.raw_constraint -> raw_constraint = fun (datacon_env, sorte_env, var_env) -> function
  | Raw.FTrue ->
      FTrue
  | Raw.FFalse ->
      FFalse
  | Raw.FBoolVar (_var0) ->
      FBoolVar (Var.find _var0 var_env)
  | Raw.FNot (_contrainte0) ->
      FNot ((import_contrainte (datacon_env, sorte_env, var_env)) _contrainte0)
  | Raw.FBoolAssocOp (_boolboolbool_operator1, _contraintes0) ->
      FBoolAssocOp ((import_boolboolbool_operator ()) _boolboolbool_operator1, List.map (import_contrainte (datacon_env, sorte_env, var_env)) _contraintes0)
  | Raw.FSetBinOp (_set_expression2, _setsetbool_operator1, _set_expression0) ->
      FSetBinOp ((import_set_expression (datacon_env, sorte_env, var_env)) _set_expression2, (import_setsetbool_operator ()) _setsetbool_operator1, (import_set_expression (datacon_env, sorte_env, var_env)) _set_expression0)

and subst_raw_constraint : Datacon.Subst.t * Sorte.Subst.t * Var.Subst.t -> raw_constraint -> raw_constraint = fun (datacon_env, sorte_env, var_env) -> function
  | FTrue ->
      FTrue
  | FFalse ->
      FFalse
  | FBoolVar (_var0) ->
      FBoolVar (Var.Subst.lookup _var0 var_env)
  | FNot (_contrainte0) ->
      FNot ((subst_contrainte (datacon_env, sorte_env, var_env)) _contrainte0)
  | FBoolAssocOp (_boolboolbool_operator1, _contraintes0) ->
      FBoolAssocOp ((subst_boolboolbool_operator ()) _boolboolbool_operator1, List.map (subst_contrainte (datacon_env, sorte_env, var_env)) _contraintes0)
  | FSetBinOp (_set_expression2, _setsetbool_operator1, _set_expression0) ->
      FSetBinOp ((subst_set_expression (datacon_env, sorte_env, var_env)) _set_expression2, (subst_setsetbool_operator ()) _setsetbool_operator1, (subst_set_expression (datacon_env, sorte_env, var_env)) _set_expression0)

and export_raw_constraint : Datacon.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> raw_constraint -> Raw.raw_constraint = fun (datacon_m, sorte_m, var_m) -> function
  | FTrue ->
      Raw.FTrue
  | FFalse ->
      Raw.FFalse
  | FBoolVar (_var0) ->
      Raw.FBoolVar (Var.AtomIdMap.lookup _var0 var_m)
  | FNot (_contrainte0) ->
      Raw.FNot ((export_contrainte (datacon_m, sorte_m, var_m)) _contrainte0)
  | FBoolAssocOp (_boolboolbool_operator1, _contraintes0) ->
      Raw.FBoolAssocOp ((export_boolboolbool_operator ()) _boolboolbool_operator1, List.map (export_contrainte (datacon_m, sorte_m, var_m)) _contraintes0)
  | FSetBinOp (_set_expression2, _setsetbool_operator1, _set_expression0) ->
      Raw.FSetBinOp ((export_set_expression (datacon_m, sorte_m, var_m)) _set_expression2, (export_setsetbool_operator ()) _setsetbool_operator1, (export_set_expression (datacon_m, sorte_m, var_m)) _set_expression0)

and flatten_raw_constraint : raw_constraint -> Flat.raw_constraint = function
  | FTrue ->
      Flat.FTrue
  | FFalse ->
      Flat.FFalse
  | FBoolVar (_var0) ->
      Flat.FBoolVar (_var0)
  | FNot (_contrainte0) ->
      Flat.FNot (flatten_contrainte _contrainte0)
  | FBoolAssocOp (_boolboolbool_operator1, _contraintes0) ->
      Flat.FBoolAssocOp (flatten_boolboolbool_operator _boolboolbool_operator1, List.map flatten_contrainte _contraintes0)
  | FSetBinOp (_set_expression2, _setsetbool_operator1, _set_expression0) ->
      Flat.FSetBinOp (flatten_set_expression _set_expression2, flatten_setsetbool_operator _setsetbool_operator1, flatten_set_expression _set_expression0)

and unflatten_raw_constraint : Flat.raw_constraint -> raw_constraint = function
  | Flat.FTrue ->
      FTrue
  | Flat.FFalse ->
      FFalse
  | Flat.FBoolVar (_var0) ->
      FBoolVar (_var0)
  | Flat.FNot (_contrainte0) ->
      FNot (unflatten_contrainte _contrainte0)
  | Flat.FBoolAssocOp (_boolboolbool_operator1, _contraintes0) ->
      FBoolAssocOp (unflatten_boolboolbool_operator _boolboolbool_operator1, List.map unflatten_contrainte _contraintes0)
  | Flat.FSetBinOp (_set_expression2, _setsetbool_operator1, _set_expression0) ->
      FSetBinOp (unflatten_set_expression _set_expression2, unflatten_setsetbool_operator _setsetbool_operator1, unflatten_set_expression _set_expression0)

and free_raw_constraint : raw_constraint -> Datacon.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function raw_constraint -> free_accu_raw_constraint (Datacon.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) raw_constraint

and equal_raw_constraint : raw_constraint -> raw_constraint -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_raw_constraint x1 x2

and aeq_raw_constraint : unit -> raw_constraint -> raw_constraint -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | FTrue, FTrue ->
      ()
  | FFalse, FFalse ->
      ()
  | FBoolVar (_var0), FBoolVar (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_raw_constraint");
      ()
  | FNot (_contrainte0), FNot (_contrainte1) ->
      aeq_contrainte () _contrainte0 _contrainte1;
      ()
  | FBoolAssocOp (_boolboolbool_operator1, _contraintes0), FBoolAssocOp (_boolboolbool_operator3, _contraintes2) ->
      aeq_boolboolbool_operator () _boolboolbool_operator1 _boolboolbool_operator3;
      List.fold_left2 aeq_contrainte () _contraintes0 _contraintes2;
      ()
  | FSetBinOp (_set_expression2, _setsetbool_operator1, _set_expression0), FSetBinOp (_set_expression5, _setsetbool_operator4, _set_expression3) ->
      aeq_set_expression () _set_expression2 _set_expression5;
      aeq_setsetbool_operator () _setsetbool_operator1 _setsetbool_operator4;
      aeq_set_expression () _set_expression0 _set_expression3;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_raw_constraint")

and free_accu_raw_constraint = fun (datacon_fvars, sorte_fvars, var_fvars) -> function
  | FTrue ->
      (datacon_fvars, sorte_fvars, var_fvars)
  | FFalse ->
      (datacon_fvars, sorte_fvars, var_fvars)
  | FBoolVar (_var0) ->
      let var_fvars = Var.AtomSet.add _var0 var_fvars in
      (datacon_fvars, sorte_fvars, var_fvars)
  | FNot (_contrainte0) ->
      let (datacon_fvars, sorte_fvars, var_fvars) = free_accu_contrainte (datacon_fvars, sorte_fvars, var_fvars) _contrainte0 in 
      (datacon_fvars, sorte_fvars, var_fvars)
  | FBoolAssocOp (_boolboolbool_operator1, _contraintes0) ->
      let () = free_accu_boolboolbool_operator () _boolboolbool_operator1 in 
      let (datacon_fvars, sorte_fvars, var_fvars) = List.fold_left free_accu_contrainte (datacon_fvars, sorte_fvars, var_fvars) _contraintes0 in 
      (datacon_fvars, sorte_fvars, var_fvars)
  | FSetBinOp (_set_expression2, _setsetbool_operator1, _set_expression0) ->
      let (datacon_fvars, sorte_fvars, var_fvars) = free_accu_set_expression (datacon_fvars, sorte_fvars, var_fvars) _set_expression2 in 
      let () = free_accu_setsetbool_operator () _setsetbool_operator1 in 
      let (datacon_fvars, sorte_fvars, var_fvars) = free_accu_set_expression (datacon_fvars, sorte_fvars, var_fvars) _set_expression0 in 
      (datacon_fvars, sorte_fvars, var_fvars)

and import_contrainte : datacon Identifier.Map.t * sorte Identifier.Map.t * var Identifier.Map.t -> Raw.contrainte -> contrainte = fun (datacon_env, sorte_env, var_env) -> function
  (_raw_constraints0) ->
    (Annotation.map (import_raw_constraint (datacon_env, sorte_env, var_env)) _raw_constraints0)

and subst_contrainte : Datacon.Subst.t * Sorte.Subst.t * Var.Subst.t -> contrainte -> contrainte = fun (datacon_env, sorte_env, var_env) -> function
  (_raw_constraints0) ->
    (Annotation.map (subst_raw_constraint (datacon_env, sorte_env, var_env)) _raw_constraints0)

and export_contrainte : Datacon.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> contrainte -> Raw.contrainte = fun (datacon_m, sorte_m, var_m) -> function
  (_raw_constraints0) ->
    (Annotation.map (export_raw_constraint (datacon_m, sorte_m, var_m)) _raw_constraints0)

and flatten_contrainte : contrainte -> Flat.contrainte = function
  (_raw_constraints0) ->
    (Annotation.map flatten_raw_constraint _raw_constraints0)

and unflatten_contrainte : Flat.contrainte -> contrainte = function
  (_raw_constraints0) ->
    (Annotation.map unflatten_raw_constraint _raw_constraints0)

and free_contrainte : contrainte -> Datacon.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function contrainte -> free_accu_contrainte (Datacon.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) contrainte

and equal_contrainte : contrainte -> contrainte -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_contrainte x1 x2

and aeq_contrainte : unit -> contrainte -> contrainte -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_raw_constraints0), (_raw_constraints1) ->
      Annotation.fold2 aeq_raw_constraint () _raw_constraints0 _raw_constraints1;
      ()

and free_accu_contrainte = fun (datacon_fvars, sorte_fvars, var_fvars) -> function
  (_raw_constraints0) ->
      let (datacon_fvars, sorte_fvars, var_fvars) = Annotation.fold free_accu_raw_constraint (datacon_fvars, sorte_fvars, var_fvars) _raw_constraints0 in 
      (datacon_fvars, sorte_fvars, var_fvars)

and import_raw_type : datatype Identifier.Map.t * sorte Identifier.Map.t -> Raw.raw_type -> raw_type = fun (datatype_env, sorte_env) -> function
  | Raw.TAtom (_sorte0) ->
      TAtom (Sorte.find _sorte0 sorte_env)
  | Raw.TAtomSet (_sorte0) ->
      TAtomSet (Sorte.find _sorte0 sorte_env)
  | Raw.TBool ->
      TBool
  | Raw.TData (_datatype0) ->
      TData (Datatype.find _datatype0 datatype_env)

and subst_raw_type : Datatype.Subst.t * Sorte.Subst.t -> raw_type -> raw_type = fun (datatype_env, sorte_env) -> function
  | TAtom (_sorte0) ->
      TAtom (Sorte.Subst.lookup _sorte0 sorte_env)
  | TAtomSet (_sorte0) ->
      TAtomSet (Sorte.Subst.lookup _sorte0 sorte_env)
  | TBool ->
      TBool
  | TData (_datatype0) ->
      TData (Datatype.Subst.lookup _datatype0 datatype_env)

and export_raw_type : Datatype.AtomIdMap.t * Sorte.AtomIdMap.t -> raw_type -> Raw.raw_type = fun (datatype_m, sorte_m) -> function
  | TAtom (_sorte0) ->
      Raw.TAtom (Sorte.AtomIdMap.lookup _sorte0 sorte_m)
  | TAtomSet (_sorte0) ->
      Raw.TAtomSet (Sorte.AtomIdMap.lookup _sorte0 sorte_m)
  | TBool ->
      Raw.TBool
  | TData (_datatype0) ->
      Raw.TData (Datatype.AtomIdMap.lookup _datatype0 datatype_m)

and flatten_raw_type : raw_type -> Flat.raw_type = function
  | TAtom (_sorte0) ->
      Flat.TAtom (_sorte0)
  | TAtomSet (_sorte0) ->
      Flat.TAtomSet (_sorte0)
  | TBool ->
      Flat.TBool
  | TData (_datatype0) ->
      Flat.TData (_datatype0)

and unflatten_raw_type : Flat.raw_type -> raw_type = function
  | Flat.TAtom (_sorte0) ->
      TAtom (_sorte0)
  | Flat.TAtomSet (_sorte0) ->
      TAtomSet (_sorte0)
  | Flat.TBool ->
      TBool
  | Flat.TData (_datatype0) ->
      TData (_datatype0)

and free_raw_type : raw_type -> Datatype.AtomSet.t * Sorte.AtomSet.t = 
  function raw_type -> free_accu_raw_type (Datatype.AtomSet.empty, Sorte.AtomSet.empty) raw_type

and equal_raw_type : raw_type -> raw_type -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_raw_type x1 x2

and aeq_raw_type : unit -> raw_type -> raw_type -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | TAtom (_sorte0), TAtom (_sorte1) ->
      if not (Sorte.Atom.equal _sorte0 _sorte1) then raise (Invalid_argument "aeq_raw_type");
      ()
  | TAtomSet (_sorte0), TAtomSet (_sorte1) ->
      if not (Sorte.Atom.equal _sorte0 _sorte1) then raise (Invalid_argument "aeq_raw_type");
      ()
  | TBool, TBool ->
      ()
  | TData (_datatype0), TData (_datatype1) ->
      if not (Datatype.Atom.equal _datatype0 _datatype1) then raise (Invalid_argument "aeq_raw_type");
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_raw_type")

and free_accu_raw_type = fun (datatype_fvars, sorte_fvars) -> function
  | TAtom (_sorte0) ->
      let sorte_fvars = Sorte.AtomSet.add _sorte0 sorte_fvars in
      (datatype_fvars, sorte_fvars)
  | TAtomSet (_sorte0) ->
      let sorte_fvars = Sorte.AtomSet.add _sorte0 sorte_fvars in
      (datatype_fvars, sorte_fvars)
  | TBool ->
      (datatype_fvars, sorte_fvars)
  | TData (_datatype0) ->
      let datatype_fvars = Datatype.AtomSet.add _datatype0 datatype_fvars in
      (datatype_fvars, sorte_fvars)

and import_typ : datatype Identifier.Map.t * sorte Identifier.Map.t -> Raw.typ -> typ = fun (datatype_env, sorte_env) -> function
  (_raw_types0) ->
    (Annotation.map (import_raw_type (datatype_env, sorte_env)) _raw_types0)

and subst_typ : Datatype.Subst.t * Sorte.Subst.t -> typ -> typ = fun (datatype_env, sorte_env) -> function
  (_raw_types0) ->
    (Annotation.map (subst_raw_type (datatype_env, sorte_env)) _raw_types0)

and export_typ : Datatype.AtomIdMap.t * Sorte.AtomIdMap.t -> typ -> Raw.typ = fun (datatype_m, sorte_m) -> function
  (_raw_types0) ->
    (Annotation.map (export_raw_type (datatype_m, sorte_m)) _raw_types0)

and flatten_typ : typ -> Flat.typ = function
  (_raw_types0) ->
    (Annotation.map flatten_raw_type _raw_types0)

and unflatten_typ : Flat.typ -> typ = function
  (_raw_types0) ->
    (Annotation.map unflatten_raw_type _raw_types0)

and free_typ : typ -> Datatype.AtomSet.t * Sorte.AtomSet.t = 
  function typ -> free_accu_typ (Datatype.AtomSet.empty, Sorte.AtomSet.empty) typ

and equal_typ : typ -> typ -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_typ x1 x2

and aeq_typ : unit -> typ -> typ -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_raw_types0), (_raw_types1) ->
      Annotation.fold2 aeq_raw_type () _raw_types0 _raw_types1;
      ()

and free_accu_typ = fun (datatype_fvars, sorte_fvars) -> function
  (_raw_types0) ->
      let (datatype_fvars, sorte_fvars) = Annotation.fold free_accu_raw_type (datatype_fvars, sorte_fvars) _raw_types0 in 
      (datatype_fvars, sorte_fvars)

and subst_raw_tuple : Datatype.Subst.t * Sorte.Subst.t * Var.Subst.t -> raw_tuple -> raw_tuple = fun (datatype_oenv, sorte_oenv, var_ienv) -> function
  | TComponent (_typ0) ->
      TComponent ((subst_typ (datatype_oenv, sorte_oenv)) _typ0)
  | TInner (_sorts1, _tuple0) ->
      TInner ((subst_sorts (sorte_oenv)) _sorts1, (subst_tuple (datatype_oenv, sorte_oenv, var_ienv)) _tuple0)
  | TOuter (_sorts1, _tuple0) ->
      TOuter ((subst_sorts (sorte_oenv)) _sorts1, (subst_tuple (datatype_oenv, sorte_oenv, var_ienv)) _tuple0)
  | TAbstraction (_sorts1, _tuple0) ->
      TAbstraction ((subst_sorts (sorte_oenv)) _sorts1, (subst_tuple (datatype_oenv, sorte_oenv, var_ienv)) _tuple0)
  | TTuple (_tuples0) ->
      TTuple (List.map (subst_tuple (datatype_oenv, sorte_oenv, var_ienv)) _tuples0)
  | TName (_var1, _tuple0) ->
      TName (Var.Subst.lookup _var1 var_ienv, (subst_tuple (datatype_oenv, sorte_oenv, var_ienv)) _tuple0)

and bound_raw_tuple : raw_tuple -> Var.AtomSet.t = 
  function raw_tuple -> bound_accu_raw_tuple (Var.AtomSet.empty) raw_tuple

and bound_free_raw_tuple : raw_tuple -> Var.AtomSet.t * Datatype.AtomSet.t * Sorte.AtomSet.t = 
  function raw_tuple -> bound_free_accu_raw_tuple (Var.AtomSet.empty, Datatype.AtomSet.empty, Sorte.AtomSet.empty) raw_tuple

and equal_raw_tuple : raw_tuple -> raw_tuple -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_raw_tuple x1 x2

and import_raw_tuple = fun (datatype_oenv, sorte_oenv, var_ienv) -> function
  | Raw.TComponent (_typ0) ->
      TComponent ((import_typ (datatype_oenv, sorte_oenv)) _typ0)
  | Raw.TInner (_sorts1, _tuple0) ->
      TInner ((import_sorts (sorte_oenv)) _sorts1, (import_tuple (datatype_oenv, sorte_oenv, var_ienv)) _tuple0)
  | Raw.TOuter (_sorts1, _tuple0) ->
      TOuter ((import_sorts (sorte_oenv)) _sorts1, (import_tuple (datatype_oenv, sorte_oenv, var_ienv)) _tuple0)
  | Raw.TAbstraction (_sorts1, _tuple0) ->
      TAbstraction ((import_sorts (sorte_oenv)) _sorts1, (import_tuple (datatype_oenv, sorte_oenv, var_ienv)) _tuple0)
  | Raw.TTuple (_tuples0) ->
      TTuple (List.map (import_tuple (datatype_oenv, sorte_oenv, var_ienv)) _tuples0)
  | Raw.TName (_var1, _tuple0) ->
      TName (Var.find _var1 var_ienv, (import_tuple (datatype_oenv, sorte_oenv, var_ienv)) _tuple0)

and bvi_accu_raw_tuple = fun (var_bvars) -> function
  | Raw.TComponent (_typ0) ->
      (var_bvars)
  | Raw.TInner (_sorts1, _tuple0) ->
      let (var_bvars) = bvi_accu_tuple (var_bvars) _tuple0 in
      (var_bvars)
  | Raw.TOuter (_sorts1, _tuple0) ->
      let (var_bvars) = bvi_accu_tuple (var_bvars) _tuple0 in
      (var_bvars)
  | Raw.TAbstraction (_sorts1, _tuple0) ->
      let (var_bvars) = bvi_accu_tuple (var_bvars) _tuple0 in
      (var_bvars)
  | Raw.TTuple (_tuples0) ->
      let (var_bvars) = List.fold_left bvi_accu_tuple (var_bvars) _tuples0 in
      (var_bvars)
  | Raw.TName (_var1, _tuple0) ->
      let var_bvars = Identifier.Map.add _var1 () var_bvars in
      let (var_bvars) = bvi_accu_tuple (var_bvars) _tuple0 in
      (var_bvars)

and bvi_raw_tuple = 
  function raw_tuple -> bvi_accu_raw_tuple (Identifier.Map.empty) raw_tuple

and bound_accu_raw_tuple = fun (var_bvars) -> function
  | TComponent (_typ0) ->
      (var_bvars)
  | TInner (_sorts1, _tuple0) ->
      let (var_bvars) = bound_accu_tuple (var_bvars) _tuple0 in
      (var_bvars)
  | TOuter (_sorts1, _tuple0) ->
      let (var_bvars) = bound_accu_tuple (var_bvars) _tuple0 in
      (var_bvars)
  | TAbstraction (_sorts1, _tuple0) ->
      let (var_bvars) = bound_accu_tuple (var_bvars) _tuple0 in
      (var_bvars)
  | TTuple (_tuples0) ->
      let (var_bvars) = List.fold_left bound_accu_tuple (var_bvars) _tuples0 in
      (var_bvars)
  | TName (_var1, _tuple0) ->
      let var_bvars = Var.AtomSet.add _var1 var_bvars in
      let (var_bvars) = bound_accu_tuple (var_bvars) _tuple0 in
      (var_bvars)

and export_raw_tuple : Datatype.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> raw_tuple -> Raw.raw_tuple = fun (datatype_om, sorte_om, var_im) -> function
  | TComponent (_typ0) ->
      Raw.TComponent ((export_typ (datatype_om, sorte_om)) _typ0)
  | TInner (_sorts1, _tuple0) ->
      Raw.TInner ((export_sorts (sorte_om)) _sorts1, (export_tuple (datatype_om, sorte_om, var_im)) _tuple0)
  | TOuter (_sorts1, _tuple0) ->
      Raw.TOuter ((export_sorts (sorte_om)) _sorts1, (export_tuple (datatype_om, sorte_om, var_im)) _tuple0)
  | TAbstraction (_sorts1, _tuple0) ->
      Raw.TAbstraction ((export_sorts (sorte_om)) _sorts1, (export_tuple (datatype_om, sorte_om, var_im)) _tuple0)
  | TTuple (_tuples0) ->
      Raw.TTuple (List.map (export_tuple (datatype_om, sorte_om, var_im)) _tuples0)
  | TName (_var1, _tuple0) ->
      Raw.TName (Var.AtomIdMap.lookup _var1 var_im, (export_tuple (datatype_om, sorte_om, var_im)) _tuple0)

and flatten_raw_tuple : raw_tuple -> Flat.raw_tuple = function
  | TComponent (_typ0) ->
      Flat.TComponent (flatten_typ _typ0)
  | TInner (_sorts1, _tuple0) ->
      Flat.TInner (flatten_sorts _sorts1, flatten_tuple _tuple0)
  | TOuter (_sorts1, _tuple0) ->
      Flat.TOuter (flatten_sorts _sorts1, flatten_tuple _tuple0)
  | TAbstraction (_sorts1, _tuple0) ->
      Flat.TAbstraction (flatten_sorts _sorts1, flatten_tuple _tuple0)
  | TTuple (_tuples0) ->
      Flat.TTuple (List.map flatten_tuple _tuples0)
  | TName (_var1, _tuple0) ->
      Flat.TName (_var1, flatten_tuple _tuple0)

and unflatten_raw_tuple : Flat.raw_tuple -> raw_tuple = function
  | Flat.TComponent (_typ0) ->
      TComponent (unflatten_typ _typ0)
  | Flat.TInner (_sorts1, _tuple0) ->
      TInner (unflatten_sorts _sorts1, unflatten_tuple _tuple0)
  | Flat.TOuter (_sorts1, _tuple0) ->
      TOuter (unflatten_sorts _sorts1, unflatten_tuple _tuple0)
  | Flat.TAbstraction (_sorts1, _tuple0) ->
      TAbstraction (unflatten_sorts _sorts1, unflatten_tuple _tuple0)
  | Flat.TTuple (_tuples0) ->
      TTuple (List.map unflatten_tuple _tuples0)
  | Flat.TName (_var1, _tuple0) ->
      TName (_var1, unflatten_tuple _tuple0)

and bound_free_accu_raw_tuple = fun (var_bvars, datatype_ofvars, sorte_ofvars) -> function
  | TComponent (_typ0) ->
      let (datatype_ofvars, sorte_ofvars) = free_accu_typ (datatype_ofvars, sorte_ofvars) _typ0 in
      (var_bvars, datatype_ofvars, sorte_ofvars)
  | TInner (_sorts1, _tuple0) ->
      let (sorte_ofvars) = free_accu_sorts (sorte_ofvars) _sorts1 in
      let (var_bvars, datatype_ofvars, sorte_ofvars) = bound_free_accu_tuple (var_bvars, datatype_ofvars, sorte_ofvars) _tuple0 in
      (var_bvars, datatype_ofvars, sorte_ofvars)
  | TOuter (_sorts1, _tuple0) ->
      let (sorte_ofvars) = free_accu_sorts (sorte_ofvars) _sorts1 in
      let (var_bvars, datatype_ofvars, sorte_ofvars) = bound_free_accu_tuple (var_bvars, datatype_ofvars, sorte_ofvars) _tuple0 in
      (var_bvars, datatype_ofvars, sorte_ofvars)
  | TAbstraction (_sorts1, _tuple0) ->
      let (sorte_ofvars) = free_accu_sorts (sorte_ofvars) _sorts1 in
      let (var_bvars, datatype_ofvars, sorte_ofvars) = bound_free_accu_tuple (var_bvars, datatype_ofvars, sorte_ofvars) _tuple0 in
      (var_bvars, datatype_ofvars, sorte_ofvars)
  | TTuple (_tuples0) ->
      let (var_bvars, datatype_ofvars, sorte_ofvars) = List.fold_left bound_free_accu_tuple (var_bvars, datatype_ofvars, sorte_ofvars) _tuples0 in
      (var_bvars, datatype_ofvars, sorte_ofvars)
  | TName (_var1, _tuple0) ->
      let var_bvars = Var.AtomSet.add _var1 var_bvars in
      let (var_bvars, datatype_ofvars, sorte_ofvars) = bound_free_accu_tuple (var_bvars, datatype_ofvars, sorte_ofvars) _tuple0 in
      (var_bvars, datatype_ofvars, sorte_ofvars)

and aeq_raw_tuple : unit -> raw_tuple -> raw_tuple -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | TComponent (_typ0), TComponent (_typ1) ->
      aeq_typ () _typ0 _typ1;
      ()
  | TInner (_sorts1, _tuple0), TInner (_sorts3, _tuple2) ->
      aeq_sorts () _sorts1 _sorts3;
      aeq_tuple () _tuple0 _tuple2;
      ()
  | TOuter (_sorts1, _tuple0), TOuter (_sorts3, _tuple2) ->
      aeq_sorts () _sorts1 _sorts3;
      aeq_tuple () _tuple0 _tuple2;
      ()
  | TAbstraction (_sorts1, _tuple0), TAbstraction (_sorts3, _tuple2) ->
      aeq_sorts () _sorts1 _sorts3;
      aeq_tuple () _tuple0 _tuple2;
      ()
  | TTuple (_tuples0), TTuple (_tuples1) ->
      List.fold_left2 aeq_tuple () _tuples0 _tuples1;
      ()
  | TName (_var1, _tuple0), TName (_var3, _tuple2) ->
      if not (Var.Atom.equal _var1 _var3) then raise (Invalid_argument "aeq_raw_tuple");
      aeq_tuple () _tuple0 _tuple2;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_raw_tuple")

and freshen2_raw_tuple : Var.Subst.t * Var.Subst.t -> raw_tuple -> raw_tuple -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | TComponent (_typ0), TComponent (_typ1) ->
      (var_env1, var_env2)
  | TInner (_sorts1, _tuple0), TInner (_sorts3, _tuple2) ->
      let (var_env1, var_env2) = freshen2_tuple (var_env1, var_env2) _tuple0 _tuple2 in
      (var_env1, var_env2)
  | TOuter (_sorts1, _tuple0), TOuter (_sorts3, _tuple2) ->
      let (var_env1, var_env2) = freshen2_tuple (var_env1, var_env2) _tuple0 _tuple2 in
      (var_env1, var_env2)
  | TAbstraction (_sorts1, _tuple0), TAbstraction (_sorts3, _tuple2) ->
      let (var_env1, var_env2) = freshen2_tuple (var_env1, var_env2) _tuple0 _tuple2 in
      (var_env1, var_env2)
  | TTuple (_tuples0), TTuple (_tuples1) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_tuple (var_env1, var_env2) _tuples0 _tuples1 in
      (var_env1, var_env2)
  | TName (_var1, _tuple0), TName (_var3, _tuple2) ->
      let var_env1, var_env2 = Var.Subst.freshen2 _var1 var_env1 _var3 var_env2 in
      let (var_env1, var_env2) = freshen2_tuple (var_env1, var_env2) _tuple0 _tuple2 in
      (var_env1, var_env2)
  | _, _ ->
      raise (Invalid_argument "freshen2_raw_tuple")

and subst_tuple : Datatype.Subst.t * Sorte.Subst.t * Var.Subst.t -> tuple -> tuple = fun (datatype_oenv, sorte_oenv, var_ienv) -> function
  (_raw_tuples0) ->
    (Annotation.map (subst_raw_tuple (datatype_oenv, sorte_oenv, var_ienv)) _raw_tuples0)

and bound_tuple : tuple -> Var.AtomSet.t = 
  function tuple -> bound_accu_tuple (Var.AtomSet.empty) tuple

and bound_free_tuple : tuple -> Var.AtomSet.t * Datatype.AtomSet.t * Sorte.AtomSet.t = 
  function tuple -> bound_free_accu_tuple (Var.AtomSet.empty, Datatype.AtomSet.empty, Sorte.AtomSet.empty) tuple

and equal_tuple : tuple -> tuple -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_tuple x1 x2

and import_tuple = fun (datatype_oenv, sorte_oenv, var_ienv) -> function
  (_raw_tuples0) ->
    (Annotation.map (import_raw_tuple (datatype_oenv, sorte_oenv, var_ienv)) _raw_tuples0)

and bvi_accu_tuple = fun (var_bvars) -> function
  (_raw_tuples0) ->
      let (var_bvars) = Annotation.fold bvi_accu_raw_tuple (var_bvars) _raw_tuples0 in
      (var_bvars)

and bvi_tuple = 
  function tuple -> bvi_accu_tuple (Identifier.Map.empty) tuple

and bound_accu_tuple = fun (var_bvars) -> function
  (_raw_tuples0) ->
      let (var_bvars) = Annotation.fold bound_accu_raw_tuple (var_bvars) _raw_tuples0 in
      (var_bvars)

and export_tuple : Datatype.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> tuple -> Raw.tuple = fun (datatype_om, sorte_om, var_im) -> function
  (_raw_tuples0) ->
    (Annotation.map (export_raw_tuple (datatype_om, sorte_om, var_im)) _raw_tuples0)

and flatten_tuple : tuple -> Flat.tuple = function
  (_raw_tuples0) ->
    (Annotation.map flatten_raw_tuple _raw_tuples0)

and unflatten_tuple : Flat.tuple -> tuple = function
  (_raw_tuples0) ->
    (Annotation.map unflatten_raw_tuple _raw_tuples0)

and bound_free_accu_tuple = fun (var_bvars, datatype_ofvars, sorte_ofvars) -> function
  (_raw_tuples0) ->
      let (var_bvars, datatype_ofvars, sorte_ofvars) = Annotation.fold bound_free_accu_raw_tuple (var_bvars, datatype_ofvars, sorte_ofvars) _raw_tuples0 in
      (var_bvars, datatype_ofvars, sorte_ofvars)

and aeq_tuple : unit -> tuple -> tuple -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_raw_tuples0), (_raw_tuples1) ->
      Annotation.fold2 aeq_raw_tuple () _raw_tuples0 _raw_tuples1;
      ()

and freshen2_tuple : Var.Subst.t * Var.Subst.t -> tuple -> tuple -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_raw_tuples0), (_raw_tuples1) ->
      let (var_env1, var_env2) = Annotation.fold2 freshen2_raw_tuple (var_env1, var_env2) _raw_tuples0 _raw_tuples1 in
      (var_env1, var_env2)

and subst_guarded_tuple : Datacon.Subst.t * Datatype.Subst.t * Sorte.Subst.t * Var.Subst.t -> guarded_tuple -> guarded_tuple = fun (datacon_oenv, datatype_oenv, sorte_oenv, var_ienv) -> function
  (_tuple1, _contrainte0) ->
    ((subst_tuple (datatype_oenv, sorte_oenv, var_ienv)) _tuple1, (subst_contrainte (datacon_oenv, sorte_oenv, var_ienv)) _contrainte0)

and bound_guarded_tuple : guarded_tuple -> Var.AtomSet.t = 
  function guarded_tuple -> bound_accu_guarded_tuple (Var.AtomSet.empty) guarded_tuple

and bound_free_guarded_tuple : guarded_tuple -> Var.AtomSet.t * Var.AtomSet.t * Datacon.AtomSet.t * Datatype.AtomSet.t * Sorte.AtomSet.t = 
  function guarded_tuple -> bound_free_accu_guarded_tuple (Var.AtomSet.empty, Var.AtomSet.empty, Datacon.AtomSet.empty, Datatype.AtomSet.empty, Sorte.AtomSet.empty) guarded_tuple

and equal_guarded_tuple : guarded_tuple -> guarded_tuple -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_guarded_tuple x1 x2

and import_guarded_tuple = fun (datacon_oenv, datatype_oenv, sorte_oenv, var_ienv) -> function
  (_tuple1, _contrainte0) ->
    ((import_tuple (datatype_oenv, sorte_oenv, var_ienv)) _tuple1, (import_contrainte (datacon_oenv, sorte_oenv, var_ienv)) _contrainte0)

and bvi_accu_guarded_tuple = fun (var_bvars) -> function
  (_tuple1, _contrainte0) ->
      let (var_bvars) = bvi_accu_tuple (var_bvars) _tuple1 in
      (var_bvars)

and bvi_guarded_tuple = 
  function guarded_tuple -> bvi_accu_guarded_tuple (Identifier.Map.empty) guarded_tuple

and bound_accu_guarded_tuple = fun (var_bvars) -> function
  (_tuple1, _contrainte0) ->
      let (var_bvars) = bound_accu_tuple (var_bvars) _tuple1 in
      (var_bvars)

and export_guarded_tuple : Datacon.AtomIdMap.t * Datatype.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> guarded_tuple -> Raw.guarded_tuple = fun (datacon_om, datatype_om, sorte_om, var_im) -> function
  (_tuple1, _contrainte0) ->
    ((export_tuple (datatype_om, sorte_om, var_im)) _tuple1, (export_contrainte (datacon_om, sorte_om, var_im)) _contrainte0)

and flatten_guarded_tuple : guarded_tuple -> Flat.guarded_tuple = function
  (_tuple1, _contrainte0) ->
    (flatten_tuple _tuple1, flatten_contrainte _contrainte0)

and unflatten_guarded_tuple : Flat.guarded_tuple -> guarded_tuple = function
  (_tuple1, _contrainte0) ->
    (unflatten_tuple _tuple1, unflatten_contrainte _contrainte0)

and bound_free_accu_guarded_tuple = fun (var_bvars, var_ifvars, datacon_ofvars, datatype_ofvars, sorte_ofvars) -> function
  (_tuple1, _contrainte0) ->
      let (var_bvars, datatype_ofvars, sorte_ofvars) = bound_free_accu_tuple (var_bvars, datatype_ofvars, sorte_ofvars) _tuple1 in
      let (datacon_ofvars, sorte_ofvars, var_ifvars) = free_accu_contrainte (datacon_ofvars, sorte_ofvars, var_ifvars) _contrainte0 in
      (var_bvars, var_ifvars, datacon_ofvars, datatype_ofvars, sorte_ofvars)

and aeq_guarded_tuple : unit -> guarded_tuple -> guarded_tuple -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_tuple1, _contrainte0), (_tuple3, _contrainte2) ->
      aeq_tuple () _tuple1 _tuple3;
      aeq_contrainte () _contrainte0 _contrainte2;
      ()

and freshen2_guarded_tuple : Var.Subst.t * Var.Subst.t -> guarded_tuple -> guarded_tuple -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_tuple1, _contrainte0), (_tuple3, _contrainte2) ->
      let (var_env1, var_env2) = freshen2_tuple (var_env1, var_env2) _tuple1 _tuple3 in
      (var_env1, var_env2)

and create_guarded_tuple : guarded_tuple -> opaque_guarded_tuple = 
  function body -> {
    guarded_tuple_delayed = (Datacon.Subst.id, Datatype.Subst.id, Sorte.Subst.id, Var.Subst.id);
    guarded_tuple = body
  }

and open_guarded_tuple : opaque_guarded_tuple -> guarded_tuple = function abstraction ->
  let (datacon_delayed, datatype_delayed, sorte_delayed, var_delayed) = abstraction.guarded_tuple_delayed in
  let body = abstraction.guarded_tuple in
  let (var_bvars) = bound_guarded_tuple body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_guarded_tuple (datacon_delayed, datatype_delayed, sorte_delayed, var_env) body in
  if not (Datacon.Subst.is_id datacon_delayed && Datatype.Subst.is_id datatype_delayed && Sorte.Subst.is_id sorte_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.guarded_tuple_delayed <- (Datacon.Subst.id, Datatype.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction.guarded_tuple <- body
  end;
  body

and open2_guarded_tuple : opaque_guarded_tuple -> opaque_guarded_tuple -> guarded_tuple * guarded_tuple = fun x1 x2 -> 
  change_invalid_to_open2 open2i_guarded_tuple x1 x2

and open2i_guarded_tuple : opaque_guarded_tuple -> opaque_guarded_tuple -> guarded_tuple * guarded_tuple = fun abstraction1 abstraction2 ->
  let (datacon_delayed1, datatype_delayed1, sorte_delayed1, var_delayed1) = abstraction1.guarded_tuple_delayed in
  let body1 = abstraction1.guarded_tuple in
  let (datacon_delayed2, datatype_delayed2, sorte_delayed2, var_delayed2) = abstraction2.guarded_tuple_delayed in
  let body2 = abstraction2.guarded_tuple in
  let (var_env1, var_env2) = freshen2_guarded_tuple (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_guarded_tuple (datacon_delayed1, datatype_delayed1, sorte_delayed1, var_env1) body1 in
  let body2 = subst_guarded_tuple (datacon_delayed2, datatype_delayed2, sorte_delayed2, var_env2) body2 in
  if not (Datacon.Subst.is_id datacon_delayed1 && Datatype.Subst.is_id datatype_delayed1 && Sorte.Subst.is_id sorte_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.guarded_tuple_delayed <- (Datacon.Subst.id, Datatype.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction1.guarded_tuple <- body1
  end;
  if not (Datacon.Subst.is_id datacon_delayed2 && Datatype.Subst.is_id datatype_delayed2 && Sorte.Subst.is_id sorte_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.guarded_tuple_delayed <- (Datacon.Subst.id, Datatype.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction2.guarded_tuple <- body2
  end;
  body1, body2

and apply_guarded_tuple = 
  fun (datacon_env, datatype_env, sorte_env, var_env) abstraction ->
    let (datacon_delayed, datatype_delayed, sorte_delayed, var_delayed) = abstraction.guarded_tuple_delayed in {
      abstraction with guarded_tuple_delayed = (Datacon.Subst.compose datacon_env datacon_delayed, Datatype.Subst.compose datatype_env datatype_delayed, Sorte.Subst.compose sorte_env sorte_delayed, Var.Subst.compose var_env var_delayed)
    }

and subst_guarded_params : Datacon.Subst.t * Sorte.Subst.t * Var.Subst.t -> guarded_params -> guarded_params = fun (datacon_oenv, sorte_oenv, var_ienv) -> function
  (_avaros1, _contrainte0) ->
    (List.map (subst_avaro (var_ienv)) _avaros1, (subst_contrainte (datacon_oenv, sorte_oenv, var_ienv)) _contrainte0)

and bound_guarded_params : guarded_params -> Var.AtomSet.t = 
  function guarded_params -> bound_accu_guarded_params (Var.AtomSet.empty) guarded_params

and bound_free_guarded_params : guarded_params -> Var.AtomSet.t * Var.AtomSet.t * Datacon.AtomSet.t * Sorte.AtomSet.t = 
  function guarded_params -> bound_free_accu_guarded_params (Var.AtomSet.empty, Var.AtomSet.empty, Datacon.AtomSet.empty, Sorte.AtomSet.empty) guarded_params

and equal_guarded_params : guarded_params -> guarded_params -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_guarded_params x1 x2

and import_guarded_params = fun (datacon_oenv, sorte_oenv, var_ienv) -> function
  (_avaros1, _contrainte0) ->
    (List.map (import_avaro (var_ienv)) _avaros1, (import_contrainte (datacon_oenv, sorte_oenv, var_ienv)) _contrainte0)

and bvi_accu_guarded_params = fun (var_bvars) -> function
  (_avaros1, _contrainte0) ->
      let (var_bvars) = List.fold_left bvi_accu_avaro (var_bvars) _avaros1 in
      (var_bvars)

and bvi_guarded_params = 
  function guarded_params -> bvi_accu_guarded_params (Identifier.Map.empty) guarded_params

and bound_accu_guarded_params = fun (var_bvars) -> function
  (_avaros1, _contrainte0) ->
      let (var_bvars) = List.fold_left bound_accu_avaro (var_bvars) _avaros1 in
      (var_bvars)

and export_guarded_params : Datacon.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> guarded_params -> Raw.guarded_params = fun (datacon_om, sorte_om, var_im) -> function
  (_avaros1, _contrainte0) ->
    (List.map (export_avaro (var_im)) _avaros1, (export_contrainte (datacon_om, sorte_om, var_im)) _contrainte0)

and flatten_guarded_params : guarded_params -> Flat.guarded_params = function
  (_avaros1, _contrainte0) ->
    (List.map flatten_avaro _avaros1, flatten_contrainte _contrainte0)

and unflatten_guarded_params : Flat.guarded_params -> guarded_params = function
  (_avaros1, _contrainte0) ->
    (List.map unflatten_avaro _avaros1, unflatten_contrainte _contrainte0)

and bound_free_accu_guarded_params = fun (var_bvars, var_ifvars, datacon_ofvars, sorte_ofvars) -> function
  (_avaros1, _contrainte0) ->
      let (var_bvars) = List.fold_left bound_free_accu_avaro (var_bvars) _avaros1 in
      let (datacon_ofvars, sorte_ofvars, var_ifvars) = free_accu_contrainte (datacon_ofvars, sorte_ofvars, var_ifvars) _contrainte0 in
      (var_bvars, var_ifvars, datacon_ofvars, sorte_ofvars)

and aeq_guarded_params : unit -> guarded_params -> guarded_params -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_avaros1, _contrainte0), (_avaros3, _contrainte2) ->
      List.fold_left2 aeq_avaro () _avaros1 _avaros3;
      aeq_contrainte () _contrainte0 _contrainte2;
      ()

and freshen2_guarded_params : Var.Subst.t * Var.Subst.t -> guarded_params -> guarded_params -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_avaros1, _contrainte0), (_avaros3, _contrainte2) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_avaro (var_env1, var_env2) _avaros1 _avaros3 in
      (var_env1, var_env2)

and create_guarded_params : guarded_params -> opaque_guarded_params = 
  function body -> {
    guarded_params_delayed = (Datacon.Subst.id, Sorte.Subst.id, Var.Subst.id);
    guarded_params = body
  }

and open_guarded_params : opaque_guarded_params -> guarded_params = function abstraction ->
  let (datacon_delayed, sorte_delayed, var_delayed) = abstraction.guarded_params_delayed in
  let body = abstraction.guarded_params in
  let (var_bvars) = bound_guarded_params body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_guarded_params (datacon_delayed, sorte_delayed, var_env) body in
  if not (Datacon.Subst.is_id datacon_delayed && Sorte.Subst.is_id sorte_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.guarded_params_delayed <- (Datacon.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction.guarded_params <- body
  end;
  body

and open2_guarded_params : opaque_guarded_params -> opaque_guarded_params -> guarded_params * guarded_params = fun x1 x2 -> 
  change_invalid_to_open2 open2i_guarded_params x1 x2

and open2i_guarded_params : opaque_guarded_params -> opaque_guarded_params -> guarded_params * guarded_params = fun abstraction1 abstraction2 ->
  let (datacon_delayed1, sorte_delayed1, var_delayed1) = abstraction1.guarded_params_delayed in
  let body1 = abstraction1.guarded_params in
  let (datacon_delayed2, sorte_delayed2, var_delayed2) = abstraction2.guarded_params_delayed in
  let body2 = abstraction2.guarded_params in
  let (var_env1, var_env2) = freshen2_guarded_params (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_guarded_params (datacon_delayed1, sorte_delayed1, var_env1) body1 in
  let body2 = subst_guarded_params (datacon_delayed2, sorte_delayed2, var_env2) body2 in
  if not (Datacon.Subst.is_id datacon_delayed1 && Sorte.Subst.is_id sorte_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.guarded_params_delayed <- (Datacon.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction1.guarded_params <- body1
  end;
  if not (Datacon.Subst.is_id datacon_delayed2 && Sorte.Subst.is_id sorte_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.guarded_params_delayed <- (Datacon.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction2.guarded_params <- body2
  end;
  body1, body2

and apply_guarded_params = 
  fun (datacon_env, sorte_env, var_env) abstraction ->
    let (datacon_delayed, sorte_delayed, var_delayed) = abstraction.guarded_params_delayed in {
      abstraction with guarded_params_delayed = (Datacon.Subst.compose datacon_env datacon_delayed, Sorte.Subst.compose sorte_env sorte_delayed, Var.Subst.compose var_env var_delayed)
    }

and subst_datatypedef : Var.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Sorte.Subst.t -> datatypedef -> datatypedef = fun (var_oenv, datacon_ienv, datatype_ienv, sorte_ienv) -> function
  { datatype_name = _datatype2; datatype_sorts = _sorts1; datatype_constructors = _datacondefs0 } ->
    { datatype_name = Datatype.Subst.lookup _datatype2 datatype_ienv; datatype_sorts = (subst_sorts (sorte_ienv)) _sorts1; datatype_constructors = List.map (subst_datacondef (var_oenv, datacon_ienv, datatype_ienv, sorte_ienv)) _datacondefs0 }

and bound_datatypedef : datatypedef -> Datacon.AtomSet.t * Datatype.AtomSet.t = 
  function datatypedef -> bound_accu_datatypedef (Datacon.AtomSet.empty, Datatype.AtomSet.empty) datatypedef

and bound_free_datatypedef : datatypedef -> Datacon.AtomSet.t * Datatype.AtomSet.t * Datacon.AtomSet.t * Datatype.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function datatypedef -> bound_free_accu_datatypedef (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Datacon.AtomSet.empty, Datatype.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) datatypedef

and equal_datatypedef : datatypedef -> datatypedef -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_datatypedef x1 x2

and import_datatypedef = fun (var_oenv, datacon_ienv, datatype_ienv, sorte_ienv) -> function
  { Raw.datatype_name = _datatype2; Raw.datatype_sorts = _sorts1; Raw.datatype_constructors = _datacondefs0 } ->
    { datatype_name = Datatype.find _datatype2 datatype_ienv; datatype_sorts = (import_sorts (sorte_ienv)) _sorts1; datatype_constructors = List.map (import_datacondef (var_oenv, datacon_ienv, datatype_ienv, sorte_ienv)) _datacondefs0 }

and bvi_accu_datatypedef = fun (datacon_bvars, datatype_bvars) -> function
  { Raw.datatype_name = _datatype2; Raw.datatype_sorts = _sorts1; Raw.datatype_constructors = _datacondefs0 } ->
      let datatype_bvars = Identifier.Map.add _datatype2 () datatype_bvars in
      let (datacon_bvars) = List.fold_left bvi_accu_datacondef (datacon_bvars) _datacondefs0 in
      (datacon_bvars, datatype_bvars)

and bvi_datatypedef = 
  function datatypedef -> bvi_accu_datatypedef (Identifier.Map.empty, Identifier.Map.empty) datatypedef

and bound_accu_datatypedef = fun (datacon_bvars, datatype_bvars) -> function
  { datatype_name = _datatype2; datatype_sorts = _sorts1; datatype_constructors = _datacondefs0 } ->
      let datatype_bvars = Datatype.AtomSet.add _datatype2 datatype_bvars in
      let (datacon_bvars) = List.fold_left bound_accu_datacondef (datacon_bvars) _datacondefs0 in
      (datacon_bvars, datatype_bvars)

and export_datatypedef : Var.AtomIdMap.t * Datacon.AtomIdMap.t * Datatype.AtomIdMap.t * Sorte.AtomIdMap.t -> datatypedef -> Raw.datatypedef = fun (var_om, datacon_im, datatype_im, sorte_im) -> function
  { datatype_name = _datatype2; datatype_sorts = _sorts1; datatype_constructors = _datacondefs0 } ->
    { Raw.datatype_name = Datatype.AtomIdMap.lookup _datatype2 datatype_im; Raw.datatype_sorts = (export_sorts (sorte_im)) _sorts1; Raw.datatype_constructors = List.map (export_datacondef (var_om, datacon_im, datatype_im, sorte_im)) _datacondefs0 }

and flatten_datatypedef : datatypedef -> Flat.datatypedef = function
  { datatype_name = _datatype2; datatype_sorts = _sorts1; datatype_constructors = _datacondefs0 } ->
    { Flat.datatype_name = _datatype2; Flat.datatype_sorts = flatten_sorts _sorts1; Flat.datatype_constructors = List.map flatten_datacondef _datacondefs0 }

and unflatten_datatypedef : Flat.datatypedef -> datatypedef = function
  { Flat.datatype_name = _datatype2; Flat.datatype_sorts = _sorts1; Flat.datatype_constructors = _datacondefs0 } ->
    { datatype_name = _datatype2; datatype_sorts = unflatten_sorts _sorts1; datatype_constructors = List.map unflatten_datacondef _datacondefs0 }

and bound_free_accu_datatypedef = fun (datacon_bvars, datatype_bvars, datacon_ifvars, datatype_ifvars, sorte_ifvars, var_ofvars) -> function
  { datatype_name = _datatype2; datatype_sorts = _sorts1; datatype_constructors = _datacondefs0 } ->
      let datatype_bvars = Datatype.AtomSet.add _datatype2 datatype_bvars in
      let (sorte_ifvars) = free_accu_sorts (sorte_ifvars) _sorts1 in
      let (datacon_bvars, datacon_ifvars, datatype_ifvars, sorte_ifvars, var_ofvars) = List.fold_left bound_free_accu_datacondef (datacon_bvars, datacon_ifvars, datatype_ifvars, sorte_ifvars, var_ofvars) _datacondefs0 in
      (datacon_bvars, datatype_bvars, datacon_ifvars, datatype_ifvars, sorte_ifvars, var_ofvars)

and aeq_datatypedef : unit -> datatypedef -> datatypedef -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | { datatype_name = _datatype2; datatype_sorts = _sorts1; datatype_constructors = _datacondefs0 }, { datatype_name = _datatype5; datatype_sorts = _sorts4; datatype_constructors = _datacondefs3 } ->
      if not (Datatype.Atom.equal _datatype2 _datatype5) then raise (Invalid_argument "aeq_datatypedef");
      aeq_sorts () _sorts1 _sorts4;
      List.fold_left2 aeq_datacondef () _datacondefs0 _datacondefs3;
      ()

and freshen2_datatypedef : Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t -> datatypedef -> datatypedef -> Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t = fun (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | { datatype_name = _datatype2; datatype_sorts = _sorts1; datatype_constructors = _datacondefs0 }, { datatype_name = _datatype5; datatype_sorts = _sorts4; datatype_constructors = _datacondefs3 } ->
      let datatype_env1, datatype_env2 = Datatype.Subst.freshen2 _datatype2 datatype_env1 _datatype5 datatype_env2 in
      let (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2) = List.fold_left2 freshen2_datacondef (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2) _datacondefs0 _datacondefs3 in
      (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2)

and subst_datacondef : Var.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Sorte.Subst.t -> datacondef -> datacondef = fun (var_oenv, datacon_ienv, datatype_ienv, sorte_ienv) -> function
  (_datacon1, _dataconparams0) ->
    (Datacon.Subst.lookup _datacon1 datacon_ienv, (subst_dataconparams (datacon_ienv, datatype_ienv, sorte_ienv, var_oenv)) _dataconparams0)

and bound_datacondef : datacondef -> Datacon.AtomSet.t = 
  function datacondef -> bound_accu_datacondef (Datacon.AtomSet.empty) datacondef

and bound_free_datacondef : datacondef -> Datacon.AtomSet.t * Datacon.AtomSet.t * Datatype.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function datacondef -> bound_free_accu_datacondef (Datacon.AtomSet.empty, Datacon.AtomSet.empty, Datatype.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) datacondef

and equal_datacondef : datacondef -> datacondef -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_datacondef x1 x2

and import_datacondef = fun (var_oenv, datacon_ienv, datatype_ienv, sorte_ienv) -> function
  (_datacon1, _dataconparams0) ->
    (Datacon.find _datacon1 datacon_ienv, (import_dataconparams (datacon_ienv, datatype_ienv, sorte_ienv, var_oenv)) _dataconparams0)

and bvi_accu_datacondef = fun (datacon_bvars) -> function
  (_datacon1, _dataconparams0) ->
      let datacon_bvars = Identifier.Map.add _datacon1 () datacon_bvars in
      (datacon_bvars)

and bvi_datacondef = 
  function datacondef -> bvi_accu_datacondef (Identifier.Map.empty) datacondef

and bound_accu_datacondef = fun (datacon_bvars) -> function
  (_datacon1, _dataconparams0) ->
      let datacon_bvars = Datacon.AtomSet.add _datacon1 datacon_bvars in
      (datacon_bvars)

and export_datacondef : Var.AtomIdMap.t * Datacon.AtomIdMap.t * Datatype.AtomIdMap.t * Sorte.AtomIdMap.t -> datacondef -> Raw.datacondef = fun (var_om, datacon_im, datatype_im, sorte_im) -> function
  (_datacon1, _dataconparams0) ->
    (Datacon.AtomIdMap.lookup _datacon1 datacon_im, (export_dataconparams (datacon_im, datatype_im, sorte_im, var_om)) _dataconparams0)

and flatten_datacondef : datacondef -> Flat.datacondef = function
  (_datacon1, _dataconparams0) ->
    (_datacon1, flatten_dataconparams _dataconparams0)

and unflatten_datacondef : Flat.datacondef -> datacondef = function
  (_datacon1, _dataconparams0) ->
    (_datacon1, unflatten_dataconparams _dataconparams0)

and bound_free_accu_datacondef = fun (datacon_bvars, datacon_ifvars, datatype_ifvars, sorte_ifvars, var_ofvars) -> function
  (_datacon1, _dataconparams0) ->
      let datacon_bvars = Datacon.AtomSet.add _datacon1 datacon_bvars in
      let (datacon_ifvars, datatype_ifvars, sorte_ifvars, var_ofvars) = free_accu_dataconparams (datacon_ifvars, datatype_ifvars, sorte_ifvars, var_ofvars) _dataconparams0 in
      (datacon_bvars, datacon_ifvars, datatype_ifvars, sorte_ifvars, var_ofvars)

and aeq_datacondef : unit -> datacondef -> datacondef -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_datacon1, _dataconparams0), (_datacon3, _dataconparams2) ->
      if not (Datacon.Atom.equal _datacon1 _datacon3) then raise (Invalid_argument "aeq_datacondef");
      aeq_dataconparams () _dataconparams0 _dataconparams2;
      ()

and freshen2_datacondef : Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t -> datacondef -> datacondef -> Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t = fun (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_datacon1, _dataconparams0), (_datacon3, _dataconparams2) ->
      let datacon_env1, datacon_env2 = Datacon.Subst.freshen2 _datacon1 datacon_env1 _datacon3 datacon_env2 in
      (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2)

and import_dataconparams : datacon Identifier.Map.t * datatype Identifier.Map.t * sorte Identifier.Map.t * var Identifier.Map.t -> Raw.dataconparams -> dataconparams = fun (datacon_env, datatype_env, sorte_env, var_env) -> function
  (_guarded_tuple0) ->
      let (var_bvars) = bvi_guarded_tuple _guarded_tuple0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _guarded_tuple0 = import_guarded_tuple (datacon_env, datatype_env, sorte_env, var_ienv) _guarded_tuple0 in
    (create_guarded_tuple _guarded_tuple0)

and subst_dataconparams : Datacon.Subst.t * Datatype.Subst.t * Sorte.Subst.t * Var.Subst.t -> dataconparams -> dataconparams = fun (datacon_env, datatype_env, sorte_env, var_env) -> function
  (_guarded_tuple0) ->
    (apply_guarded_tuple (datacon_env, datatype_env, sorte_env, var_env) _guarded_tuple0)

and export_dataconparams : Datacon.AtomIdMap.t * Datatype.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> dataconparams -> Raw.dataconparams = fun (datacon_m, datatype_m, sorte_m, var_m) -> function
  (_guarded_tuple0) ->
      let guarded_tuple = open_guarded_tuple _guarded_tuple0 in
      let (var_bvars) = bound_guarded_tuple guarded_tuple in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
    (export_guarded_tuple (datacon_m, datatype_m, sorte_m, var_im) guarded_tuple)

and flatten_dataconparams : dataconparams -> Flat.dataconparams = function
  (_guarded_tuple0) ->
      let guarded_tuple = open_guarded_tuple _guarded_tuple0 in
    (flatten_guarded_tuple guarded_tuple)

and unflatten_dataconparams : Flat.dataconparams -> dataconparams = function
  (_guarded_tuple0) ->
      let guarded_tuple = unflatten_guarded_tuple _guarded_tuple0 in
    (create_guarded_tuple guarded_tuple)

and free_dataconparams : dataconparams -> Datacon.AtomSet.t * Datatype.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function dataconparams -> free_accu_dataconparams (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) dataconparams

and equal_dataconparams : dataconparams -> dataconparams -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_dataconparams x1 x2

and aeq_dataconparams : unit -> dataconparams -> dataconparams -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_guarded_tuple0), (_guarded_tuple1) ->
      let _guarded_tuple0, _guarded_tuple1 = open2i_guarded_tuple _guarded_tuple0 _guarded_tuple1 in
      aeq_guarded_tuple () _guarded_tuple0 _guarded_tuple1;
      ()

and free_accu_dataconparams = fun (datacon_fvars, datatype_fvars, sorte_fvars, var_fvars) -> function
  (_guarded_tuple0) ->
      let guarded_tuple = open_guarded_tuple _guarded_tuple0 in
      let (var_bvars, var_ifvars, datacon_fvars, datatype_fvars, sorte_fvars) = bound_free_accu_guarded_tuple (Var.AtomSet.empty, Var.AtomSet.empty, datacon_fvars, datatype_fvars, sorte_fvars) guarded_tuple in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (datacon_fvars, datatype_fvars, sorte_fvars, var_fvars)

and import_lemmadef : datacon Identifier.Map.t * datatype Identifier.Map.t * sorte Identifier.Map.t * var Identifier.Map.t -> Raw.lemmadef -> lemmadef = fun (datacon_env, datatype_env, sorte_env, var_env) -> function
  (_lemma0) ->
      let (var_bvars) = bvi_lemma _lemma0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _lemma0 = import_lemma (datacon_env, datatype_env, sorte_env, var_ienv) _lemma0 in
    (create_lemma _lemma0)

and subst_lemmadef : Datacon.Subst.t * Datatype.Subst.t * Sorte.Subst.t * Var.Subst.t -> lemmadef -> lemmadef = fun (datacon_env, datatype_env, sorte_env, var_env) -> function
  (_lemma0) ->
    (apply_lemma (datacon_env, datatype_env, sorte_env, var_env) _lemma0)

and export_lemmadef : Datacon.AtomIdMap.t * Datatype.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> lemmadef -> Raw.lemmadef = fun (datacon_m, datatype_m, sorte_m, var_m) -> function
  (_lemma0) ->
      let lemma = open_lemma _lemma0 in
      let (var_bvars) = bound_lemma lemma in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
    (export_lemma (datacon_m, datatype_m, sorte_m, var_im) lemma)

and flatten_lemmadef : lemmadef -> Flat.lemmadef = function
  (_lemma0) ->
      let lemma = open_lemma _lemma0 in
    (flatten_lemma lemma)

and unflatten_lemmadef : Flat.lemmadef -> lemmadef = function
  (_lemma0) ->
      let lemma = unflatten_lemma _lemma0 in
    (create_lemma lemma)

and free_lemmadef : lemmadef -> Datacon.AtomSet.t * Datatype.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function lemmadef -> free_accu_lemmadef (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) lemmadef

and equal_lemmadef : lemmadef -> lemmadef -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_lemmadef x1 x2

and aeq_lemmadef : unit -> lemmadef -> lemmadef -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_lemma0), (_lemma1) ->
      let _lemma0, _lemma1 = open2i_lemma _lemma0 _lemma1 in
      aeq_lemma () _lemma0 _lemma1;
      ()

and free_accu_lemmadef = fun (datacon_fvars, datatype_fvars, sorte_fvars, var_fvars) -> function
  (_lemma0) ->
      let lemma = open_lemma _lemma0 in
      let (var_bvars, var_ifvars, datacon_fvars, datatype_fvars, sorte_fvars) = bound_free_accu_lemma (Var.AtomSet.empty, Var.AtomSet.empty, datacon_fvars, datatype_fvars, sorte_fvars) lemma in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (datacon_fvars, datatype_fvars, sorte_fvars, var_fvars)

and subst_lemma : Datacon.Subst.t * Datatype.Subst.t * Sorte.Subst.t * Var.Subst.t -> lemma -> lemma = fun (datacon_oenv, datatype_oenv, sorte_oenv, var_ienv) -> function
  (_var2, _adatatype1, _contrainte0) ->
    (Var.Subst.lookup _var2 var_ienv, (subst_adatatype (datatype_oenv)) _adatatype1, (subst_contrainte (datacon_oenv, sorte_oenv, var_ienv)) _contrainte0)

and bound_lemma : lemma -> Var.AtomSet.t = 
  function lemma -> bound_accu_lemma (Var.AtomSet.empty) lemma

and bound_free_lemma : lemma -> Var.AtomSet.t * Var.AtomSet.t * Datacon.AtomSet.t * Datatype.AtomSet.t * Sorte.AtomSet.t = 
  function lemma -> bound_free_accu_lemma (Var.AtomSet.empty, Var.AtomSet.empty, Datacon.AtomSet.empty, Datatype.AtomSet.empty, Sorte.AtomSet.empty) lemma

and equal_lemma : lemma -> lemma -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_lemma x1 x2

and import_lemma = fun (datacon_oenv, datatype_oenv, sorte_oenv, var_ienv) -> function
  (_var2, _adatatype1, _contrainte0) ->
    (Var.find _var2 var_ienv, (import_adatatype (datatype_oenv)) _adatatype1, (import_contrainte (datacon_oenv, sorte_oenv, var_ienv)) _contrainte0)

and bvi_accu_lemma = fun (var_bvars) -> function
  (_var2, _adatatype1, _contrainte0) ->
      let var_bvars = Identifier.Map.add _var2 () var_bvars in
      (var_bvars)

and bvi_lemma = 
  function lemma -> bvi_accu_lemma (Identifier.Map.empty) lemma

and bound_accu_lemma = fun (var_bvars) -> function
  (_var2, _adatatype1, _contrainte0) ->
      let var_bvars = Var.AtomSet.add _var2 var_bvars in
      (var_bvars)

and export_lemma : Datacon.AtomIdMap.t * Datatype.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> lemma -> Raw.lemma = fun (datacon_om, datatype_om, sorte_om, var_im) -> function
  (_var2, _adatatype1, _contrainte0) ->
    (Var.AtomIdMap.lookup _var2 var_im, (export_adatatype (datatype_om)) _adatatype1, (export_contrainte (datacon_om, sorte_om, var_im)) _contrainte0)

and flatten_lemma : lemma -> Flat.lemma = function
  (_var2, _adatatype1, _contrainte0) ->
    (_var2, flatten_adatatype _adatatype1, flatten_contrainte _contrainte0)

and unflatten_lemma : Flat.lemma -> lemma = function
  (_var2, _adatatype1, _contrainte0) ->
    (_var2, unflatten_adatatype _adatatype1, unflatten_contrainte _contrainte0)

and bound_free_accu_lemma = fun (var_bvars, var_ifvars, datacon_ofvars, datatype_ofvars, sorte_ofvars) -> function
  (_var2, _adatatype1, _contrainte0) ->
      let var_bvars = Var.AtomSet.add _var2 var_bvars in
      let (datatype_ofvars) = free_accu_adatatype (datatype_ofvars) _adatatype1 in
      let (datacon_ofvars, sorte_ofvars, var_ifvars) = free_accu_contrainte (datacon_ofvars, sorte_ofvars, var_ifvars) _contrainte0 in
      (var_bvars, var_ifvars, datacon_ofvars, datatype_ofvars, sorte_ofvars)

and aeq_lemma : unit -> lemma -> lemma -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var2, _adatatype1, _contrainte0), (_var5, _adatatype4, _contrainte3) ->
      if not (Var.Atom.equal _var2 _var5) then raise (Invalid_argument "aeq_lemma");
      aeq_adatatype () _adatatype1 _adatatype4;
      aeq_contrainte () _contrainte0 _contrainte3;
      ()

and freshen2_lemma : Var.Subst.t * Var.Subst.t -> lemma -> lemma -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var2, _adatatype1, _contrainte0), (_var5, _adatatype4, _contrainte3) ->
      let var_env1, var_env2 = Var.Subst.freshen2 _var2 var_env1 _var5 var_env2 in
      (var_env1, var_env2)

and create_lemma : lemma -> opaque_lemma = 
  function body -> {
    lemma_delayed = (Datacon.Subst.id, Datatype.Subst.id, Sorte.Subst.id, Var.Subst.id);
    lemma = body
  }

and open_lemma : opaque_lemma -> lemma = function abstraction ->
  let (datacon_delayed, datatype_delayed, sorte_delayed, var_delayed) = abstraction.lemma_delayed in
  let body = abstraction.lemma in
  let (var_bvars) = bound_lemma body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_lemma (datacon_delayed, datatype_delayed, sorte_delayed, var_env) body in
  if not (Datacon.Subst.is_id datacon_delayed && Datatype.Subst.is_id datatype_delayed && Sorte.Subst.is_id sorte_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.lemma_delayed <- (Datacon.Subst.id, Datatype.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction.lemma <- body
  end;
  body

and open2_lemma : opaque_lemma -> opaque_lemma -> lemma * lemma = fun x1 x2 -> 
  change_invalid_to_open2 open2i_lemma x1 x2

and open2i_lemma : opaque_lemma -> opaque_lemma -> lemma * lemma = fun abstraction1 abstraction2 ->
  let (datacon_delayed1, datatype_delayed1, sorte_delayed1, var_delayed1) = abstraction1.lemma_delayed in
  let body1 = abstraction1.lemma in
  let (datacon_delayed2, datatype_delayed2, sorte_delayed2, var_delayed2) = abstraction2.lemma_delayed in
  let body2 = abstraction2.lemma in
  let (var_env1, var_env2) = freshen2_lemma (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_lemma (datacon_delayed1, datatype_delayed1, sorte_delayed1, var_env1) body1 in
  let body2 = subst_lemma (datacon_delayed2, datatype_delayed2, sorte_delayed2, var_env2) body2 in
  if not (Datacon.Subst.is_id datacon_delayed1 && Datatype.Subst.is_id datatype_delayed1 && Sorte.Subst.is_id sorte_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.lemma_delayed <- (Datacon.Subst.id, Datatype.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction1.lemma <- body1
  end;
  if not (Datacon.Subst.is_id datacon_delayed2 && Datatype.Subst.is_id datatype_delayed2 && Sorte.Subst.is_id sorte_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.lemma_delayed <- (Datacon.Subst.id, Datatype.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction2.lemma <- body2
  end;
  body1, body2

and apply_lemma = 
  fun (datacon_env, datatype_env, sorte_env, var_env) abstraction ->
    let (datacon_delayed, datatype_delayed, sorte_delayed, var_delayed) = abstraction.lemma_delayed in {
      abstraction with lemma_delayed = (Datacon.Subst.compose datacon_env datacon_delayed, Datatype.Subst.compose datatype_env datatype_delayed, Sorte.Subst.compose sorte_env sorte_delayed, Var.Subst.compose var_env var_delayed)
    }

and subst_excdef : Var.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Sorte.Subst.t -> excdef -> excdef = fun (var_oenv, datacon_ienv, datatype_ienv, exccon_ienv, sorte_ienv) -> function
  (_exccon1, _dataconparams0) ->
    (Exccon.Subst.lookup _exccon1 exccon_ienv, (subst_dataconparams (datacon_ienv, datatype_ienv, sorte_ienv, var_oenv)) _dataconparams0)

and bound_excdef : excdef -> Exccon.AtomSet.t = 
  function excdef -> bound_accu_excdef (Exccon.AtomSet.empty) excdef

and bound_free_excdef : excdef -> Exccon.AtomSet.t * Datacon.AtomSet.t * Datatype.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function excdef -> bound_free_accu_excdef (Exccon.AtomSet.empty, Datacon.AtomSet.empty, Datatype.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) excdef

and equal_excdef : excdef -> excdef -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_excdef x1 x2

and import_excdef = fun (var_oenv, datacon_ienv, datatype_ienv, exccon_ienv, sorte_ienv) -> function
  (_exccon1, _dataconparams0) ->
    (Exccon.find _exccon1 exccon_ienv, (import_dataconparams (datacon_ienv, datatype_ienv, sorte_ienv, var_oenv)) _dataconparams0)

and bvi_accu_excdef = fun (exccon_bvars) -> function
  (_exccon1, _dataconparams0) ->
      let exccon_bvars = Identifier.Map.add _exccon1 () exccon_bvars in
      (exccon_bvars)

and bvi_excdef = 
  function excdef -> bvi_accu_excdef (Identifier.Map.empty) excdef

and bound_accu_excdef = fun (exccon_bvars) -> function
  (_exccon1, _dataconparams0) ->
      let exccon_bvars = Exccon.AtomSet.add _exccon1 exccon_bvars in
      (exccon_bvars)

and export_excdef : Var.AtomIdMap.t * Datacon.AtomIdMap.t * Datatype.AtomIdMap.t * Exccon.AtomIdMap.t * Sorte.AtomIdMap.t -> excdef -> Raw.excdef = fun (var_om, datacon_im, datatype_im, exccon_im, sorte_im) -> function
  (_exccon1, _dataconparams0) ->
    (Exccon.AtomIdMap.lookup _exccon1 exccon_im, (export_dataconparams (datacon_im, datatype_im, sorte_im, var_om)) _dataconparams0)

and flatten_excdef : excdef -> Flat.excdef = function
  (_exccon1, _dataconparams0) ->
    (_exccon1, flatten_dataconparams _dataconparams0)

and unflatten_excdef : Flat.excdef -> excdef = function
  (_exccon1, _dataconparams0) ->
    (_exccon1, unflatten_dataconparams _dataconparams0)

and bound_free_accu_excdef = fun (exccon_bvars, datacon_ifvars, datatype_ifvars, sorte_ifvars, var_ofvars) -> function
  (_exccon1, _dataconparams0) ->
      let exccon_bvars = Exccon.AtomSet.add _exccon1 exccon_bvars in
      let (datacon_ifvars, datatype_ifvars, sorte_ifvars, var_ofvars) = free_accu_dataconparams (datacon_ifvars, datatype_ifvars, sorte_ifvars, var_ofvars) _dataconparams0 in
      (exccon_bvars, datacon_ifvars, datatype_ifvars, sorte_ifvars, var_ofvars)

and aeq_excdef : unit -> excdef -> excdef -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_exccon1, _dataconparams0), (_exccon3, _dataconparams2) ->
      if not (Exccon.Atom.equal _exccon1 _exccon3) then raise (Invalid_argument "aeq_excdef");
      aeq_dataconparams () _dataconparams0 _dataconparams2;
      ()

and freshen2_excdef : Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t -> excdef -> excdef -> Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t = fun (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_exccon1, _dataconparams0), (_exccon3, _dataconparams2) ->
      let exccon_env1, exccon_env2 = Exccon.Subst.freshen2 _exccon1 exccon_env1 _exccon3 exccon_env2 in
      (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2)

and subst_raw_pattern : Datacon.Subst.t * Var.Subst.t -> raw_pattern -> raw_pattern = fun (datacon_oenv, var_ienv) -> function
  | PZero ->
      PZero
  | POne ->
      POne
  | PVar (_var0) ->
      PVar (Var.Subst.lookup _var0 var_ienv)
  | PBool (_x0) ->
      PBool (_x0)
  | PData (_adatacon1, _patterns0) ->
      PData ((subst_adatacon (datacon_oenv)) _adatacon1, List.map (subst_pattern (datacon_oenv, var_ienv)) _patterns0)
  | PConjunction (_patterns0) ->
      PConjunction (List.map (subst_pattern (datacon_oenv, var_ienv)) _patterns0)
  | PDisjunction (_patterns0) ->
      PDisjunction (List.map (subst_pattern (datacon_oenv, var_ienv)) _patterns0)
  | PTuple (_patterns0) ->
      PTuple (List.map (subst_pattern (datacon_oenv, var_ienv)) _patterns0)

and bound_raw_pattern : raw_pattern -> Var.AtomSet.t = 
  function raw_pattern -> bound_accu_raw_pattern (Var.AtomSet.empty) raw_pattern

and bound_free_raw_pattern : raw_pattern -> Var.AtomSet.t * Datacon.AtomSet.t = 
  function raw_pattern -> bound_free_accu_raw_pattern (Var.AtomSet.empty, Datacon.AtomSet.empty) raw_pattern

and equal_raw_pattern : raw_pattern -> raw_pattern -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_raw_pattern x1 x2

and import_raw_pattern = fun (datacon_oenv, var_ienv) -> function
  | Raw.PZero ->
      PZero
  | Raw.POne ->
      POne
  | Raw.PVar (_var0) ->
      PVar (Var.find _var0 var_ienv)
  | Raw.PBool (_x0) ->
      PBool (_x0)
  | Raw.PData (_adatacon1, _patterns0) ->
      PData ((import_adatacon (datacon_oenv)) _adatacon1, List.map (import_pattern (datacon_oenv, var_ienv)) _patterns0)
  | Raw.PConjunction (_patterns0) ->
      PConjunction (List.map (import_pattern (datacon_oenv, var_ienv)) _patterns0)
  | Raw.PDisjunction (_patterns0) ->
      PDisjunction (List.map (import_pattern (datacon_oenv, var_ienv)) _patterns0)
  | Raw.PTuple (_patterns0) ->
      PTuple (List.map (import_pattern (datacon_oenv, var_ienv)) _patterns0)

and bvi_accu_raw_pattern = fun (var_bvars) -> function
  | Raw.PZero ->
      (var_bvars)
  | Raw.POne ->
      (var_bvars)
  | Raw.PVar (_var0) ->
      let var_bvars = Identifier.Map.add _var0 () var_bvars in
      (var_bvars)
  | Raw.PBool (_x0) ->
      (var_bvars)
  | Raw.PData (_adatacon1, _patterns0) ->
      let (var_bvars) = List.fold_left bvi_accu_pattern (var_bvars) _patterns0 in
      (var_bvars)
  | Raw.PConjunction (_patterns0) ->
      let (var_bvars) = List.fold_left bvi_accu_pattern (var_bvars) _patterns0 in
      (var_bvars)
  | Raw.PDisjunction (_patterns0) ->
      let (var_bvars) = List.fold_left bvi_accu_pattern (var_bvars) _patterns0 in
      (var_bvars)
  | Raw.PTuple (_patterns0) ->
      let (var_bvars) = List.fold_left bvi_accu_pattern (var_bvars) _patterns0 in
      (var_bvars)

and bvi_raw_pattern = 
  function raw_pattern -> bvi_accu_raw_pattern (Identifier.Map.empty) raw_pattern

and bound_accu_raw_pattern = fun (var_bvars) -> function
  | PZero ->
      (var_bvars)
  | POne ->
      (var_bvars)
  | PVar (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars)
  | PBool (_x0) ->
      (var_bvars)
  | PData (_adatacon1, _patterns0) ->
      let (var_bvars) = List.fold_left bound_accu_pattern (var_bvars) _patterns0 in
      (var_bvars)
  | PConjunction (_patterns0) ->
      let (var_bvars) = List.fold_left bound_accu_pattern (var_bvars) _patterns0 in
      (var_bvars)
  | PDisjunction (_patterns0) ->
      let (var_bvars) = List.fold_left bound_accu_pattern (var_bvars) _patterns0 in
      (var_bvars)
  | PTuple (_patterns0) ->
      let (var_bvars) = List.fold_left bound_accu_pattern (var_bvars) _patterns0 in
      (var_bvars)

and export_raw_pattern : Datacon.AtomIdMap.t * Var.AtomIdMap.t -> raw_pattern -> Raw.raw_pattern = fun (datacon_om, var_im) -> function
  | PZero ->
      Raw.PZero
  | POne ->
      Raw.POne
  | PVar (_var0) ->
      Raw.PVar (Var.AtomIdMap.lookup _var0 var_im)
  | PBool (_x0) ->
      Raw.PBool (_x0)
  | PData (_adatacon1, _patterns0) ->
      Raw.PData ((export_adatacon (datacon_om)) _adatacon1, List.map (export_pattern (datacon_om, var_im)) _patterns0)
  | PConjunction (_patterns0) ->
      Raw.PConjunction (List.map (export_pattern (datacon_om, var_im)) _patterns0)
  | PDisjunction (_patterns0) ->
      Raw.PDisjunction (List.map (export_pattern (datacon_om, var_im)) _patterns0)
  | PTuple (_patterns0) ->
      Raw.PTuple (List.map (export_pattern (datacon_om, var_im)) _patterns0)

and flatten_raw_pattern : raw_pattern -> Flat.raw_pattern = function
  | PZero ->
      Flat.PZero
  | POne ->
      Flat.POne
  | PVar (_var0) ->
      Flat.PVar (_var0)
  | PBool (_x0) ->
      Flat.PBool (_x0)
  | PData (_adatacon1, _patterns0) ->
      Flat.PData (flatten_adatacon _adatacon1, List.map flatten_pattern _patterns0)
  | PConjunction (_patterns0) ->
      Flat.PConjunction (List.map flatten_pattern _patterns0)
  | PDisjunction (_patterns0) ->
      Flat.PDisjunction (List.map flatten_pattern _patterns0)
  | PTuple (_patterns0) ->
      Flat.PTuple (List.map flatten_pattern _patterns0)

and unflatten_raw_pattern : Flat.raw_pattern -> raw_pattern = function
  | Flat.PZero ->
      PZero
  | Flat.POne ->
      POne
  | Flat.PVar (_var0) ->
      PVar (_var0)
  | Flat.PBool (_x0) ->
      PBool (_x0)
  | Flat.PData (_adatacon1, _patterns0) ->
      PData (unflatten_adatacon _adatacon1, List.map unflatten_pattern _patterns0)
  | Flat.PConjunction (_patterns0) ->
      PConjunction (List.map unflatten_pattern _patterns0)
  | Flat.PDisjunction (_patterns0) ->
      PDisjunction (List.map unflatten_pattern _patterns0)
  | Flat.PTuple (_patterns0) ->
      PTuple (List.map unflatten_pattern _patterns0)

and bound_free_accu_raw_pattern = fun (var_bvars, datacon_ofvars) -> function
  | PZero ->
      (var_bvars, datacon_ofvars)
  | POne ->
      (var_bvars, datacon_ofvars)
  | PVar (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars, datacon_ofvars)
  | PBool (_x0) ->
      (var_bvars, datacon_ofvars)
  | PData (_adatacon1, _patterns0) ->
      let (datacon_ofvars) = free_accu_adatacon (datacon_ofvars) _adatacon1 in
      let (var_bvars, datacon_ofvars) = List.fold_left bound_free_accu_pattern (var_bvars, datacon_ofvars) _patterns0 in
      (var_bvars, datacon_ofvars)
  | PConjunction (_patterns0) ->
      let (var_bvars, datacon_ofvars) = List.fold_left bound_free_accu_pattern (var_bvars, datacon_ofvars) _patterns0 in
      (var_bvars, datacon_ofvars)
  | PDisjunction (_patterns0) ->
      let (var_bvars, datacon_ofvars) = List.fold_left bound_free_accu_pattern (var_bvars, datacon_ofvars) _patterns0 in
      (var_bvars, datacon_ofvars)
  | PTuple (_patterns0) ->
      let (var_bvars, datacon_ofvars) = List.fold_left bound_free_accu_pattern (var_bvars, datacon_ofvars) _patterns0 in
      (var_bvars, datacon_ofvars)

and aeq_raw_pattern : unit -> raw_pattern -> raw_pattern -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | PZero, PZero ->
      ()
  | POne, POne ->
      ()
  | PVar (_var0), PVar (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_raw_pattern");
      ()
  | PBool (_x0), PBool (_x1) ->
      ()
  | PData (_adatacon1, _patterns0), PData (_adatacon3, _patterns2) ->
      aeq_adatacon () _adatacon1 _adatacon3;
      List.fold_left2 aeq_pattern () _patterns0 _patterns2;
      ()
  | PConjunction (_patterns0), PConjunction (_patterns1) ->
      List.fold_left2 aeq_pattern () _patterns0 _patterns1;
      ()
  | PDisjunction (_patterns0), PDisjunction (_patterns1) ->
      List.fold_left2 aeq_pattern () _patterns0 _patterns1;
      ()
  | PTuple (_patterns0), PTuple (_patterns1) ->
      List.fold_left2 aeq_pattern () _patterns0 _patterns1;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_raw_pattern")

and freshen2_raw_pattern : Var.Subst.t * Var.Subst.t -> raw_pattern -> raw_pattern -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | PZero, PZero ->
      (var_env1, var_env2)
  | POne, POne ->
      (var_env1, var_env2)
  | PVar (_var0), PVar (_var1) ->
      let var_env1, var_env2 = Var.Subst.freshen2 _var0 var_env1 _var1 var_env2 in
      (var_env1, var_env2)
  | PBool (_x0), PBool (_x1) ->
      (var_env1, var_env2)
  | PData (_adatacon1, _patterns0), PData (_adatacon3, _patterns2) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_pattern (var_env1, var_env2) _patterns0 _patterns2 in
      (var_env1, var_env2)
  | PConjunction (_patterns0), PConjunction (_patterns1) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_pattern (var_env1, var_env2) _patterns0 _patterns1 in
      (var_env1, var_env2)
  | PDisjunction (_patterns0), PDisjunction (_patterns1) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_pattern (var_env1, var_env2) _patterns0 _patterns1 in
      (var_env1, var_env2)
  | PTuple (_patterns0), PTuple (_patterns1) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_pattern (var_env1, var_env2) _patterns0 _patterns1 in
      (var_env1, var_env2)
  | _, _ ->
      raise (Invalid_argument "freshen2_raw_pattern")

and subst_pattern : Datacon.Subst.t * Var.Subst.t -> pattern -> pattern = fun (datacon_oenv, var_ienv) -> function
  (_raw_patterns0) ->
    (Annotation.map (subst_raw_pattern (datacon_oenv, var_ienv)) _raw_patterns0)

and bound_pattern : pattern -> Var.AtomSet.t = 
  function pattern -> bound_accu_pattern (Var.AtomSet.empty) pattern

and bound_free_pattern : pattern -> Var.AtomSet.t * Datacon.AtomSet.t = 
  function pattern -> bound_free_accu_pattern (Var.AtomSet.empty, Datacon.AtomSet.empty) pattern

and equal_pattern : pattern -> pattern -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_pattern x1 x2

and import_pattern = fun (datacon_oenv, var_ienv) -> function
  (_raw_patterns0) ->
    (Annotation.map (import_raw_pattern (datacon_oenv, var_ienv)) _raw_patterns0)

and bvi_accu_pattern = fun (var_bvars) -> function
  (_raw_patterns0) ->
      let (var_bvars) = Annotation.fold bvi_accu_raw_pattern (var_bvars) _raw_patterns0 in
      (var_bvars)

and bvi_pattern = 
  function pattern -> bvi_accu_pattern (Identifier.Map.empty) pattern

and bound_accu_pattern = fun (var_bvars) -> function
  (_raw_patterns0) ->
      let (var_bvars) = Annotation.fold bound_accu_raw_pattern (var_bvars) _raw_patterns0 in
      (var_bvars)

and export_pattern : Datacon.AtomIdMap.t * Var.AtomIdMap.t -> pattern -> Raw.pattern = fun (datacon_om, var_im) -> function
  (_raw_patterns0) ->
    (Annotation.map (export_raw_pattern (datacon_om, var_im)) _raw_patterns0)

and flatten_pattern : pattern -> Flat.pattern = function
  (_raw_patterns0) ->
    (Annotation.map flatten_raw_pattern _raw_patterns0)

and unflatten_pattern : Flat.pattern -> pattern = function
  (_raw_patterns0) ->
    (Annotation.map unflatten_raw_pattern _raw_patterns0)

and bound_free_accu_pattern = fun (var_bvars, datacon_ofvars) -> function
  (_raw_patterns0) ->
      let (var_bvars, datacon_ofvars) = Annotation.fold bound_free_accu_raw_pattern (var_bvars, datacon_ofvars) _raw_patterns0 in
      (var_bvars, datacon_ofvars)

and aeq_pattern : unit -> pattern -> pattern -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_raw_patterns0), (_raw_patterns1) ->
      Annotation.fold2 aeq_raw_pattern () _raw_patterns0 _raw_patterns1;
      ()

and freshen2_pattern : Var.Subst.t * Var.Subst.t -> pattern -> pattern -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_raw_patterns0), (_raw_patterns1) ->
      let (var_env1, var_env2) = Annotation.fold2 freshen2_raw_pattern (var_env1, var_env2) _raw_patterns0 _raw_patterns1 in
      (var_env1, var_env2)

and subst_raw_exception_pattern : Exccon.Subst.t * Var.Subst.t -> raw_exception_pattern -> raw_exception_pattern = fun (exccon_oenv, var_ienv) -> function
  | EPData (_aexccon1, _avaros0) ->
      EPData ((subst_aexccon (exccon_oenv)) _aexccon1, List.map (subst_avaro (var_ienv)) _avaros0)
  | EPDisjunction (_exception_patterns0) ->
      EPDisjunction (List.map (subst_exception_pattern (exccon_oenv, var_ienv)) _exception_patterns0)

and bound_raw_exception_pattern : raw_exception_pattern -> Var.AtomSet.t = 
  function raw_exception_pattern -> bound_accu_raw_exception_pattern (Var.AtomSet.empty) raw_exception_pattern

and bound_free_raw_exception_pattern : raw_exception_pattern -> Var.AtomSet.t * Exccon.AtomSet.t = 
  function raw_exception_pattern -> bound_free_accu_raw_exception_pattern (Var.AtomSet.empty, Exccon.AtomSet.empty) raw_exception_pattern

and equal_raw_exception_pattern : raw_exception_pattern -> raw_exception_pattern -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_raw_exception_pattern x1 x2

and import_raw_exception_pattern = fun (exccon_oenv, var_ienv) -> function
  | Raw.EPData (_aexccon1, _avaros0) ->
      EPData ((import_aexccon (exccon_oenv)) _aexccon1, List.map (import_avaro (var_ienv)) _avaros0)
  | Raw.EPDisjunction (_exception_patterns0) ->
      EPDisjunction (List.map (import_exception_pattern (exccon_oenv, var_ienv)) _exception_patterns0)

and bvi_accu_raw_exception_pattern = fun (var_bvars) -> function
  | Raw.EPData (_aexccon1, _avaros0) ->
      let (var_bvars) = List.fold_left bvi_accu_avaro (var_bvars) _avaros0 in
      (var_bvars)
  | Raw.EPDisjunction (_exception_patterns0) ->
      let (var_bvars) = List.fold_left bvi_accu_exception_pattern (var_bvars) _exception_patterns0 in
      (var_bvars)

and bvi_raw_exception_pattern = 
  function raw_exception_pattern -> bvi_accu_raw_exception_pattern (Identifier.Map.empty) raw_exception_pattern

and bound_accu_raw_exception_pattern = fun (var_bvars) -> function
  | EPData (_aexccon1, _avaros0) ->
      let (var_bvars) = List.fold_left bound_accu_avaro (var_bvars) _avaros0 in
      (var_bvars)
  | EPDisjunction (_exception_patterns0) ->
      let (var_bvars) = List.fold_left bound_accu_exception_pattern (var_bvars) _exception_patterns0 in
      (var_bvars)

and export_raw_exception_pattern : Exccon.AtomIdMap.t * Var.AtomIdMap.t -> raw_exception_pattern -> Raw.raw_exception_pattern = fun (exccon_om, var_im) -> function
  | EPData (_aexccon1, _avaros0) ->
      Raw.EPData ((export_aexccon (exccon_om)) _aexccon1, List.map (export_avaro (var_im)) _avaros0)
  | EPDisjunction (_exception_patterns0) ->
      Raw.EPDisjunction (List.map (export_exception_pattern (exccon_om, var_im)) _exception_patterns0)

and flatten_raw_exception_pattern : raw_exception_pattern -> Flat.raw_exception_pattern = function
  | EPData (_aexccon1, _avaros0) ->
      Flat.EPData (flatten_aexccon _aexccon1, List.map flatten_avaro _avaros0)
  | EPDisjunction (_exception_patterns0) ->
      Flat.EPDisjunction (List.map flatten_exception_pattern _exception_patterns0)

and unflatten_raw_exception_pattern : Flat.raw_exception_pattern -> raw_exception_pattern = function
  | Flat.EPData (_aexccon1, _avaros0) ->
      EPData (unflatten_aexccon _aexccon1, List.map unflatten_avaro _avaros0)
  | Flat.EPDisjunction (_exception_patterns0) ->
      EPDisjunction (List.map unflatten_exception_pattern _exception_patterns0)

and bound_free_accu_raw_exception_pattern = fun (var_bvars, exccon_ofvars) -> function
  | EPData (_aexccon1, _avaros0) ->
      let (exccon_ofvars) = free_accu_aexccon (exccon_ofvars) _aexccon1 in
      let (var_bvars) = List.fold_left bound_free_accu_avaro (var_bvars) _avaros0 in
      (var_bvars, exccon_ofvars)
  | EPDisjunction (_exception_patterns0) ->
      let (var_bvars, exccon_ofvars) = List.fold_left bound_free_accu_exception_pattern (var_bvars, exccon_ofvars) _exception_patterns0 in
      (var_bvars, exccon_ofvars)

and aeq_raw_exception_pattern : unit -> raw_exception_pattern -> raw_exception_pattern -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | EPData (_aexccon1, _avaros0), EPData (_aexccon3, _avaros2) ->
      aeq_aexccon () _aexccon1 _aexccon3;
      List.fold_left2 aeq_avaro () _avaros0 _avaros2;
      ()
  | EPDisjunction (_exception_patterns0), EPDisjunction (_exception_patterns1) ->
      List.fold_left2 aeq_exception_pattern () _exception_patterns0 _exception_patterns1;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_raw_exception_pattern")

and freshen2_raw_exception_pattern : Var.Subst.t * Var.Subst.t -> raw_exception_pattern -> raw_exception_pattern -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | EPData (_aexccon1, _avaros0), EPData (_aexccon3, _avaros2) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_avaro (var_env1, var_env2) _avaros0 _avaros2 in
      (var_env1, var_env2)
  | EPDisjunction (_exception_patterns0), EPDisjunction (_exception_patterns1) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_exception_pattern (var_env1, var_env2) _exception_patterns0 _exception_patterns1 in
      (var_env1, var_env2)
  | _, _ ->
      raise (Invalid_argument "freshen2_raw_exception_pattern")

and subst_exception_pattern : Exccon.Subst.t * Var.Subst.t -> exception_pattern -> exception_pattern = fun (exccon_oenv, var_ienv) -> function
  (_raw_exception_patterns0) ->
    (Annotation.map (subst_raw_exception_pattern (exccon_oenv, var_ienv)) _raw_exception_patterns0)

and bound_exception_pattern : exception_pattern -> Var.AtomSet.t = 
  function exception_pattern -> bound_accu_exception_pattern (Var.AtomSet.empty) exception_pattern

and bound_free_exception_pattern : exception_pattern -> Var.AtomSet.t * Exccon.AtomSet.t = 
  function exception_pattern -> bound_free_accu_exception_pattern (Var.AtomSet.empty, Exccon.AtomSet.empty) exception_pattern

and equal_exception_pattern : exception_pattern -> exception_pattern -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_exception_pattern x1 x2

and import_exception_pattern = fun (exccon_oenv, var_ienv) -> function
  (_raw_exception_patterns0) ->
    (Annotation.map (import_raw_exception_pattern (exccon_oenv, var_ienv)) _raw_exception_patterns0)

and bvi_accu_exception_pattern = fun (var_bvars) -> function
  (_raw_exception_patterns0) ->
      let (var_bvars) = Annotation.fold bvi_accu_raw_exception_pattern (var_bvars) _raw_exception_patterns0 in
      (var_bvars)

and bvi_exception_pattern = 
  function exception_pattern -> bvi_accu_exception_pattern (Identifier.Map.empty) exception_pattern

and bound_accu_exception_pattern = fun (var_bvars) -> function
  (_raw_exception_patterns0) ->
      let (var_bvars) = Annotation.fold bound_accu_raw_exception_pattern (var_bvars) _raw_exception_patterns0 in
      (var_bvars)

and export_exception_pattern : Exccon.AtomIdMap.t * Var.AtomIdMap.t -> exception_pattern -> Raw.exception_pattern = fun (exccon_om, var_im) -> function
  (_raw_exception_patterns0) ->
    (Annotation.map (export_raw_exception_pattern (exccon_om, var_im)) _raw_exception_patterns0)

and flatten_exception_pattern : exception_pattern -> Flat.exception_pattern = function
  (_raw_exception_patterns0) ->
    (Annotation.map flatten_raw_exception_pattern _raw_exception_patterns0)

and unflatten_exception_pattern : Flat.exception_pattern -> exception_pattern = function
  (_raw_exception_patterns0) ->
    (Annotation.map unflatten_raw_exception_pattern _raw_exception_patterns0)

and bound_free_accu_exception_pattern = fun (var_bvars, exccon_ofvars) -> function
  (_raw_exception_patterns0) ->
      let (var_bvars, exccon_ofvars) = Annotation.fold bound_free_accu_raw_exception_pattern (var_bvars, exccon_ofvars) _raw_exception_patterns0 in
      (var_bvars, exccon_ofvars)

and aeq_exception_pattern : unit -> exception_pattern -> exception_pattern -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_raw_exception_patterns0), (_raw_exception_patterns1) ->
      Annotation.fold2 aeq_raw_exception_pattern () _raw_exception_patterns0 _raw_exception_patterns1;
      ()

and freshen2_exception_pattern : Var.Subst.t * Var.Subst.t -> exception_pattern -> exception_pattern -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_raw_exception_patterns0), (_raw_exception_patterns1) ->
      let (var_env1, var_env2) = Annotation.fold2 freshen2_raw_exception_pattern (var_env1, var_env2) _raw_exception_patterns0 _raw_exception_patterns1 in
      (var_env1, var_env2)

and import_time : unit -> Raw.time -> time = fun () -> function
  | Raw.CompileTime ->
      CompileTime
  | Raw.RunTime ->
      RunTime

and subst_time : unit -> time -> time = fun () -> function
  | CompileTime ->
      CompileTime
  | RunTime ->
      RunTime

and export_time : unit -> time -> Raw.time = fun () -> function
  | CompileTime ->
      Raw.CompileTime
  | RunTime ->
      Raw.RunTime

and flatten_time : time -> Flat.time = function
  | CompileTime ->
      Flat.CompileTime
  | RunTime ->
      Flat.RunTime

and unflatten_time : Flat.time -> time = function
  | Flat.CompileTime ->
      CompileTime
  | Flat.RunTime ->
      RunTime

and free_time : time -> unit = 
  function time -> free_accu_time () time

and equal_time : time -> time -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_time x1 x2

and aeq_time : unit -> time -> time -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | CompileTime, CompileTime ->
      ()
  | RunTime, RunTime ->
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_time")

and free_accu_time = fun () -> function
  | CompileTime ->
      ()
  | RunTime ->
      ()

and import_raw_expression : datacon Identifier.Map.t * exccon Identifier.Map.t * func Identifier.Map.t * sorte Identifier.Map.t * var Identifier.Map.t -> Raw.raw_expression -> raw_expression = fun (datacon_env, exccon_env, func_env, sorte_env, var_env) -> function
  | Raw.EVar (_var0) ->
      EVar (Var.find _var0 var_env)
  | Raw.EBool (_x0) ->
      EBool (_x0)
  | Raw.EData (_datacon1, _expressions0) ->
      EData (Datacon.find _datacon1 datacon_env, List.map (import_expression (datacon_env, exccon_env, func_env, sorte_env, var_env)) _expressions0)
  | Raw.EFresh (_fresh_binding0) ->
      let (var_bvars) = bvi_fresh_binding _fresh_binding0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _fresh_binding0 = import_fresh_binding (datacon_env, exccon_env, func_env, sorte_env, var_ienv) _fresh_binding0 in
      EFresh (create_fresh_binding _fresh_binding0)
  | Raw.ECase (_expression1, _branchs0) ->
      ECase ((import_expression (datacon_env, exccon_env, func_env, sorte_env, var_env)) _expression1, List.map (import_branch (datacon_env, exccon_env, func_env, sorte_env, var_env)) _branchs0)
  | Raw.ECall (_func1, _expression0) ->
      ECall (Func.find _func1 func_env, (import_expression (datacon_env, exccon_env, func_env, sorte_env, var_env)) _expression0)
  | Raw.ETuple (_expressions0) ->
      ETuple (List.map (import_expression (datacon_env, exccon_env, func_env, sorte_env, var_env)) _expressions0)
  | Raw.ERaise (_exccon1, _expressions0) ->
      ERaise (Exccon.find _exccon1 exccon_env, List.map (import_expression (datacon_env, exccon_env, func_env, sorte_env, var_env)) _expressions0)
  | Raw.ELetWhereUnless (_expression2, _handlers1, _letw_binding0) ->
      let (var_bvars) = bvi_letw_binding _letw_binding0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _letw_binding0 = import_letw_binding (datacon_env, exccon_env, func_env, sorte_env, var_ienv) _letw_binding0 in
      ELetWhereUnless ((import_expression (datacon_env, exccon_env, func_env, sorte_env, var_env)) _expression2, List.map (import_handler (datacon_env, exccon_env, func_env, sorte_env, var_env)) _handlers1, create_letw_binding _letw_binding0)
  | Raw.ETryUnless (_expression1, _handlers0) ->
      ETryUnless ((import_expression (datacon_env, exccon_env, func_env, sorte_env, var_env)) _expression1, List.map (import_handler (datacon_env, exccon_env, func_env, sorte_env, var_env)) _handlers0)
  | Raw.EAssert (_time2, _contrainte1, _expression0) ->
      EAssert ((import_time ()) _time2, (import_contrainte (datacon_env, sorte_env, var_env)) _contrainte1, (import_expression (datacon_env, exccon_env, func_env, sorte_env, var_env)) _expression0)
  | Raw.EAssertFalse (_time0) ->
      EAssertFalse ((import_time ()) _time0)

and subst_raw_expression : Datacon.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Var.Subst.t -> raw_expression -> raw_expression = fun (datacon_env, exccon_env, func_env, sorte_env, var_env) -> function
  | EVar (_var0) ->
      EVar (Var.Subst.lookup _var0 var_env)
  | EBool (_x0) ->
      EBool (_x0)
  | EData (_datacon1, _expressions0) ->
      EData (Datacon.Subst.lookup _datacon1 datacon_env, List.map (subst_expression (datacon_env, exccon_env, func_env, sorte_env, var_env)) _expressions0)
  | EFresh (_fresh_binding0) ->
      EFresh (apply_fresh_binding (datacon_env, exccon_env, func_env, sorte_env, var_env) _fresh_binding0)
  | ECase (_expression1, _branchs0) ->
      ECase ((subst_expression (datacon_env, exccon_env, func_env, sorte_env, var_env)) _expression1, List.map (subst_branch (datacon_env, exccon_env, func_env, sorte_env, var_env)) _branchs0)
  | ECall (_func1, _expression0) ->
      ECall (Func.Subst.lookup _func1 func_env, (subst_expression (datacon_env, exccon_env, func_env, sorte_env, var_env)) _expression0)
  | ETuple (_expressions0) ->
      ETuple (List.map (subst_expression (datacon_env, exccon_env, func_env, sorte_env, var_env)) _expressions0)
  | ERaise (_exccon1, _expressions0) ->
      ERaise (Exccon.Subst.lookup _exccon1 exccon_env, List.map (subst_expression (datacon_env, exccon_env, func_env, sorte_env, var_env)) _expressions0)
  | ELetWhereUnless (_expression2, _handlers1, _letw_binding0) ->
      ELetWhereUnless ((subst_expression (datacon_env, exccon_env, func_env, sorte_env, var_env)) _expression2, List.map (subst_handler (datacon_env, exccon_env, func_env, sorte_env, var_env)) _handlers1, apply_letw_binding (datacon_env, exccon_env, func_env, sorte_env, var_env) _letw_binding0)
  | ETryUnless (_expression1, _handlers0) ->
      ETryUnless ((subst_expression (datacon_env, exccon_env, func_env, sorte_env, var_env)) _expression1, List.map (subst_handler (datacon_env, exccon_env, func_env, sorte_env, var_env)) _handlers0)
  | EAssert (_time2, _contrainte1, _expression0) ->
      EAssert ((subst_time ()) _time2, (subst_contrainte (datacon_env, sorte_env, var_env)) _contrainte1, (subst_expression (datacon_env, exccon_env, func_env, sorte_env, var_env)) _expression0)
  | EAssertFalse (_time0) ->
      EAssertFalse ((subst_time ()) _time0)

and export_raw_expression : Datacon.AtomIdMap.t * Exccon.AtomIdMap.t * Func.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> raw_expression -> Raw.raw_expression = fun (datacon_m, exccon_m, func_m, sorte_m, var_m) -> function
  | EVar (_var0) ->
      Raw.EVar (Var.AtomIdMap.lookup _var0 var_m)
  | EBool (_x0) ->
      Raw.EBool (_x0)
  | EData (_datacon1, _expressions0) ->
      Raw.EData (Datacon.AtomIdMap.lookup _datacon1 datacon_m, List.map (export_expression (datacon_m, exccon_m, func_m, sorte_m, var_m)) _expressions0)
  | EFresh (_fresh_binding0) ->
      let fresh_binding = open_fresh_binding _fresh_binding0 in
      let (var_bvars) = bound_fresh_binding fresh_binding in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.EFresh (export_fresh_binding (datacon_m, exccon_m, func_m, sorte_m, var_im) fresh_binding)
  | ECase (_expression1, _branchs0) ->
      Raw.ECase ((export_expression (datacon_m, exccon_m, func_m, sorte_m, var_m)) _expression1, List.map (export_branch (datacon_m, exccon_m, func_m, sorte_m, var_m)) _branchs0)
  | ECall (_func1, _expression0) ->
      Raw.ECall (Func.AtomIdMap.lookup _func1 func_m, (export_expression (datacon_m, exccon_m, func_m, sorte_m, var_m)) _expression0)
  | ETuple (_expressions0) ->
      Raw.ETuple (List.map (export_expression (datacon_m, exccon_m, func_m, sorte_m, var_m)) _expressions0)
  | ERaise (_exccon1, _expressions0) ->
      Raw.ERaise (Exccon.AtomIdMap.lookup _exccon1 exccon_m, List.map (export_expression (datacon_m, exccon_m, func_m, sorte_m, var_m)) _expressions0)
  | ELetWhereUnless (_expression2, _handlers1, _letw_binding0) ->
      let letw_binding = open_letw_binding _letw_binding0 in
      let (var_bvars) = bound_letw_binding letw_binding in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.ELetWhereUnless ((export_expression (datacon_m, exccon_m, func_m, sorte_m, var_m)) _expression2, List.map (export_handler (datacon_m, exccon_m, func_m, sorte_m, var_m)) _handlers1, export_letw_binding (datacon_m, exccon_m, func_m, sorte_m, var_im) letw_binding)
  | ETryUnless (_expression1, _handlers0) ->
      Raw.ETryUnless ((export_expression (datacon_m, exccon_m, func_m, sorte_m, var_m)) _expression1, List.map (export_handler (datacon_m, exccon_m, func_m, sorte_m, var_m)) _handlers0)
  | EAssert (_time2, _contrainte1, _expression0) ->
      Raw.EAssert ((export_time ()) _time2, (export_contrainte (datacon_m, sorte_m, var_m)) _contrainte1, (export_expression (datacon_m, exccon_m, func_m, sorte_m, var_m)) _expression0)
  | EAssertFalse (_time0) ->
      Raw.EAssertFalse ((export_time ()) _time0)

and flatten_raw_expression : raw_expression -> Flat.raw_expression = function
  | EVar (_var0) ->
      Flat.EVar (_var0)
  | EBool (_x0) ->
      Flat.EBool (_x0)
  | EData (_datacon1, _expressions0) ->
      Flat.EData (_datacon1, List.map flatten_expression _expressions0)
  | EFresh (_fresh_binding0) ->
      let fresh_binding = open_fresh_binding _fresh_binding0 in
      Flat.EFresh (flatten_fresh_binding fresh_binding)
  | ECase (_expression1, _branchs0) ->
      Flat.ECase (flatten_expression _expression1, List.map flatten_branch _branchs0)
  | ECall (_func1, _expression0) ->
      Flat.ECall (_func1, flatten_expression _expression0)
  | ETuple (_expressions0) ->
      Flat.ETuple (List.map flatten_expression _expressions0)
  | ERaise (_exccon1, _expressions0) ->
      Flat.ERaise (_exccon1, List.map flatten_expression _expressions0)
  | ELetWhereUnless (_expression2, _handlers1, _letw_binding0) ->
      let letw_binding = open_letw_binding _letw_binding0 in
      Flat.ELetWhereUnless (flatten_expression _expression2, List.map flatten_handler _handlers1, flatten_letw_binding letw_binding)
  | ETryUnless (_expression1, _handlers0) ->
      Flat.ETryUnless (flatten_expression _expression1, List.map flatten_handler _handlers0)
  | EAssert (_time2, _contrainte1, _expression0) ->
      Flat.EAssert (flatten_time _time2, flatten_contrainte _contrainte1, flatten_expression _expression0)
  | EAssertFalse (_time0) ->
      Flat.EAssertFalse (flatten_time _time0)

and unflatten_raw_expression : Flat.raw_expression -> raw_expression = function
  | Flat.EVar (_var0) ->
      EVar (_var0)
  | Flat.EBool (_x0) ->
      EBool (_x0)
  | Flat.EData (_datacon1, _expressions0) ->
      EData (_datacon1, List.map unflatten_expression _expressions0)
  | Flat.EFresh (_fresh_binding0) ->
      let fresh_binding = unflatten_fresh_binding _fresh_binding0 in
      EFresh (create_fresh_binding fresh_binding)
  | Flat.ECase (_expression1, _branchs0) ->
      ECase (unflatten_expression _expression1, List.map unflatten_branch _branchs0)
  | Flat.ECall (_func1, _expression0) ->
      ECall (_func1, unflatten_expression _expression0)
  | Flat.ETuple (_expressions0) ->
      ETuple (List.map unflatten_expression _expressions0)
  | Flat.ERaise (_exccon1, _expressions0) ->
      ERaise (_exccon1, List.map unflatten_expression _expressions0)
  | Flat.ELetWhereUnless (_expression2, _handlers1, _letw_binding0) ->
      let letw_binding = unflatten_letw_binding _letw_binding0 in
      ELetWhereUnless (unflatten_expression _expression2, List.map unflatten_handler _handlers1, create_letw_binding letw_binding)
  | Flat.ETryUnless (_expression1, _handlers0) ->
      ETryUnless (unflatten_expression _expression1, List.map unflatten_handler _handlers0)
  | Flat.EAssert (_time2, _contrainte1, _expression0) ->
      EAssert (unflatten_time _time2, unflatten_contrainte _contrainte1, unflatten_expression _expression0)
  | Flat.EAssertFalse (_time0) ->
      EAssertFalse (unflatten_time _time0)

and free_raw_expression : raw_expression -> Datacon.AtomSet.t * Exccon.AtomSet.t * Func.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function raw_expression -> free_accu_raw_expression (Datacon.AtomSet.empty, Exccon.AtomSet.empty, Func.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) raw_expression

and equal_raw_expression : raw_expression -> raw_expression -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_raw_expression x1 x2

and aeq_raw_expression : unit -> raw_expression -> raw_expression -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | EVar (_var0), EVar (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_raw_expression");
      ()
  | EBool (_x0), EBool (_x1) ->
      ()
  | EData (_datacon1, _expressions0), EData (_datacon3, _expressions2) ->
      if not (Datacon.Atom.equal _datacon1 _datacon3) then raise (Invalid_argument "aeq_raw_expression");
      List.fold_left2 aeq_expression () _expressions0 _expressions2;
      ()
  | EFresh (_fresh_binding0), EFresh (_fresh_binding1) ->
      let _fresh_binding0, _fresh_binding1 = open2i_fresh_binding _fresh_binding0 _fresh_binding1 in
      aeq_fresh_binding () _fresh_binding0 _fresh_binding1;
      ()
  | ECase (_expression1, _branchs0), ECase (_expression3, _branchs2) ->
      aeq_expression () _expression1 _expression3;
      List.fold_left2 aeq_branch () _branchs0 _branchs2;
      ()
  | ECall (_func1, _expression0), ECall (_func3, _expression2) ->
      if not (Func.Atom.equal _func1 _func3) then raise (Invalid_argument "aeq_raw_expression");
      aeq_expression () _expression0 _expression2;
      ()
  | ETuple (_expressions0), ETuple (_expressions1) ->
      List.fold_left2 aeq_expression () _expressions0 _expressions1;
      ()
  | ERaise (_exccon1, _expressions0), ERaise (_exccon3, _expressions2) ->
      if not (Exccon.Atom.equal _exccon1 _exccon3) then raise (Invalid_argument "aeq_raw_expression");
      List.fold_left2 aeq_expression () _expressions0 _expressions2;
      ()
  | ELetWhereUnless (_expression2, _handlers1, _letw_binding0), ELetWhereUnless (_expression5, _handlers4, _letw_binding3) ->
      aeq_expression () _expression2 _expression5;
      List.fold_left2 aeq_handler () _handlers1 _handlers4;
      let _letw_binding0, _letw_binding3 = open2i_letw_binding _letw_binding0 _letw_binding3 in
      aeq_letw_binding () _letw_binding0 _letw_binding3;
      ()
  | ETryUnless (_expression1, _handlers0), ETryUnless (_expression3, _handlers2) ->
      aeq_expression () _expression1 _expression3;
      List.fold_left2 aeq_handler () _handlers0 _handlers2;
      ()
  | EAssert (_time2, _contrainte1, _expression0), EAssert (_time5, _contrainte4, _expression3) ->
      aeq_time () _time2 _time5;
      aeq_contrainte () _contrainte1 _contrainte4;
      aeq_expression () _expression0 _expression3;
      ()
  | EAssertFalse (_time0), EAssertFalse (_time1) ->
      aeq_time () _time0 _time1;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_raw_expression")

and free_accu_raw_expression = fun (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) -> function
  | EVar (_var0) ->
      let var_fvars = Var.AtomSet.add _var0 var_fvars in
      (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars)
  | EBool (_x0) ->
      (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars)
  | EData (_datacon1, _expressions0) ->
      let datacon_fvars = Datacon.AtomSet.add _datacon1 datacon_fvars in
      let (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) = List.fold_left free_accu_expression (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) _expressions0 in 
      (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars)
  | EFresh (_fresh_binding0) ->
      let fresh_binding = open_fresh_binding _fresh_binding0 in
      let (var_bvars, var_ifvars, datacon_fvars, exccon_fvars, func_fvars, sorte_fvars) = bound_free_accu_fresh_binding (Var.AtomSet.empty, Var.AtomSet.empty, datacon_fvars, exccon_fvars, func_fvars, sorte_fvars) fresh_binding in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars)
  | ECase (_expression1, _branchs0) ->
      let (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) = free_accu_expression (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) _expression1 in 
      let (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) = List.fold_left free_accu_branch (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) _branchs0 in 
      (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars)
  | ECall (_func1, _expression0) ->
      let func_fvars = Func.AtomSet.add _func1 func_fvars in
      let (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) = free_accu_expression (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) _expression0 in 
      (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars)
  | ETuple (_expressions0) ->
      let (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) = List.fold_left free_accu_expression (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) _expressions0 in 
      (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars)
  | ERaise (_exccon1, _expressions0) ->
      let exccon_fvars = Exccon.AtomSet.add _exccon1 exccon_fvars in
      let (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) = List.fold_left free_accu_expression (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) _expressions0 in 
      (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars)
  | ELetWhereUnless (_expression2, _handlers1, _letw_binding0) ->
      let (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) = free_accu_expression (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) _expression2 in 
      let (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) = List.fold_left free_accu_handler (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) _handlers1 in 
      let letw_binding = open_letw_binding _letw_binding0 in
      let (var_bvars, var_ifvars, datacon_fvars, exccon_fvars, func_fvars, sorte_fvars) = bound_free_accu_letw_binding (Var.AtomSet.empty, Var.AtomSet.empty, datacon_fvars, exccon_fvars, func_fvars, sorte_fvars) letw_binding in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars)
  | ETryUnless (_expression1, _handlers0) ->
      let (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) = free_accu_expression (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) _expression1 in 
      let (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) = List.fold_left free_accu_handler (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) _handlers0 in 
      (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars)
  | EAssert (_time2, _contrainte1, _expression0) ->
      let () = free_accu_time () _time2 in 
      let (datacon_fvars, sorte_fvars, var_fvars) = free_accu_contrainte (datacon_fvars, sorte_fvars, var_fvars) _contrainte1 in 
      let (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) = free_accu_expression (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) _expression0 in 
      (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars)
  | EAssertFalse (_time0) ->
      let () = free_accu_time () _time0 in 
      (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars)

and import_expression : datacon Identifier.Map.t * exccon Identifier.Map.t * func Identifier.Map.t * sorte Identifier.Map.t * var Identifier.Map.t -> Raw.expression -> expression = fun (datacon_env, exccon_env, func_env, sorte_env, var_env) -> function
  (_raw_expressions0) ->
    (Annotation.map (import_raw_expression (datacon_env, exccon_env, func_env, sorte_env, var_env)) _raw_expressions0)

and subst_expression : Datacon.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Var.Subst.t -> expression -> expression = fun (datacon_env, exccon_env, func_env, sorte_env, var_env) -> function
  (_raw_expressions0) ->
    (Annotation.map (subst_raw_expression (datacon_env, exccon_env, func_env, sorte_env, var_env)) _raw_expressions0)

and export_expression : Datacon.AtomIdMap.t * Exccon.AtomIdMap.t * Func.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> expression -> Raw.expression = fun (datacon_m, exccon_m, func_m, sorte_m, var_m) -> function
  (_raw_expressions0) ->
    (Annotation.map (export_raw_expression (datacon_m, exccon_m, func_m, sorte_m, var_m)) _raw_expressions0)

and flatten_expression : expression -> Flat.expression = function
  (_raw_expressions0) ->
    (Annotation.map flatten_raw_expression _raw_expressions0)

and unflatten_expression : Flat.expression -> expression = function
  (_raw_expressions0) ->
    (Annotation.map unflatten_raw_expression _raw_expressions0)

and free_expression : expression -> Datacon.AtomSet.t * Exccon.AtomSet.t * Func.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function expression -> free_accu_expression (Datacon.AtomSet.empty, Exccon.AtomSet.empty, Func.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) expression

and equal_expression : expression -> expression -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_expression x1 x2

and aeq_expression : unit -> expression -> expression -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_raw_expressions0), (_raw_expressions1) ->
      Annotation.fold2 aeq_raw_expression () _raw_expressions0 _raw_expressions1;
      ()

and free_accu_expression = fun (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) -> function
  (_raw_expressions0) ->
      let (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) = Annotation.fold free_accu_raw_expression (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) _raw_expressions0 in 
      (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars)

and subst_fresh_binding : Datacon.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Var.Subst.t -> fresh_binding -> fresh_binding = fun (datacon_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv) -> function
  (_asorte2, _avars1, _expression0) ->
    ((subst_asorte (sorte_oenv)) _asorte2, List.map (subst_avar (var_ienv)) _avars1, (subst_expression (datacon_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv)) _expression0)

and bound_fresh_binding : fresh_binding -> Var.AtomSet.t = 
  function fresh_binding -> bound_accu_fresh_binding (Var.AtomSet.empty) fresh_binding

and bound_free_fresh_binding : fresh_binding -> Var.AtomSet.t * Var.AtomSet.t * Datacon.AtomSet.t * Exccon.AtomSet.t * Func.AtomSet.t * Sorte.AtomSet.t = 
  function fresh_binding -> bound_free_accu_fresh_binding (Var.AtomSet.empty, Var.AtomSet.empty, Datacon.AtomSet.empty, Exccon.AtomSet.empty, Func.AtomSet.empty, Sorte.AtomSet.empty) fresh_binding

and equal_fresh_binding : fresh_binding -> fresh_binding -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_fresh_binding x1 x2

and import_fresh_binding = fun (datacon_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv) -> function
  (_asorte2, _avars1, _expression0) ->
    ((import_asorte (sorte_oenv)) _asorte2, List.map (import_avar (var_ienv)) _avars1, (import_expression (datacon_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv)) _expression0)

and bvi_accu_fresh_binding = fun (var_bvars) -> function
  (_asorte2, _avars1, _expression0) ->
      let (var_bvars) = List.fold_left bvi_accu_avar (var_bvars) _avars1 in
      (var_bvars)

and bvi_fresh_binding = 
  function fresh_binding -> bvi_accu_fresh_binding (Identifier.Map.empty) fresh_binding

and bound_accu_fresh_binding = fun (var_bvars) -> function
  (_asorte2, _avars1, _expression0) ->
      let (var_bvars) = List.fold_left bound_accu_avar (var_bvars) _avars1 in
      (var_bvars)

and export_fresh_binding : Datacon.AtomIdMap.t * Exccon.AtomIdMap.t * Func.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> fresh_binding -> Raw.fresh_binding = fun (datacon_om, exccon_om, func_om, sorte_om, var_im) -> function
  (_asorte2, _avars1, _expression0) ->
    ((export_asorte (sorte_om)) _asorte2, List.map (export_avar (var_im)) _avars1, (export_expression (datacon_om, exccon_om, func_om, sorte_om, var_im)) _expression0)

and flatten_fresh_binding : fresh_binding -> Flat.fresh_binding = function
  (_asorte2, _avars1, _expression0) ->
    (flatten_asorte _asorte2, List.map flatten_avar _avars1, flatten_expression _expression0)

and unflatten_fresh_binding : Flat.fresh_binding -> fresh_binding = function
  (_asorte2, _avars1, _expression0) ->
    (unflatten_asorte _asorte2, List.map unflatten_avar _avars1, unflatten_expression _expression0)

and bound_free_accu_fresh_binding = fun (var_bvars, var_ifvars, datacon_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars) -> function
  (_asorte2, _avars1, _expression0) ->
      let (sorte_ofvars) = free_accu_asorte (sorte_ofvars) _asorte2 in
      let (var_bvars) = List.fold_left bound_free_accu_avar (var_bvars) _avars1 in
      let (datacon_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars, var_ifvars) = free_accu_expression (datacon_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars, var_ifvars) _expression0 in
      (var_bvars, var_ifvars, datacon_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars)

and aeq_fresh_binding : unit -> fresh_binding -> fresh_binding -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_asorte2, _avars1, _expression0), (_asorte5, _avars4, _expression3) ->
      aeq_asorte () _asorte2 _asorte5;
      List.fold_left2 aeq_avar () _avars1 _avars4;
      aeq_expression () _expression0 _expression3;
      ()

and freshen2_fresh_binding : Var.Subst.t * Var.Subst.t -> fresh_binding -> fresh_binding -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_asorte2, _avars1, _expression0), (_asorte5, _avars4, _expression3) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_avar (var_env1, var_env2) _avars1 _avars4 in
      (var_env1, var_env2)

and create_fresh_binding : fresh_binding -> opaque_fresh_binding = 
  function body -> {
    fresh_binding_delayed = (Datacon.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    fresh_binding = body
  }

and open_fresh_binding : opaque_fresh_binding -> fresh_binding = function abstraction ->
  let (datacon_delayed, exccon_delayed, func_delayed, sorte_delayed, var_delayed) = abstraction.fresh_binding_delayed in
  let body = abstraction.fresh_binding in
  let (var_bvars) = bound_fresh_binding body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_fresh_binding (datacon_delayed, exccon_delayed, func_delayed, sorte_delayed, var_env) body in
  if not (Datacon.Subst.is_id datacon_delayed && Exccon.Subst.is_id exccon_delayed && Func.Subst.is_id func_delayed && Sorte.Subst.is_id sorte_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.fresh_binding_delayed <- (Datacon.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction.fresh_binding <- body
  end;
  body

and open2_fresh_binding : opaque_fresh_binding -> opaque_fresh_binding -> fresh_binding * fresh_binding = fun x1 x2 -> 
  change_invalid_to_open2 open2i_fresh_binding x1 x2

and open2i_fresh_binding : opaque_fresh_binding -> opaque_fresh_binding -> fresh_binding * fresh_binding = fun abstraction1 abstraction2 ->
  let (datacon_delayed1, exccon_delayed1, func_delayed1, sorte_delayed1, var_delayed1) = abstraction1.fresh_binding_delayed in
  let body1 = abstraction1.fresh_binding in
  let (datacon_delayed2, exccon_delayed2, func_delayed2, sorte_delayed2, var_delayed2) = abstraction2.fresh_binding_delayed in
  let body2 = abstraction2.fresh_binding in
  let (var_env1, var_env2) = freshen2_fresh_binding (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_fresh_binding (datacon_delayed1, exccon_delayed1, func_delayed1, sorte_delayed1, var_env1) body1 in
  let body2 = subst_fresh_binding (datacon_delayed2, exccon_delayed2, func_delayed2, sorte_delayed2, var_env2) body2 in
  if not (Datacon.Subst.is_id datacon_delayed1 && Exccon.Subst.is_id exccon_delayed1 && Func.Subst.is_id func_delayed1 && Sorte.Subst.is_id sorte_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.fresh_binding_delayed <- (Datacon.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction1.fresh_binding <- body1
  end;
  if not (Datacon.Subst.is_id datacon_delayed2 && Exccon.Subst.is_id exccon_delayed2 && Func.Subst.is_id func_delayed2 && Sorte.Subst.is_id sorte_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.fresh_binding_delayed <- (Datacon.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction2.fresh_binding <- body2
  end;
  body1, body2

and apply_fresh_binding = 
  fun (datacon_env, exccon_env, func_env, sorte_env, var_env) abstraction ->
    let (datacon_delayed, exccon_delayed, func_delayed, sorte_delayed, var_delayed) = abstraction.fresh_binding_delayed in {
      abstraction with fresh_binding_delayed = (Datacon.Subst.compose datacon_env datacon_delayed, Exccon.Subst.compose exccon_env exccon_delayed, Func.Subst.compose func_env func_delayed, Sorte.Subst.compose sorte_env sorte_delayed, Var.Subst.compose var_env var_delayed)
    }

and subst_letw_binding : Datacon.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Var.Subst.t -> letw_binding -> letw_binding = fun (datacon_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv) -> function
  (_avaros2, _contrainte1, _expression0) ->
    (List.map (subst_avaro (var_ienv)) _avaros2, (subst_contrainte (datacon_oenv, sorte_oenv, var_ienv)) _contrainte1, (subst_expression (datacon_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv)) _expression0)

and bound_letw_binding : letw_binding -> Var.AtomSet.t = 
  function letw_binding -> bound_accu_letw_binding (Var.AtomSet.empty) letw_binding

and bound_free_letw_binding : letw_binding -> Var.AtomSet.t * Var.AtomSet.t * Datacon.AtomSet.t * Exccon.AtomSet.t * Func.AtomSet.t * Sorte.AtomSet.t = 
  function letw_binding -> bound_free_accu_letw_binding (Var.AtomSet.empty, Var.AtomSet.empty, Datacon.AtomSet.empty, Exccon.AtomSet.empty, Func.AtomSet.empty, Sorte.AtomSet.empty) letw_binding

and equal_letw_binding : letw_binding -> letw_binding -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_letw_binding x1 x2

and import_letw_binding = fun (datacon_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv) -> function
  (_avaros2, _contrainte1, _expression0) ->
    (List.map (import_avaro (var_ienv)) _avaros2, (import_contrainte (datacon_oenv, sorte_oenv, var_ienv)) _contrainte1, (import_expression (datacon_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv)) _expression0)

and bvi_accu_letw_binding = fun (var_bvars) -> function
  (_avaros2, _contrainte1, _expression0) ->
      let (var_bvars) = List.fold_left bvi_accu_avaro (var_bvars) _avaros2 in
      (var_bvars)

and bvi_letw_binding = 
  function letw_binding -> bvi_accu_letw_binding (Identifier.Map.empty) letw_binding

and bound_accu_letw_binding = fun (var_bvars) -> function
  (_avaros2, _contrainte1, _expression0) ->
      let (var_bvars) = List.fold_left bound_accu_avaro (var_bvars) _avaros2 in
      (var_bvars)

and export_letw_binding : Datacon.AtomIdMap.t * Exccon.AtomIdMap.t * Func.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> letw_binding -> Raw.letw_binding = fun (datacon_om, exccon_om, func_om, sorte_om, var_im) -> function
  (_avaros2, _contrainte1, _expression0) ->
    (List.map (export_avaro (var_im)) _avaros2, (export_contrainte (datacon_om, sorte_om, var_im)) _contrainte1, (export_expression (datacon_om, exccon_om, func_om, sorte_om, var_im)) _expression0)

and flatten_letw_binding : letw_binding -> Flat.letw_binding = function
  (_avaros2, _contrainte1, _expression0) ->
    (List.map flatten_avaro _avaros2, flatten_contrainte _contrainte1, flatten_expression _expression0)

and unflatten_letw_binding : Flat.letw_binding -> letw_binding = function
  (_avaros2, _contrainte1, _expression0) ->
    (List.map unflatten_avaro _avaros2, unflatten_contrainte _contrainte1, unflatten_expression _expression0)

and bound_free_accu_letw_binding = fun (var_bvars, var_ifvars, datacon_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars) -> function
  (_avaros2, _contrainte1, _expression0) ->
      let (var_bvars) = List.fold_left bound_free_accu_avaro (var_bvars) _avaros2 in
      let (datacon_ofvars, sorte_ofvars, var_ifvars) = free_accu_contrainte (datacon_ofvars, sorte_ofvars, var_ifvars) _contrainte1 in
      let (datacon_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars, var_ifvars) = free_accu_expression (datacon_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars, var_ifvars) _expression0 in
      (var_bvars, var_ifvars, datacon_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars)

and aeq_letw_binding : unit -> letw_binding -> letw_binding -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_avaros2, _contrainte1, _expression0), (_avaros5, _contrainte4, _expression3) ->
      List.fold_left2 aeq_avaro () _avaros2 _avaros5;
      aeq_contrainte () _contrainte1 _contrainte4;
      aeq_expression () _expression0 _expression3;
      ()

and freshen2_letw_binding : Var.Subst.t * Var.Subst.t -> letw_binding -> letw_binding -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_avaros2, _contrainte1, _expression0), (_avaros5, _contrainte4, _expression3) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_avaro (var_env1, var_env2) _avaros2 _avaros5 in
      (var_env1, var_env2)

and create_letw_binding : letw_binding -> opaque_letw_binding = 
  function body -> {
    letw_binding_delayed = (Datacon.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    letw_binding = body
  }

and open_letw_binding : opaque_letw_binding -> letw_binding = function abstraction ->
  let (datacon_delayed, exccon_delayed, func_delayed, sorte_delayed, var_delayed) = abstraction.letw_binding_delayed in
  let body = abstraction.letw_binding in
  let (var_bvars) = bound_letw_binding body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_letw_binding (datacon_delayed, exccon_delayed, func_delayed, sorte_delayed, var_env) body in
  if not (Datacon.Subst.is_id datacon_delayed && Exccon.Subst.is_id exccon_delayed && Func.Subst.is_id func_delayed && Sorte.Subst.is_id sorte_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.letw_binding_delayed <- (Datacon.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction.letw_binding <- body
  end;
  body

and open2_letw_binding : opaque_letw_binding -> opaque_letw_binding -> letw_binding * letw_binding = fun x1 x2 -> 
  change_invalid_to_open2 open2i_letw_binding x1 x2

and open2i_letw_binding : opaque_letw_binding -> opaque_letw_binding -> letw_binding * letw_binding = fun abstraction1 abstraction2 ->
  let (datacon_delayed1, exccon_delayed1, func_delayed1, sorte_delayed1, var_delayed1) = abstraction1.letw_binding_delayed in
  let body1 = abstraction1.letw_binding in
  let (datacon_delayed2, exccon_delayed2, func_delayed2, sorte_delayed2, var_delayed2) = abstraction2.letw_binding_delayed in
  let body2 = abstraction2.letw_binding in
  let (var_env1, var_env2) = freshen2_letw_binding (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_letw_binding (datacon_delayed1, exccon_delayed1, func_delayed1, sorte_delayed1, var_env1) body1 in
  let body2 = subst_letw_binding (datacon_delayed2, exccon_delayed2, func_delayed2, sorte_delayed2, var_env2) body2 in
  if not (Datacon.Subst.is_id datacon_delayed1 && Exccon.Subst.is_id exccon_delayed1 && Func.Subst.is_id func_delayed1 && Sorte.Subst.is_id sorte_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.letw_binding_delayed <- (Datacon.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction1.letw_binding <- body1
  end;
  if not (Datacon.Subst.is_id datacon_delayed2 && Exccon.Subst.is_id exccon_delayed2 && Func.Subst.is_id func_delayed2 && Sorte.Subst.is_id sorte_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.letw_binding_delayed <- (Datacon.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction2.letw_binding <- body2
  end;
  body1, body2

and apply_letw_binding = 
  fun (datacon_env, exccon_env, func_env, sorte_env, var_env) abstraction ->
    let (datacon_delayed, exccon_delayed, func_delayed, sorte_delayed, var_delayed) = abstraction.letw_binding_delayed in {
      abstraction with letw_binding_delayed = (Datacon.Subst.compose datacon_env datacon_delayed, Exccon.Subst.compose exccon_env exccon_delayed, Func.Subst.compose func_env func_delayed, Sorte.Subst.compose sorte_env sorte_delayed, Var.Subst.compose var_env var_delayed)
    }

and subst_pat_binding : Datacon.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Var.Subst.t -> pat_binding -> pat_binding = fun (datacon_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv) -> function
  (_pattern1, _expression0) ->
    ((subst_pattern (datacon_oenv, var_ienv)) _pattern1, (subst_expression (datacon_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv)) _expression0)

and bound_pat_binding : pat_binding -> Var.AtomSet.t = 
  function pat_binding -> bound_accu_pat_binding (Var.AtomSet.empty) pat_binding

and bound_free_pat_binding : pat_binding -> Var.AtomSet.t * Var.AtomSet.t * Datacon.AtomSet.t * Exccon.AtomSet.t * Func.AtomSet.t * Sorte.AtomSet.t = 
  function pat_binding -> bound_free_accu_pat_binding (Var.AtomSet.empty, Var.AtomSet.empty, Datacon.AtomSet.empty, Exccon.AtomSet.empty, Func.AtomSet.empty, Sorte.AtomSet.empty) pat_binding

and equal_pat_binding : pat_binding -> pat_binding -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_pat_binding x1 x2

and import_pat_binding = fun (datacon_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv) -> function
  (_pattern1, _expression0) ->
    ((import_pattern (datacon_oenv, var_ienv)) _pattern1, (import_expression (datacon_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv)) _expression0)

and bvi_accu_pat_binding = fun (var_bvars) -> function
  (_pattern1, _expression0) ->
      let (var_bvars) = bvi_accu_pattern (var_bvars) _pattern1 in
      (var_bvars)

and bvi_pat_binding = 
  function pat_binding -> bvi_accu_pat_binding (Identifier.Map.empty) pat_binding

and bound_accu_pat_binding = fun (var_bvars) -> function
  (_pattern1, _expression0) ->
      let (var_bvars) = bound_accu_pattern (var_bvars) _pattern1 in
      (var_bvars)

and export_pat_binding : Datacon.AtomIdMap.t * Exccon.AtomIdMap.t * Func.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> pat_binding -> Raw.pat_binding = fun (datacon_om, exccon_om, func_om, sorte_om, var_im) -> function
  (_pattern1, _expression0) ->
    ((export_pattern (datacon_om, var_im)) _pattern1, (export_expression (datacon_om, exccon_om, func_om, sorte_om, var_im)) _expression0)

and flatten_pat_binding : pat_binding -> Flat.pat_binding = function
  (_pattern1, _expression0) ->
    (flatten_pattern _pattern1, flatten_expression _expression0)

and unflatten_pat_binding : Flat.pat_binding -> pat_binding = function
  (_pattern1, _expression0) ->
    (unflatten_pattern _pattern1, unflatten_expression _expression0)

and bound_free_accu_pat_binding = fun (var_bvars, var_ifvars, datacon_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars) -> function
  (_pattern1, _expression0) ->
      let (var_bvars, datacon_ofvars) = bound_free_accu_pattern (var_bvars, datacon_ofvars) _pattern1 in
      let (datacon_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars, var_ifvars) = free_accu_expression (datacon_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars, var_ifvars) _expression0 in
      (var_bvars, var_ifvars, datacon_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars)

and aeq_pat_binding : unit -> pat_binding -> pat_binding -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_pattern1, _expression0), (_pattern3, _expression2) ->
      aeq_pattern () _pattern1 _pattern3;
      aeq_expression () _expression0 _expression2;
      ()

and freshen2_pat_binding : Var.Subst.t * Var.Subst.t -> pat_binding -> pat_binding -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_pattern1, _expression0), (_pattern3, _expression2) ->
      let (var_env1, var_env2) = freshen2_pattern (var_env1, var_env2) _pattern1 _pattern3 in
      (var_env1, var_env2)

and create_pat_binding : pat_binding -> opaque_pat_binding = 
  function body -> {
    pat_binding_delayed = (Datacon.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    pat_binding = body
  }

and open_pat_binding : opaque_pat_binding -> pat_binding = function abstraction ->
  let (datacon_delayed, exccon_delayed, func_delayed, sorte_delayed, var_delayed) = abstraction.pat_binding_delayed in
  let body = abstraction.pat_binding in
  let (var_bvars) = bound_pat_binding body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_pat_binding (datacon_delayed, exccon_delayed, func_delayed, sorte_delayed, var_env) body in
  if not (Datacon.Subst.is_id datacon_delayed && Exccon.Subst.is_id exccon_delayed && Func.Subst.is_id func_delayed && Sorte.Subst.is_id sorte_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.pat_binding_delayed <- (Datacon.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction.pat_binding <- body
  end;
  body

and open2_pat_binding : opaque_pat_binding -> opaque_pat_binding -> pat_binding * pat_binding = fun x1 x2 -> 
  change_invalid_to_open2 open2i_pat_binding x1 x2

and open2i_pat_binding : opaque_pat_binding -> opaque_pat_binding -> pat_binding * pat_binding = fun abstraction1 abstraction2 ->
  let (datacon_delayed1, exccon_delayed1, func_delayed1, sorte_delayed1, var_delayed1) = abstraction1.pat_binding_delayed in
  let body1 = abstraction1.pat_binding in
  let (datacon_delayed2, exccon_delayed2, func_delayed2, sorte_delayed2, var_delayed2) = abstraction2.pat_binding_delayed in
  let body2 = abstraction2.pat_binding in
  let (var_env1, var_env2) = freshen2_pat_binding (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_pat_binding (datacon_delayed1, exccon_delayed1, func_delayed1, sorte_delayed1, var_env1) body1 in
  let body2 = subst_pat_binding (datacon_delayed2, exccon_delayed2, func_delayed2, sorte_delayed2, var_env2) body2 in
  if not (Datacon.Subst.is_id datacon_delayed1 && Exccon.Subst.is_id exccon_delayed1 && Func.Subst.is_id func_delayed1 && Sorte.Subst.is_id sorte_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.pat_binding_delayed <- (Datacon.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction1.pat_binding <- body1
  end;
  if not (Datacon.Subst.is_id datacon_delayed2 && Exccon.Subst.is_id exccon_delayed2 && Func.Subst.is_id func_delayed2 && Sorte.Subst.is_id sorte_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.pat_binding_delayed <- (Datacon.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction2.pat_binding <- body2
  end;
  body1, body2

and apply_pat_binding = 
  fun (datacon_env, exccon_env, func_env, sorte_env, var_env) abstraction ->
    let (datacon_delayed, exccon_delayed, func_delayed, sorte_delayed, var_delayed) = abstraction.pat_binding_delayed in {
      abstraction with pat_binding_delayed = (Datacon.Subst.compose datacon_env datacon_delayed, Exccon.Subst.compose exccon_env exccon_delayed, Func.Subst.compose func_env func_delayed, Sorte.Subst.compose sorte_env sorte_delayed, Var.Subst.compose var_env var_delayed)
    }

and import_branch : datacon Identifier.Map.t * exccon Identifier.Map.t * func Identifier.Map.t * sorte Identifier.Map.t * var Identifier.Map.t -> Raw.branch -> branch = fun (datacon_env, exccon_env, func_env, sorte_env, var_env) -> function
  (_pat_binding0) ->
      let (var_bvars) = bvi_pat_binding _pat_binding0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _pat_binding0 = import_pat_binding (datacon_env, exccon_env, func_env, sorte_env, var_ienv) _pat_binding0 in
    (create_pat_binding _pat_binding0)

and subst_branch : Datacon.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Var.Subst.t -> branch -> branch = fun (datacon_env, exccon_env, func_env, sorte_env, var_env) -> function
  (_pat_binding0) ->
    (apply_pat_binding (datacon_env, exccon_env, func_env, sorte_env, var_env) _pat_binding0)

and export_branch : Datacon.AtomIdMap.t * Exccon.AtomIdMap.t * Func.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> branch -> Raw.branch = fun (datacon_m, exccon_m, func_m, sorte_m, var_m) -> function
  (_pat_binding0) ->
      let pat_binding = open_pat_binding _pat_binding0 in
      let (var_bvars) = bound_pat_binding pat_binding in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
    (export_pat_binding (datacon_m, exccon_m, func_m, sorte_m, var_im) pat_binding)

and flatten_branch : branch -> Flat.branch = function
  (_pat_binding0) ->
      let pat_binding = open_pat_binding _pat_binding0 in
    (flatten_pat_binding pat_binding)

and unflatten_branch : Flat.branch -> branch = function
  (_pat_binding0) ->
      let pat_binding = unflatten_pat_binding _pat_binding0 in
    (create_pat_binding pat_binding)

and free_branch : branch -> Datacon.AtomSet.t * Exccon.AtomSet.t * Func.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function branch -> free_accu_branch (Datacon.AtomSet.empty, Exccon.AtomSet.empty, Func.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) branch

and equal_branch : branch -> branch -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_branch x1 x2

and aeq_branch : unit -> branch -> branch -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_pat_binding0), (_pat_binding1) ->
      let _pat_binding0, _pat_binding1 = open2i_pat_binding _pat_binding0 _pat_binding1 in
      aeq_pat_binding () _pat_binding0 _pat_binding1;
      ()

and free_accu_branch = fun (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) -> function
  (_pat_binding0) ->
      let pat_binding = open_pat_binding _pat_binding0 in
      let (var_bvars, var_ifvars, datacon_fvars, exccon_fvars, func_fvars, sorte_fvars) = bound_free_accu_pat_binding (Var.AtomSet.empty, Var.AtomSet.empty, datacon_fvars, exccon_fvars, func_fvars, sorte_fvars) pat_binding in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars)

and subst_exc_pat_binding : Datacon.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Var.Subst.t -> exc_pat_binding -> exc_pat_binding = fun (datacon_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv) -> function
  (_exception_pattern1, _expression0) ->
    ((subst_exception_pattern (exccon_oenv, var_ienv)) _exception_pattern1, (subst_expression (datacon_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv)) _expression0)

and bound_exc_pat_binding : exc_pat_binding -> Var.AtomSet.t = 
  function exc_pat_binding -> bound_accu_exc_pat_binding (Var.AtomSet.empty) exc_pat_binding

and bound_free_exc_pat_binding : exc_pat_binding -> Var.AtomSet.t * Var.AtomSet.t * Datacon.AtomSet.t * Exccon.AtomSet.t * Func.AtomSet.t * Sorte.AtomSet.t = 
  function exc_pat_binding -> bound_free_accu_exc_pat_binding (Var.AtomSet.empty, Var.AtomSet.empty, Datacon.AtomSet.empty, Exccon.AtomSet.empty, Func.AtomSet.empty, Sorte.AtomSet.empty) exc_pat_binding

and equal_exc_pat_binding : exc_pat_binding -> exc_pat_binding -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_exc_pat_binding x1 x2

and import_exc_pat_binding = fun (datacon_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv) -> function
  (_exception_pattern1, _expression0) ->
    ((import_exception_pattern (exccon_oenv, var_ienv)) _exception_pattern1, (import_expression (datacon_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv)) _expression0)

and bvi_accu_exc_pat_binding = fun (var_bvars) -> function
  (_exception_pattern1, _expression0) ->
      let (var_bvars) = bvi_accu_exception_pattern (var_bvars) _exception_pattern1 in
      (var_bvars)

and bvi_exc_pat_binding = 
  function exc_pat_binding -> bvi_accu_exc_pat_binding (Identifier.Map.empty) exc_pat_binding

and bound_accu_exc_pat_binding = fun (var_bvars) -> function
  (_exception_pattern1, _expression0) ->
      let (var_bvars) = bound_accu_exception_pattern (var_bvars) _exception_pattern1 in
      (var_bvars)

and export_exc_pat_binding : Datacon.AtomIdMap.t * Exccon.AtomIdMap.t * Func.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> exc_pat_binding -> Raw.exc_pat_binding = fun (datacon_om, exccon_om, func_om, sorte_om, var_im) -> function
  (_exception_pattern1, _expression0) ->
    ((export_exception_pattern (exccon_om, var_im)) _exception_pattern1, (export_expression (datacon_om, exccon_om, func_om, sorte_om, var_im)) _expression0)

and flatten_exc_pat_binding : exc_pat_binding -> Flat.exc_pat_binding = function
  (_exception_pattern1, _expression0) ->
    (flatten_exception_pattern _exception_pattern1, flatten_expression _expression0)

and unflatten_exc_pat_binding : Flat.exc_pat_binding -> exc_pat_binding = function
  (_exception_pattern1, _expression0) ->
    (unflatten_exception_pattern _exception_pattern1, unflatten_expression _expression0)

and bound_free_accu_exc_pat_binding = fun (var_bvars, var_ifvars, datacon_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars) -> function
  (_exception_pattern1, _expression0) ->
      let (var_bvars, exccon_ofvars) = bound_free_accu_exception_pattern (var_bvars, exccon_ofvars) _exception_pattern1 in
      let (datacon_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars, var_ifvars) = free_accu_expression (datacon_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars, var_ifvars) _expression0 in
      (var_bvars, var_ifvars, datacon_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars)

and aeq_exc_pat_binding : unit -> exc_pat_binding -> exc_pat_binding -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_exception_pattern1, _expression0), (_exception_pattern3, _expression2) ->
      aeq_exception_pattern () _exception_pattern1 _exception_pattern3;
      aeq_expression () _expression0 _expression2;
      ()

and freshen2_exc_pat_binding : Var.Subst.t * Var.Subst.t -> exc_pat_binding -> exc_pat_binding -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_exception_pattern1, _expression0), (_exception_pattern3, _expression2) ->
      let (var_env1, var_env2) = freshen2_exception_pattern (var_env1, var_env2) _exception_pattern1 _exception_pattern3 in
      (var_env1, var_env2)

and create_exc_pat_binding : exc_pat_binding -> opaque_exc_pat_binding = 
  function body -> {
    exc_pat_binding_delayed = (Datacon.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    exc_pat_binding = body
  }

and open_exc_pat_binding : opaque_exc_pat_binding -> exc_pat_binding = function abstraction ->
  let (datacon_delayed, exccon_delayed, func_delayed, sorte_delayed, var_delayed) = abstraction.exc_pat_binding_delayed in
  let body = abstraction.exc_pat_binding in
  let (var_bvars) = bound_exc_pat_binding body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_exc_pat_binding (datacon_delayed, exccon_delayed, func_delayed, sorte_delayed, var_env) body in
  if not (Datacon.Subst.is_id datacon_delayed && Exccon.Subst.is_id exccon_delayed && Func.Subst.is_id func_delayed && Sorte.Subst.is_id sorte_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.exc_pat_binding_delayed <- (Datacon.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction.exc_pat_binding <- body
  end;
  body

and open2_exc_pat_binding : opaque_exc_pat_binding -> opaque_exc_pat_binding -> exc_pat_binding * exc_pat_binding = fun x1 x2 -> 
  change_invalid_to_open2 open2i_exc_pat_binding x1 x2

and open2i_exc_pat_binding : opaque_exc_pat_binding -> opaque_exc_pat_binding -> exc_pat_binding * exc_pat_binding = fun abstraction1 abstraction2 ->
  let (datacon_delayed1, exccon_delayed1, func_delayed1, sorte_delayed1, var_delayed1) = abstraction1.exc_pat_binding_delayed in
  let body1 = abstraction1.exc_pat_binding in
  let (datacon_delayed2, exccon_delayed2, func_delayed2, sorte_delayed2, var_delayed2) = abstraction2.exc_pat_binding_delayed in
  let body2 = abstraction2.exc_pat_binding in
  let (var_env1, var_env2) = freshen2_exc_pat_binding (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_exc_pat_binding (datacon_delayed1, exccon_delayed1, func_delayed1, sorte_delayed1, var_env1) body1 in
  let body2 = subst_exc_pat_binding (datacon_delayed2, exccon_delayed2, func_delayed2, sorte_delayed2, var_env2) body2 in
  if not (Datacon.Subst.is_id datacon_delayed1 && Exccon.Subst.is_id exccon_delayed1 && Func.Subst.is_id func_delayed1 && Sorte.Subst.is_id sorte_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.exc_pat_binding_delayed <- (Datacon.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction1.exc_pat_binding <- body1
  end;
  if not (Datacon.Subst.is_id datacon_delayed2 && Exccon.Subst.is_id exccon_delayed2 && Func.Subst.is_id func_delayed2 && Sorte.Subst.is_id sorte_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.exc_pat_binding_delayed <- (Datacon.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction2.exc_pat_binding <- body2
  end;
  body1, body2

and apply_exc_pat_binding = 
  fun (datacon_env, exccon_env, func_env, sorte_env, var_env) abstraction ->
    let (datacon_delayed, exccon_delayed, func_delayed, sorte_delayed, var_delayed) = abstraction.exc_pat_binding_delayed in {
      abstraction with exc_pat_binding_delayed = (Datacon.Subst.compose datacon_env datacon_delayed, Exccon.Subst.compose exccon_env exccon_delayed, Func.Subst.compose func_env func_delayed, Sorte.Subst.compose sorte_env sorte_delayed, Var.Subst.compose var_env var_delayed)
    }

and import_handler : datacon Identifier.Map.t * exccon Identifier.Map.t * func Identifier.Map.t * sorte Identifier.Map.t * var Identifier.Map.t -> Raw.handler -> handler = fun (datacon_env, exccon_env, func_env, sorte_env, var_env) -> function
  (_exc_pat_binding0) ->
      let (var_bvars) = bvi_exc_pat_binding _exc_pat_binding0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _exc_pat_binding0 = import_exc_pat_binding (datacon_env, exccon_env, func_env, sorte_env, var_ienv) _exc_pat_binding0 in
    (create_exc_pat_binding _exc_pat_binding0)

and subst_handler : Datacon.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Var.Subst.t -> handler -> handler = fun (datacon_env, exccon_env, func_env, sorte_env, var_env) -> function
  (_exc_pat_binding0) ->
    (apply_exc_pat_binding (datacon_env, exccon_env, func_env, sorte_env, var_env) _exc_pat_binding0)

and export_handler : Datacon.AtomIdMap.t * Exccon.AtomIdMap.t * Func.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> handler -> Raw.handler = fun (datacon_m, exccon_m, func_m, sorte_m, var_m) -> function
  (_exc_pat_binding0) ->
      let exc_pat_binding = open_exc_pat_binding _exc_pat_binding0 in
      let (var_bvars) = bound_exc_pat_binding exc_pat_binding in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
    (export_exc_pat_binding (datacon_m, exccon_m, func_m, sorte_m, var_im) exc_pat_binding)

and flatten_handler : handler -> Flat.handler = function
  (_exc_pat_binding0) ->
      let exc_pat_binding = open_exc_pat_binding _exc_pat_binding0 in
    (flatten_exc_pat_binding exc_pat_binding)

and unflatten_handler : Flat.handler -> handler = function
  (_exc_pat_binding0) ->
      let exc_pat_binding = unflatten_exc_pat_binding _exc_pat_binding0 in
    (create_exc_pat_binding exc_pat_binding)

and free_handler : handler -> Datacon.AtomSet.t * Exccon.AtomSet.t * Func.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function handler -> free_accu_handler (Datacon.AtomSet.empty, Exccon.AtomSet.empty, Func.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) handler

and equal_handler : handler -> handler -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_handler x1 x2

and aeq_handler : unit -> handler -> handler -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_exc_pat_binding0), (_exc_pat_binding1) ->
      let _exc_pat_binding0, _exc_pat_binding1 = open2i_exc_pat_binding _exc_pat_binding0 _exc_pat_binding1 in
      aeq_exc_pat_binding () _exc_pat_binding0 _exc_pat_binding1;
      ()

and free_accu_handler = fun (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) -> function
  (_exc_pat_binding0) ->
      let exc_pat_binding = open_exc_pat_binding _exc_pat_binding0 in
      let (var_bvars, var_ifvars, datacon_fvars, exccon_fvars, func_fvars, sorte_fvars) = bound_free_accu_exc_pat_binding (Var.AtomSet.empty, Var.AtomSet.empty, datacon_fvars, exccon_fvars, func_fvars, sorte_fvars) exc_pat_binding in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (datacon_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars)

and import_normal_postcondition : datacon Identifier.Map.t * datatype Identifier.Map.t * sorte Identifier.Map.t * var Identifier.Map.t -> Raw.normal_postcondition -> normal_postcondition = fun (datacon_env, datatype_env, sorte_env, var_env) -> function
  (_guarded_tuple0) ->
      let (var_bvars) = bvi_guarded_tuple _guarded_tuple0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _guarded_tuple0 = import_guarded_tuple (datacon_env, datatype_env, sorte_env, var_ienv) _guarded_tuple0 in
    (create_guarded_tuple _guarded_tuple0)

and subst_normal_postcondition : Datacon.Subst.t * Datatype.Subst.t * Sorte.Subst.t * Var.Subst.t -> normal_postcondition -> normal_postcondition = fun (datacon_env, datatype_env, sorte_env, var_env) -> function
  (_guarded_tuple0) ->
    (apply_guarded_tuple (datacon_env, datatype_env, sorte_env, var_env) _guarded_tuple0)

and export_normal_postcondition : Datacon.AtomIdMap.t * Datatype.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> normal_postcondition -> Raw.normal_postcondition = fun (datacon_m, datatype_m, sorte_m, var_m) -> function
  (_guarded_tuple0) ->
      let guarded_tuple = open_guarded_tuple _guarded_tuple0 in
      let (var_bvars) = bound_guarded_tuple guarded_tuple in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
    (export_guarded_tuple (datacon_m, datatype_m, sorte_m, var_im) guarded_tuple)

and flatten_normal_postcondition : normal_postcondition -> Flat.normal_postcondition = function
  (_guarded_tuple0) ->
      let guarded_tuple = open_guarded_tuple _guarded_tuple0 in
    (flatten_guarded_tuple guarded_tuple)

and unflatten_normal_postcondition : Flat.normal_postcondition -> normal_postcondition = function
  (_guarded_tuple0) ->
      let guarded_tuple = unflatten_guarded_tuple _guarded_tuple0 in
    (create_guarded_tuple guarded_tuple)

and free_normal_postcondition : normal_postcondition -> Datacon.AtomSet.t * Datatype.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function normal_postcondition -> free_accu_normal_postcondition (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) normal_postcondition

and equal_normal_postcondition : normal_postcondition -> normal_postcondition -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_normal_postcondition x1 x2

and aeq_normal_postcondition : unit -> normal_postcondition -> normal_postcondition -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_guarded_tuple0), (_guarded_tuple1) ->
      let _guarded_tuple0, _guarded_tuple1 = open2i_guarded_tuple _guarded_tuple0 _guarded_tuple1 in
      aeq_guarded_tuple () _guarded_tuple0 _guarded_tuple1;
      ()

and free_accu_normal_postcondition = fun (datacon_fvars, datatype_fvars, sorte_fvars, var_fvars) -> function
  (_guarded_tuple0) ->
      let guarded_tuple = open_guarded_tuple _guarded_tuple0 in
      let (var_bvars, var_ifvars, datacon_fvars, datatype_fvars, sorte_fvars) = bound_free_accu_guarded_tuple (Var.AtomSet.empty, Var.AtomSet.empty, datacon_fvars, datatype_fvars, sorte_fvars) guarded_tuple in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (datacon_fvars, datatype_fvars, sorte_fvars, var_fvars)

and import_exceptional_postcondition : datacon Identifier.Map.t * exccon Identifier.Map.t * sorte Identifier.Map.t * var Identifier.Map.t -> Raw.exceptional_postcondition -> exceptional_postcondition = fun (datacon_env, exccon_env, sorte_env, var_env) -> function
  (_exccon1, _guarded_params0) ->
      let (var_bvars) = bvi_guarded_params _guarded_params0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _guarded_params0 = import_guarded_params (datacon_env, sorte_env, var_ienv) _guarded_params0 in
    (Exccon.find _exccon1 exccon_env, create_guarded_params _guarded_params0)

and subst_exceptional_postcondition : Datacon.Subst.t * Exccon.Subst.t * Sorte.Subst.t * Var.Subst.t -> exceptional_postcondition -> exceptional_postcondition = fun (datacon_env, exccon_env, sorte_env, var_env) -> function
  (_exccon1, _guarded_params0) ->
    (Exccon.Subst.lookup _exccon1 exccon_env, apply_guarded_params (datacon_env, sorte_env, var_env) _guarded_params0)

and export_exceptional_postcondition : Datacon.AtomIdMap.t * Exccon.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> exceptional_postcondition -> Raw.exceptional_postcondition = fun (datacon_m, exccon_m, sorte_m, var_m) -> function
  (_exccon1, _guarded_params0) ->
      let guarded_params = open_guarded_params _guarded_params0 in
      let (var_bvars) = bound_guarded_params guarded_params in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
    (Exccon.AtomIdMap.lookup _exccon1 exccon_m, export_guarded_params (datacon_m, sorte_m, var_im) guarded_params)

and flatten_exceptional_postcondition : exceptional_postcondition -> Flat.exceptional_postcondition = function
  (_exccon1, _guarded_params0) ->
      let guarded_params = open_guarded_params _guarded_params0 in
    (_exccon1, flatten_guarded_params guarded_params)

and unflatten_exceptional_postcondition : Flat.exceptional_postcondition -> exceptional_postcondition = function
  (_exccon1, _guarded_params0) ->
      let guarded_params = unflatten_guarded_params _guarded_params0 in
    (_exccon1, create_guarded_params guarded_params)

and free_exceptional_postcondition : exceptional_postcondition -> Datacon.AtomSet.t * Exccon.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function exceptional_postcondition -> free_accu_exceptional_postcondition (Datacon.AtomSet.empty, Exccon.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) exceptional_postcondition

and equal_exceptional_postcondition : exceptional_postcondition -> exceptional_postcondition -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_exceptional_postcondition x1 x2

and aeq_exceptional_postcondition : unit -> exceptional_postcondition -> exceptional_postcondition -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_exccon1, _guarded_params0), (_exccon3, _guarded_params2) ->
      if not (Exccon.Atom.equal _exccon1 _exccon3) then raise (Invalid_argument "aeq_exceptional_postcondition");
      let _guarded_params0, _guarded_params2 = open2i_guarded_params _guarded_params0 _guarded_params2 in
      aeq_guarded_params () _guarded_params0 _guarded_params2;
      ()

and free_accu_exceptional_postcondition = fun (datacon_fvars, exccon_fvars, sorte_fvars, var_fvars) -> function
  (_exccon1, _guarded_params0) ->
      let exccon_fvars = Exccon.AtomSet.add _exccon1 exccon_fvars in
      let guarded_params = open_guarded_params _guarded_params0 in
      let (var_bvars, var_ifvars, datacon_fvars, sorte_fvars) = bound_free_accu_guarded_params (Var.AtomSet.empty, Var.AtomSet.empty, datacon_fvars, sorte_fvars) guarded_params in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (datacon_fvars, exccon_fvars, sorte_fvars, var_fvars)

and import_postcondition : datacon Identifier.Map.t * datatype Identifier.Map.t * exccon Identifier.Map.t * sorte Identifier.Map.t * var Identifier.Map.t -> Raw.postcondition -> postcondition = fun (datacon_env, datatype_env, exccon_env, sorte_env, var_env) -> function
  { Raw.post_normal = _normal_postcondition2; Raw.post_exceptional = _exceptional_postconditions1; Raw.post_other_exceptions = _x0 } ->
    { post_normal = (import_normal_postcondition (datacon_env, datatype_env, sorte_env, var_env)) _normal_postcondition2; post_exceptional = List.map (import_exceptional_postcondition (datacon_env, exccon_env, sorte_env, var_env)) _exceptional_postconditions1; post_other_exceptions = _x0 }

and subst_postcondition : Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Sorte.Subst.t * Var.Subst.t -> postcondition -> postcondition = fun (datacon_env, datatype_env, exccon_env, sorte_env, var_env) -> function
  { post_normal = _normal_postcondition2; post_exceptional = _exceptional_postconditions1; post_other_exceptions = _x0 } ->
    { post_normal = (subst_normal_postcondition (datacon_env, datatype_env, sorte_env, var_env)) _normal_postcondition2; post_exceptional = List.map (subst_exceptional_postcondition (datacon_env, exccon_env, sorte_env, var_env)) _exceptional_postconditions1; post_other_exceptions = _x0 }

and export_postcondition : Datacon.AtomIdMap.t * Datatype.AtomIdMap.t * Exccon.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> postcondition -> Raw.postcondition = fun (datacon_m, datatype_m, exccon_m, sorte_m, var_m) -> function
  { post_normal = _normal_postcondition2; post_exceptional = _exceptional_postconditions1; post_other_exceptions = _x0 } ->
    { Raw.post_normal = (export_normal_postcondition (datacon_m, datatype_m, sorte_m, var_m)) _normal_postcondition2; Raw.post_exceptional = List.map (export_exceptional_postcondition (datacon_m, exccon_m, sorte_m, var_m)) _exceptional_postconditions1; Raw.post_other_exceptions = _x0 }

and flatten_postcondition : postcondition -> Flat.postcondition = function
  { post_normal = _normal_postcondition2; post_exceptional = _exceptional_postconditions1; post_other_exceptions = _x0 } ->
    { Flat.post_normal = flatten_normal_postcondition _normal_postcondition2; Flat.post_exceptional = List.map flatten_exceptional_postcondition _exceptional_postconditions1; Flat.post_other_exceptions = _x0 }

and unflatten_postcondition : Flat.postcondition -> postcondition = function
  { Flat.post_normal = _normal_postcondition2; Flat.post_exceptional = _exceptional_postconditions1; Flat.post_other_exceptions = _x0 } ->
    { post_normal = unflatten_normal_postcondition _normal_postcondition2; post_exceptional = List.map unflatten_exceptional_postcondition _exceptional_postconditions1; post_other_exceptions = _x0 }

and free_postcondition : postcondition -> Datacon.AtomSet.t * Datatype.AtomSet.t * Exccon.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function postcondition -> free_accu_postcondition (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Exccon.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) postcondition

and equal_postcondition : postcondition -> postcondition -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_postcondition x1 x2

and aeq_postcondition : unit -> postcondition -> postcondition -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | { post_normal = _normal_postcondition2; post_exceptional = _exceptional_postconditions1; post_other_exceptions = _x0 }, { post_normal = _normal_postcondition5; post_exceptional = _exceptional_postconditions4; post_other_exceptions = _x3 } ->
      aeq_normal_postcondition () _normal_postcondition2 _normal_postcondition5;
      List.fold_left2 aeq_exceptional_postcondition () _exceptional_postconditions1 _exceptional_postconditions4;
      ()

and free_accu_postcondition = fun (datacon_fvars, datatype_fvars, exccon_fvars, sorte_fvars, var_fvars) -> function
  { post_normal = _normal_postcondition2; post_exceptional = _exceptional_postconditions1; post_other_exceptions = _x0 } ->
      let (datacon_fvars, datatype_fvars, sorte_fvars, var_fvars) = free_accu_normal_postcondition (datacon_fvars, datatype_fvars, sorte_fvars, var_fvars) _normal_postcondition2 in 
      let (datacon_fvars, exccon_fvars, sorte_fvars, var_fvars) = List.fold_left free_accu_exceptional_postcondition (datacon_fvars, exccon_fvars, sorte_fvars, var_fvars) _exceptional_postconditions1 in 
      (datacon_fvars, datatype_fvars, exccon_fvars, sorte_fvars, var_fvars)

and subst_fundef : Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Var.Subst.t -> fundef -> fundef = fun (datacon_oenv, datatype_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv) -> function
  { fun_params = _tuple3; fun_precondition = _contrainte2; fun_postcondition = _postcondition1; fun_body = _expression0 } ->
    { fun_params = (subst_tuple (datatype_oenv, sorte_oenv, var_ienv)) _tuple3; fun_precondition = (subst_contrainte (datacon_oenv, sorte_oenv, var_ienv)) _contrainte2; fun_postcondition = (subst_postcondition (datacon_oenv, datatype_oenv, exccon_oenv, sorte_oenv, var_ienv)) _postcondition1; fun_body = (subst_expression (datacon_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv)) _expression0 }

and bound_fundef : fundef -> Var.AtomSet.t = 
  function fundef -> bound_accu_fundef (Var.AtomSet.empty) fundef

and bound_free_fundef : fundef -> Var.AtomSet.t * Var.AtomSet.t * Datacon.AtomSet.t * Datatype.AtomSet.t * Exccon.AtomSet.t * Func.AtomSet.t * Sorte.AtomSet.t = 
  function fundef -> bound_free_accu_fundef (Var.AtomSet.empty, Var.AtomSet.empty, Datacon.AtomSet.empty, Datatype.AtomSet.empty, Exccon.AtomSet.empty, Func.AtomSet.empty, Sorte.AtomSet.empty) fundef

and equal_fundef : fundef -> fundef -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_fundef x1 x2

and import_fundef = fun (datacon_oenv, datatype_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv) -> function
  { Raw.fun_params = _tuple3; Raw.fun_precondition = _contrainte2; Raw.fun_postcondition = _postcondition1; Raw.fun_body = _expression0 } ->
    { fun_params = (import_tuple (datatype_oenv, sorte_oenv, var_ienv)) _tuple3; fun_precondition = (import_contrainte (datacon_oenv, sorte_oenv, var_ienv)) _contrainte2; fun_postcondition = (import_postcondition (datacon_oenv, datatype_oenv, exccon_oenv, sorte_oenv, var_ienv)) _postcondition1; fun_body = (import_expression (datacon_oenv, exccon_oenv, func_oenv, sorte_oenv, var_ienv)) _expression0 }

and bvi_accu_fundef = fun (var_bvars) -> function
  { Raw.fun_params = _tuple3; Raw.fun_precondition = _contrainte2; Raw.fun_postcondition = _postcondition1; Raw.fun_body = _expression0 } ->
      let (var_bvars) = bvi_accu_tuple (var_bvars) _tuple3 in
      (var_bvars)

and bvi_fundef = 
  function fundef -> bvi_accu_fundef (Identifier.Map.empty) fundef

and bound_accu_fundef = fun (var_bvars) -> function
  { fun_params = _tuple3; fun_precondition = _contrainte2; fun_postcondition = _postcondition1; fun_body = _expression0 } ->
      let (var_bvars) = bound_accu_tuple (var_bvars) _tuple3 in
      (var_bvars)

and export_fundef : Datacon.AtomIdMap.t * Datatype.AtomIdMap.t * Exccon.AtomIdMap.t * Func.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> fundef -> Raw.fundef = fun (datacon_om, datatype_om, exccon_om, func_om, sorte_om, var_im) -> function
  { fun_params = _tuple3; fun_precondition = _contrainte2; fun_postcondition = _postcondition1; fun_body = _expression0 } ->
    { Raw.fun_params = (export_tuple (datatype_om, sorte_om, var_im)) _tuple3; Raw.fun_precondition = (export_contrainte (datacon_om, sorte_om, var_im)) _contrainte2; Raw.fun_postcondition = (export_postcondition (datacon_om, datatype_om, exccon_om, sorte_om, var_im)) _postcondition1; Raw.fun_body = (export_expression (datacon_om, exccon_om, func_om, sorte_om, var_im)) _expression0 }

and flatten_fundef : fundef -> Flat.fundef = function
  { fun_params = _tuple3; fun_precondition = _contrainte2; fun_postcondition = _postcondition1; fun_body = _expression0 } ->
    { Flat.fun_params = flatten_tuple _tuple3; Flat.fun_precondition = flatten_contrainte _contrainte2; Flat.fun_postcondition = flatten_postcondition _postcondition1; Flat.fun_body = flatten_expression _expression0 }

and unflatten_fundef : Flat.fundef -> fundef = function
  { Flat.fun_params = _tuple3; Flat.fun_precondition = _contrainte2; Flat.fun_postcondition = _postcondition1; Flat.fun_body = _expression0 } ->
    { fun_params = unflatten_tuple _tuple3; fun_precondition = unflatten_contrainte _contrainte2; fun_postcondition = unflatten_postcondition _postcondition1; fun_body = unflatten_expression _expression0 }

and bound_free_accu_fundef = fun (var_bvars, var_ifvars, datacon_ofvars, datatype_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars) -> function
  { fun_params = _tuple3; fun_precondition = _contrainte2; fun_postcondition = _postcondition1; fun_body = _expression0 } ->
      let (var_bvars, datatype_ofvars, sorte_ofvars) = bound_free_accu_tuple (var_bvars, datatype_ofvars, sorte_ofvars) _tuple3 in
      let (datacon_ofvars, sorte_ofvars, var_ifvars) = free_accu_contrainte (datacon_ofvars, sorte_ofvars, var_ifvars) _contrainte2 in
      let (datacon_ofvars, datatype_ofvars, exccon_ofvars, sorte_ofvars, var_ifvars) = free_accu_postcondition (datacon_ofvars, datatype_ofvars, exccon_ofvars, sorte_ofvars, var_ifvars) _postcondition1 in
      let (datacon_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars, var_ifvars) = free_accu_expression (datacon_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars, var_ifvars) _expression0 in
      (var_bvars, var_ifvars, datacon_ofvars, datatype_ofvars, exccon_ofvars, func_ofvars, sorte_ofvars)

and aeq_fundef : unit -> fundef -> fundef -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | { fun_params = _tuple3; fun_precondition = _contrainte2; fun_postcondition = _postcondition1; fun_body = _expression0 }, { fun_params = _tuple7; fun_precondition = _contrainte6; fun_postcondition = _postcondition5; fun_body = _expression4 } ->
      aeq_tuple () _tuple3 _tuple7;
      aeq_contrainte () _contrainte2 _contrainte6;
      aeq_postcondition () _postcondition1 _postcondition5;
      aeq_expression () _expression0 _expression4;
      ()

and freshen2_fundef : Var.Subst.t * Var.Subst.t -> fundef -> fundef -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | { fun_params = _tuple3; fun_precondition = _contrainte2; fun_postcondition = _postcondition1; fun_body = _expression0 }, { fun_params = _tuple7; fun_precondition = _contrainte6; fun_postcondition = _postcondition5; fun_body = _expression4 } ->
      let (var_env1, var_env2) = freshen2_tuple (var_env1, var_env2) _tuple3 _tuple7 in
      (var_env1, var_env2)

and create_fundef : fundef -> opaque_fundef = 
  function body -> {
    fundef_delayed = (Datacon.Subst.id, Datatype.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    fundef = body
  }

and open_fundef : opaque_fundef -> fundef = function abstraction ->
  let (datacon_delayed, datatype_delayed, exccon_delayed, func_delayed, sorte_delayed, var_delayed) = abstraction.fundef_delayed in
  let body = abstraction.fundef in
  let (var_bvars) = bound_fundef body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_fundef (datacon_delayed, datatype_delayed, exccon_delayed, func_delayed, sorte_delayed, var_env) body in
  if not (Datacon.Subst.is_id datacon_delayed && Datatype.Subst.is_id datatype_delayed && Exccon.Subst.is_id exccon_delayed && Func.Subst.is_id func_delayed && Sorte.Subst.is_id sorte_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.fundef_delayed <- (Datacon.Subst.id, Datatype.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction.fundef <- body
  end;
  body

and open2_fundef : opaque_fundef -> opaque_fundef -> fundef * fundef = fun x1 x2 -> 
  change_invalid_to_open2 open2i_fundef x1 x2

and open2i_fundef : opaque_fundef -> opaque_fundef -> fundef * fundef = fun abstraction1 abstraction2 ->
  let (datacon_delayed1, datatype_delayed1, exccon_delayed1, func_delayed1, sorte_delayed1, var_delayed1) = abstraction1.fundef_delayed in
  let body1 = abstraction1.fundef in
  let (datacon_delayed2, datatype_delayed2, exccon_delayed2, func_delayed2, sorte_delayed2, var_delayed2) = abstraction2.fundef_delayed in
  let body2 = abstraction2.fundef in
  let (var_env1, var_env2) = freshen2_fundef (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_fundef (datacon_delayed1, datatype_delayed1, exccon_delayed1, func_delayed1, sorte_delayed1, var_env1) body1 in
  let body2 = subst_fundef (datacon_delayed2, datatype_delayed2, exccon_delayed2, func_delayed2, sorte_delayed2, var_env2) body2 in
  if not (Datacon.Subst.is_id datacon_delayed1 && Datatype.Subst.is_id datatype_delayed1 && Exccon.Subst.is_id exccon_delayed1 && Func.Subst.is_id func_delayed1 && Sorte.Subst.is_id sorte_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.fundef_delayed <- (Datacon.Subst.id, Datatype.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction1.fundef <- body1
  end;
  if not (Datacon.Subst.is_id datacon_delayed2 && Datatype.Subst.is_id datatype_delayed2 && Exccon.Subst.is_id exccon_delayed2 && Func.Subst.is_id func_delayed2 && Sorte.Subst.is_id sorte_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.fundef_delayed <- (Datacon.Subst.id, Datatype.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction2.fundef <- body2
  end;
  body1, body2

and apply_fundef = 
  fun (datacon_env, datatype_env, exccon_env, func_env, sorte_env, var_env) abstraction ->
    let (datacon_delayed, datatype_delayed, exccon_delayed, func_delayed, sorte_delayed, var_delayed) = abstraction.fundef_delayed in {
      abstraction with fundef_delayed = (Datacon.Subst.compose datacon_env datacon_delayed, Datatype.Subst.compose datatype_env datatype_delayed, Exccon.Subst.compose exccon_env exccon_delayed, Func.Subst.compose func_env func_delayed, Sorte.Subst.compose sorte_env sorte_delayed, Var.Subst.compose var_env var_delayed)
    }

and import_fundef1 : datacon Identifier.Map.t * datatype Identifier.Map.t * exccon Identifier.Map.t * func Identifier.Map.t * sorte Identifier.Map.t * var Identifier.Map.t -> Raw.fundef1 -> fundef1 = fun (datacon_env, datatype_env, exccon_env, func_env, sorte_env, var_env) -> function
  (_fundef0) ->
      let (var_bvars) = bvi_fundef _fundef0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _fundef0 = import_fundef (datacon_env, datatype_env, exccon_env, func_env, sorte_env, var_ienv) _fundef0 in
    (create_fundef _fundef0)

and subst_fundef1 : Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Var.Subst.t -> fundef1 -> fundef1 = fun (datacon_env, datatype_env, exccon_env, func_env, sorte_env, var_env) -> function
  (_fundef0) ->
    (apply_fundef (datacon_env, datatype_env, exccon_env, func_env, sorte_env, var_env) _fundef0)

and export_fundef1 : Datacon.AtomIdMap.t * Datatype.AtomIdMap.t * Exccon.AtomIdMap.t * Func.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> fundef1 -> Raw.fundef1 = fun (datacon_m, datatype_m, exccon_m, func_m, sorte_m, var_m) -> function
  (_fundef0) ->
      let fundef = open_fundef _fundef0 in
      let (var_bvars) = bound_fundef fundef in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
    (export_fundef (datacon_m, datatype_m, exccon_m, func_m, sorte_m, var_im) fundef)

and flatten_fundef1 : fundef1 -> Flat.fundef1 = function
  (_fundef0) ->
      let fundef = open_fundef _fundef0 in
    (flatten_fundef fundef)

and unflatten_fundef1 : Flat.fundef1 -> fundef1 = function
  (_fundef0) ->
      let fundef = unflatten_fundef _fundef0 in
    (create_fundef fundef)

and free_fundef1 : fundef1 -> Datacon.AtomSet.t * Datatype.AtomSet.t * Exccon.AtomSet.t * Func.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function fundef1 -> free_accu_fundef1 (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Exccon.AtomSet.empty, Func.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) fundef1

and equal_fundef1 : fundef1 -> fundef1 -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_fundef1 x1 x2

and aeq_fundef1 : unit -> fundef1 -> fundef1 -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_fundef0), (_fundef1) ->
      let _fundef0, _fundef1 = open2i_fundef _fundef0 _fundef1 in
      aeq_fundef () _fundef0 _fundef1;
      ()

and free_accu_fundef1 = fun (datacon_fvars, datatype_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) -> function
  (_fundef0) ->
      let fundef = open_fundef _fundef0 in
      let (var_bvars, var_ifvars, datacon_fvars, datatype_fvars, exccon_fvars, func_fvars, sorte_fvars) = bound_free_accu_fundef (Var.AtomSet.empty, Var.AtomSet.empty, datacon_fvars, datatype_fvars, exccon_fvars, func_fvars, sorte_fvars) fundef in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (datacon_fvars, datatype_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars)

and subst_def : Var.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t -> def -> def = fun (var_oenv, datacon_ienv, datatype_ienv, exccon_ienv, func_ienv, sorte_ienv) -> function
  | DefSort (_sorte0) ->
      DefSort (Sorte.Subst.lookup _sorte0 sorte_ienv)
  | DefDataType (_datatypedef0) ->
      DefDataType ((subst_datatypedef (var_oenv, datacon_ienv, datatype_ienv, sorte_ienv)) _datatypedef0)
  | DefLemma (_lemmadef0) ->
      DefLemma ((subst_lemmadef (datacon_ienv, datatype_ienv, sorte_ienv, var_oenv)) _lemmadef0)
  | DefException (_excdef0) ->
      DefException ((subst_excdef (var_oenv, datacon_ienv, datatype_ienv, exccon_ienv, sorte_ienv)) _excdef0)
  | DefFun (_func1, _fundef10) ->
      DefFun (Func.Subst.lookup _func1 func_ienv, (subst_fundef1 (datacon_ienv, datatype_ienv, exccon_ienv, func_ienv, sorte_ienv, var_oenv)) _fundef10)
  | DefBuiltin (_func0) ->
      DefBuiltin (Func.Subst.lookup _func0 func_ienv)

and bound_def : def -> Datacon.AtomSet.t * Datatype.AtomSet.t * Exccon.AtomSet.t * Func.AtomSet.t * Sorte.AtomSet.t = 
  function def -> bound_accu_def (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Exccon.AtomSet.empty, Func.AtomSet.empty, Sorte.AtomSet.empty) def

and bound_free_def : def -> Datacon.AtomSet.t * Datatype.AtomSet.t * Exccon.AtomSet.t * Func.AtomSet.t * Sorte.AtomSet.t * Datacon.AtomSet.t * Datatype.AtomSet.t * Exccon.AtomSet.t * Func.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function def -> bound_free_accu_def (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Exccon.AtomSet.empty, Func.AtomSet.empty, Sorte.AtomSet.empty, Datacon.AtomSet.empty, Datatype.AtomSet.empty, Exccon.AtomSet.empty, Func.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) def

and equal_def : def -> def -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_def x1 x2

and import_def = fun (var_oenv, datacon_ienv, datatype_ienv, exccon_ienv, func_ienv, sorte_ienv) -> function
  | Raw.DefSort (_sorte0) ->
      DefSort (Sorte.find _sorte0 sorte_ienv)
  | Raw.DefDataType (_datatypedef0) ->
      DefDataType ((import_datatypedef (var_oenv, datacon_ienv, datatype_ienv, sorte_ienv)) _datatypedef0)
  | Raw.DefLemma (_lemmadef0) ->
      DefLemma ((import_lemmadef (datacon_ienv, datatype_ienv, sorte_ienv, var_oenv)) _lemmadef0)
  | Raw.DefException (_excdef0) ->
      DefException ((import_excdef (var_oenv, datacon_ienv, datatype_ienv, exccon_ienv, sorte_ienv)) _excdef0)
  | Raw.DefFun (_func1, _fundef10) ->
      DefFun (Func.find _func1 func_ienv, (import_fundef1 (datacon_ienv, datatype_ienv, exccon_ienv, func_ienv, sorte_ienv, var_oenv)) _fundef10)
  | Raw.DefBuiltin (_func0) ->
      DefBuiltin (Func.find _func0 func_ienv)

and bvi_accu_def = fun (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars) -> function
  | Raw.DefSort (_sorte0) ->
      let sorte_bvars = Identifier.Map.add _sorte0 () sorte_bvars in
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars)
  | Raw.DefDataType (_datatypedef0) ->
      let (datacon_bvars, datatype_bvars) = bvi_accu_datatypedef (datacon_bvars, datatype_bvars) _datatypedef0 in
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars)
  | Raw.DefLemma (_lemmadef0) ->
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars)
  | Raw.DefException (_excdef0) ->
      let (exccon_bvars) = bvi_accu_excdef (exccon_bvars) _excdef0 in
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars)
  | Raw.DefFun (_func1, _fundef10) ->
      let func_bvars = Identifier.Map.add _func1 () func_bvars in
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars)
  | Raw.DefBuiltin (_func0) ->
      let func_bvars = Identifier.Map.add _func0 () func_bvars in
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars)

and bvi_def = 
  function def -> bvi_accu_def (Identifier.Map.empty, Identifier.Map.empty, Identifier.Map.empty, Identifier.Map.empty, Identifier.Map.empty) def

and bound_accu_def = fun (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars) -> function
  | DefSort (_sorte0) ->
      let sorte_bvars = Sorte.AtomSet.add _sorte0 sorte_bvars in
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars)
  | DefDataType (_datatypedef0) ->
      let (datacon_bvars, datatype_bvars) = bound_accu_datatypedef (datacon_bvars, datatype_bvars) _datatypedef0 in
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars)
  | DefLemma (_lemmadef0) ->
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars)
  | DefException (_excdef0) ->
      let (exccon_bvars) = bound_accu_excdef (exccon_bvars) _excdef0 in
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars)
  | DefFun (_func1, _fundef10) ->
      let func_bvars = Func.AtomSet.add _func1 func_bvars in
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars)
  | DefBuiltin (_func0) ->
      let func_bvars = Func.AtomSet.add _func0 func_bvars in
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars)

and export_def : Var.AtomIdMap.t * Datacon.AtomIdMap.t * Datatype.AtomIdMap.t * Exccon.AtomIdMap.t * Func.AtomIdMap.t * Sorte.AtomIdMap.t -> def -> Raw.def = fun (var_om, datacon_im, datatype_im, exccon_im, func_im, sorte_im) -> function
  | DefSort (_sorte0) ->
      Raw.DefSort (Sorte.AtomIdMap.lookup _sorte0 sorte_im)
  | DefDataType (_datatypedef0) ->
      Raw.DefDataType ((export_datatypedef (var_om, datacon_im, datatype_im, sorte_im)) _datatypedef0)
  | DefLemma (_lemmadef0) ->
      Raw.DefLemma ((export_lemmadef (datacon_im, datatype_im, sorte_im, var_om)) _lemmadef0)
  | DefException (_excdef0) ->
      Raw.DefException ((export_excdef (var_om, datacon_im, datatype_im, exccon_im, sorte_im)) _excdef0)
  | DefFun (_func1, _fundef10) ->
      Raw.DefFun (Func.AtomIdMap.lookup _func1 func_im, (export_fundef1 (datacon_im, datatype_im, exccon_im, func_im, sorte_im, var_om)) _fundef10)
  | DefBuiltin (_func0) ->
      Raw.DefBuiltin (Func.AtomIdMap.lookup _func0 func_im)

and flatten_def : def -> Flat.def = function
  | DefSort (_sorte0) ->
      Flat.DefSort (_sorte0)
  | DefDataType (_datatypedef0) ->
      Flat.DefDataType (flatten_datatypedef _datatypedef0)
  | DefLemma (_lemmadef0) ->
      Flat.DefLemma (flatten_lemmadef _lemmadef0)
  | DefException (_excdef0) ->
      Flat.DefException (flatten_excdef _excdef0)
  | DefFun (_func1, _fundef10) ->
      Flat.DefFun (_func1, flatten_fundef1 _fundef10)
  | DefBuiltin (_func0) ->
      Flat.DefBuiltin (_func0)

and unflatten_def : Flat.def -> def = function
  | Flat.DefSort (_sorte0) ->
      DefSort (_sorte0)
  | Flat.DefDataType (_datatypedef0) ->
      DefDataType (unflatten_datatypedef _datatypedef0)
  | Flat.DefLemma (_lemmadef0) ->
      DefLemma (unflatten_lemmadef _lemmadef0)
  | Flat.DefException (_excdef0) ->
      DefException (unflatten_excdef _excdef0)
  | Flat.DefFun (_func1, _fundef10) ->
      DefFun (_func1, unflatten_fundef1 _fundef10)
  | Flat.DefBuiltin (_func0) ->
      DefBuiltin (_func0)

and bound_free_accu_def = fun (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars, datacon_ifvars, datatype_ifvars, exccon_ifvars, func_ifvars, sorte_ifvars, var_ofvars) -> function
  | DefSort (_sorte0) ->
      let sorte_bvars = Sorte.AtomSet.add _sorte0 sorte_bvars in
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars, datacon_ifvars, datatype_ifvars, exccon_ifvars, func_ifvars, sorte_ifvars, var_ofvars)
  | DefDataType (_datatypedef0) ->
      let (datacon_bvars, datatype_bvars, datacon_ifvars, datatype_ifvars, sorte_ifvars, var_ofvars) = bound_free_accu_datatypedef (datacon_bvars, datatype_bvars, datacon_ifvars, datatype_ifvars, sorte_ifvars, var_ofvars) _datatypedef0 in
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars, datacon_ifvars, datatype_ifvars, exccon_ifvars, func_ifvars, sorte_ifvars, var_ofvars)
  | DefLemma (_lemmadef0) ->
      let (datacon_ifvars, datatype_ifvars, sorte_ifvars, var_ofvars) = free_accu_lemmadef (datacon_ifvars, datatype_ifvars, sorte_ifvars, var_ofvars) _lemmadef0 in
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars, datacon_ifvars, datatype_ifvars, exccon_ifvars, func_ifvars, sorte_ifvars, var_ofvars)
  | DefException (_excdef0) ->
      let (exccon_bvars, datacon_ifvars, datatype_ifvars, sorte_ifvars, var_ofvars) = bound_free_accu_excdef (exccon_bvars, datacon_ifvars, datatype_ifvars, sorte_ifvars, var_ofvars) _excdef0 in
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars, datacon_ifvars, datatype_ifvars, exccon_ifvars, func_ifvars, sorte_ifvars, var_ofvars)
  | DefFun (_func1, _fundef10) ->
      let func_bvars = Func.AtomSet.add _func1 func_bvars in
      let (datacon_ifvars, datatype_ifvars, exccon_ifvars, func_ifvars, sorte_ifvars, var_ofvars) = free_accu_fundef1 (datacon_ifvars, datatype_ifvars, exccon_ifvars, func_ifvars, sorte_ifvars, var_ofvars) _fundef10 in
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars, datacon_ifvars, datatype_ifvars, exccon_ifvars, func_ifvars, sorte_ifvars, var_ofvars)
  | DefBuiltin (_func0) ->
      let func_bvars = Func.AtomSet.add _func0 func_bvars in
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars, datacon_ifvars, datatype_ifvars, exccon_ifvars, func_ifvars, sorte_ifvars, var_ofvars)

and aeq_def : unit -> def -> def -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | DefSort (_sorte0), DefSort (_sorte1) ->
      if not (Sorte.Atom.equal _sorte0 _sorte1) then raise (Invalid_argument "aeq_def");
      ()
  | DefDataType (_datatypedef0), DefDataType (_datatypedef1) ->
      aeq_datatypedef () _datatypedef0 _datatypedef1;
      ()
  | DefLemma (_lemmadef0), DefLemma (_lemmadef1) ->
      aeq_lemmadef () _lemmadef0 _lemmadef1;
      ()
  | DefException (_excdef0), DefException (_excdef1) ->
      aeq_excdef () _excdef0 _excdef1;
      ()
  | DefFun (_func1, _fundef10), DefFun (_func3, _fundef12) ->
      if not (Func.Atom.equal _func1 _func3) then raise (Invalid_argument "aeq_def");
      aeq_fundef1 () _fundef10 _fundef12;
      ()
  | DefBuiltin (_func0), DefBuiltin (_func1) ->
      if not (Func.Atom.equal _func0 _func1) then raise (Invalid_argument "aeq_def");
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_def")

and freshen2_def : Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t -> def -> def -> Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t = fun (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | DefSort (_sorte0), DefSort (_sorte1) ->
      let sorte_env1, sorte_env2 = Sorte.Subst.freshen2 _sorte0 sorte_env1 _sorte1 sorte_env2 in
      (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2)
  | DefDataType (_datatypedef0), DefDataType (_datatypedef1) ->
      let (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2) = freshen2_datatypedef (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2) _datatypedef0 _datatypedef1 in
      (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2)
  | DefLemma (_lemmadef0), DefLemma (_lemmadef1) ->
      (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2)
  | DefException (_excdef0), DefException (_excdef1) ->
      let (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2) = freshen2_excdef (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2) _excdef0 _excdef1 in
      (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2)
  | DefFun (_func1, _fundef10), DefFun (_func3, _fundef12) ->
      let func_env1, func_env2 = Func.Subst.freshen2 _func1 func_env1 _func3 func_env2 in
      (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2)
  | DefBuiltin (_func0), DefBuiltin (_func1) ->
      let func_env1, func_env2 = Func.Subst.freshen2 _func0 func_env1 _func1 func_env2 in
      (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2)
  | _, _ ->
      raise (Invalid_argument "freshen2_def")

and subst_defs : Var.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t -> defs -> defs = fun (var_oenv, datacon_ienv, datatype_ienv, exccon_ienv, func_ienv, sorte_ienv) -> function
  (_defs0) ->
    (List.map (subst_def (var_oenv, datacon_ienv, datatype_ienv, exccon_ienv, func_ienv, sorte_ienv)) _defs0)

and bound_defs : defs -> Datacon.AtomSet.t * Datatype.AtomSet.t * Exccon.AtomSet.t * Func.AtomSet.t * Sorte.AtomSet.t = 
  function defs -> bound_accu_defs (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Exccon.AtomSet.empty, Func.AtomSet.empty, Sorte.AtomSet.empty) defs

and bound_free_defs : defs -> Datacon.AtomSet.t * Datatype.AtomSet.t * Exccon.AtomSet.t * Func.AtomSet.t * Sorte.AtomSet.t * Datacon.AtomSet.t * Datatype.AtomSet.t * Exccon.AtomSet.t * Func.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function defs -> bound_free_accu_defs (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Exccon.AtomSet.empty, Func.AtomSet.empty, Sorte.AtomSet.empty, Datacon.AtomSet.empty, Datatype.AtomSet.empty, Exccon.AtomSet.empty, Func.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) defs

and equal_defs : defs -> defs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_defs x1 x2

and import_defs = fun (var_oenv, datacon_ienv, datatype_ienv, exccon_ienv, func_ienv, sorte_ienv) -> function
  (_defs0) ->
    (List.map (import_def (var_oenv, datacon_ienv, datatype_ienv, exccon_ienv, func_ienv, sorte_ienv)) _defs0)

and bvi_accu_defs = fun (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars) -> function
  (_defs0) ->
      let (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars) = List.fold_left bvi_accu_def (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars) _defs0 in
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars)

and bvi_defs = 
  function defs -> bvi_accu_defs (Identifier.Map.empty, Identifier.Map.empty, Identifier.Map.empty, Identifier.Map.empty, Identifier.Map.empty) defs

and bound_accu_defs = fun (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars) -> function
  (_defs0) ->
      let (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars) = List.fold_left bound_accu_def (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars) _defs0 in
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars)

and export_defs : Var.AtomIdMap.t * Datacon.AtomIdMap.t * Datatype.AtomIdMap.t * Exccon.AtomIdMap.t * Func.AtomIdMap.t * Sorte.AtomIdMap.t -> defs -> Raw.defs = fun (var_om, datacon_im, datatype_im, exccon_im, func_im, sorte_im) -> function
  (_defs0) ->
    (List.map (export_def (var_om, datacon_im, datatype_im, exccon_im, func_im, sorte_im)) _defs0)

and flatten_defs : defs -> Flat.defs = function
  (_defs0) ->
    (List.map flatten_def _defs0)

and unflatten_defs : Flat.defs -> defs = function
  (_defs0) ->
    (List.map unflatten_def _defs0)

and bound_free_accu_defs = fun (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars, datacon_ifvars, datatype_ifvars, exccon_ifvars, func_ifvars, sorte_ifvars, var_ofvars) -> function
  (_defs0) ->
      let (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars, datacon_ifvars, datatype_ifvars, exccon_ifvars, func_ifvars, sorte_ifvars, var_ofvars) = List.fold_left bound_free_accu_def (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars, datacon_ifvars, datatype_ifvars, exccon_ifvars, func_ifvars, sorte_ifvars, var_ofvars) _defs0 in
      (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars, datacon_ifvars, datatype_ifvars, exccon_ifvars, func_ifvars, sorte_ifvars, var_ofvars)

and aeq_defs : unit -> defs -> defs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_defs0), (_defs1) ->
      List.fold_left2 aeq_def () _defs0 _defs1;
      ()

and freshen2_defs : Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t -> defs -> defs -> Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t = fun (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_defs0), (_defs1) ->
      let (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2) = List.fold_left2 freshen2_def (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2) _defs0 _defs1 in
      (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2)

and create_defs : defs -> opaque_defs = 
  function body -> {
    defs_delayed = (Datacon.Subst.id, Datatype.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    defs = body
  }

and open_defs : opaque_defs -> defs = function abstraction ->
  let (datacon_delayed, datatype_delayed, exccon_delayed, func_delayed, sorte_delayed, var_delayed) = abstraction.defs_delayed in
  let body = abstraction.defs in
  let (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars) = bound_defs body in
  let datacon_env = Datacon.Subst.freshen datacon_bvars datacon_delayed in
  let datatype_env = Datatype.Subst.freshen datatype_bvars datatype_delayed in
  let exccon_env = Exccon.Subst.freshen exccon_bvars exccon_delayed in
  let func_env = Func.Subst.freshen func_bvars func_delayed in
  let sorte_env = Sorte.Subst.freshen sorte_bvars sorte_delayed in
  let body = subst_defs (var_delayed, datacon_env, datatype_env, exccon_env, func_env, sorte_env) body in
  if not (Datacon.Subst.is_id datacon_delayed && Datatype.Subst.is_id datatype_delayed && Exccon.Subst.is_id exccon_delayed && Func.Subst.is_id func_delayed && Sorte.Subst.is_id sorte_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.defs_delayed <- (Datacon.Subst.id, Datatype.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction.defs <- body
  end;
  body

and open2_defs : opaque_defs -> opaque_defs -> defs * defs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_defs x1 x2

and open2i_defs : opaque_defs -> opaque_defs -> defs * defs = fun abstraction1 abstraction2 ->
  let (datacon_delayed1, datatype_delayed1, exccon_delayed1, func_delayed1, sorte_delayed1, var_delayed1) = abstraction1.defs_delayed in
  let body1 = abstraction1.defs in
  let (datacon_delayed2, datatype_delayed2, exccon_delayed2, func_delayed2, sorte_delayed2, var_delayed2) = abstraction2.defs_delayed in
  let body2 = abstraction2.defs in
  let (datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2) = freshen2_defs (Datacon.Subst.id, Datatype.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Datacon.Subst.id, Datatype.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id) body1 body2 in
  let datacon_env1 = Datacon.Subst.union datacon_delayed1 datacon_env1 in
  let datatype_env1 = Datatype.Subst.union datatype_delayed1 datatype_env1 in
  let exccon_env1 = Exccon.Subst.union exccon_delayed1 exccon_env1 in
  let func_env1 = Func.Subst.union func_delayed1 func_env1 in
  let sorte_env1 = Sorte.Subst.union sorte_delayed1 sorte_env1 in
  let datacon_env2 = Datacon.Subst.union datacon_delayed2 datacon_env2 in
  let datatype_env2 = Datatype.Subst.union datatype_delayed2 datatype_env2 in
  let exccon_env2 = Exccon.Subst.union exccon_delayed2 exccon_env2 in
  let func_env2 = Func.Subst.union func_delayed2 func_env2 in
  let sorte_env2 = Sorte.Subst.union sorte_delayed2 sorte_env2 in
  let body1 = subst_defs (var_delayed1, datacon_env1, datatype_env1, exccon_env1, func_env1, sorte_env1) body1 in
  let body2 = subst_defs (var_delayed2, datacon_env2, datatype_env2, exccon_env2, func_env2, sorte_env2) body2 in
  if not (Datacon.Subst.is_id datacon_delayed1 && Datatype.Subst.is_id datatype_delayed1 && Exccon.Subst.is_id exccon_delayed1 && Func.Subst.is_id func_delayed1 && Sorte.Subst.is_id sorte_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.defs_delayed <- (Datacon.Subst.id, Datatype.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction1.defs <- body1
  end;
  if not (Datacon.Subst.is_id datacon_delayed2 && Datatype.Subst.is_id datatype_delayed2 && Exccon.Subst.is_id exccon_delayed2 && Func.Subst.is_id func_delayed2 && Sorte.Subst.is_id sorte_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.defs_delayed <- (Datacon.Subst.id, Datatype.Subst.id, Exccon.Subst.id, Func.Subst.id, Sorte.Subst.id, Var.Subst.id);
    abstraction2.defs <- body2
  end;
  body1, body2

and apply_defs = 
  fun (datacon_env, datatype_env, exccon_env, func_env, sorte_env, var_env) abstraction ->
    let (datacon_delayed, datatype_delayed, exccon_delayed, func_delayed, sorte_delayed, var_delayed) = abstraction.defs_delayed in {
      abstraction with defs_delayed = (Datacon.Subst.compose datacon_env datacon_delayed, Datatype.Subst.compose datatype_env datatype_delayed, Exccon.Subst.compose exccon_env exccon_delayed, Func.Subst.compose func_env func_delayed, Sorte.Subst.compose sorte_env sorte_delayed, Var.Subst.compose var_env var_delayed)
    }

and import_program : datacon Identifier.Map.t * datatype Identifier.Map.t * exccon Identifier.Map.t * func Identifier.Map.t * sorte Identifier.Map.t * var Identifier.Map.t -> Raw.program -> program = fun (datacon_env, datatype_env, exccon_env, func_env, sorte_env, var_env) -> function
  (_defs0) ->
      let (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars) = bvi_defs _defs0 in 
      let datacon_ienv = Datacon.Atom.mfreshb datacon_bvars datacon_env in
      let datatype_ienv = Datatype.Atom.mfreshb datatype_bvars datatype_env in
      let exccon_ienv = Exccon.Atom.mfreshb exccon_bvars exccon_env in
      let func_ienv = Func.Atom.mfreshb func_bvars func_env in
      let sorte_ienv = Sorte.Atom.mfreshb sorte_bvars sorte_env in
      let _defs0 = import_defs (var_env, datacon_ienv, datatype_ienv, exccon_ienv, func_ienv, sorte_ienv) _defs0 in
    (create_defs _defs0)

and subst_program : Datacon.Subst.t * Datatype.Subst.t * Exccon.Subst.t * Func.Subst.t * Sorte.Subst.t * Var.Subst.t -> program -> program = fun (datacon_env, datatype_env, exccon_env, func_env, sorte_env, var_env) -> function
  (_defs0) ->
    (apply_defs (datacon_env, datatype_env, exccon_env, func_env, sorte_env, var_env) _defs0)

and export_program : Datacon.AtomIdMap.t * Datatype.AtomIdMap.t * Exccon.AtomIdMap.t * Func.AtomIdMap.t * Sorte.AtomIdMap.t * Var.AtomIdMap.t -> program -> Raw.program = fun (datacon_m, datatype_m, exccon_m, func_m, sorte_m, var_m) -> function
  (_defs0) ->
      let defs = open_defs _defs0 in
      let (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars) = bound_defs defs in
      let datacon_im = Datacon.AtomIdMap.add_set datacon_bvars datacon_m in
      let datatype_im = Datatype.AtomIdMap.add_set datatype_bvars datatype_m in
      let exccon_im = Exccon.AtomIdMap.add_set exccon_bvars exccon_m in
      let func_im = Func.AtomIdMap.add_set func_bvars func_m in
      let sorte_im = Sorte.AtomIdMap.add_set sorte_bvars sorte_m in
    (export_defs (var_m, datacon_im, datatype_im, exccon_im, func_im, sorte_im) defs)

and flatten_program : program -> Flat.program = function
  (_defs0) ->
      let defs = open_defs _defs0 in
    (flatten_defs defs)

and unflatten_program : Flat.program -> program = function
  (_defs0) ->
      let defs = unflatten_defs _defs0 in
    (create_defs defs)

and free_program : program -> Datacon.AtomSet.t * Datatype.AtomSet.t * Exccon.AtomSet.t * Func.AtomSet.t * Sorte.AtomSet.t * Var.AtomSet.t = 
  function program -> free_accu_program (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Exccon.AtomSet.empty, Func.AtomSet.empty, Sorte.AtomSet.empty, Var.AtomSet.empty) program

and equal_program : program -> program -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_program x1 x2

and aeq_program : unit -> program -> program -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_defs0), (_defs1) ->
      let _defs0, _defs1 = open2i_defs _defs0 _defs1 in
      aeq_defs () _defs0 _defs1;
      ()

and free_accu_program = fun (datacon_fvars, datatype_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars) -> function
  (_defs0) ->
      let defs = open_defs _defs0 in
      let (datacon_bvars, datatype_bvars, exccon_bvars, func_bvars, sorte_bvars, datacon_ifvars, datatype_ifvars, exccon_ifvars, func_ifvars, sorte_ifvars, var_fvars) = bound_free_accu_defs (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Exccon.AtomSet.empty, Func.AtomSet.empty, Sorte.AtomSet.empty, Datacon.AtomSet.empty, Datatype.AtomSet.empty, Exccon.AtomSet.empty, Func.AtomSet.empty, Sorte.AtomSet.empty, var_fvars) defs in
      let datacon_fvars = Datacon.AtomSet.union datacon_fvars (Datacon.AtomSet.diff datacon_ifvars datacon_bvars) in
      let datatype_fvars = Datatype.AtomSet.union datatype_fvars (Datatype.AtomSet.diff datatype_ifvars datatype_bvars) in
      let exccon_fvars = Exccon.AtomSet.union exccon_fvars (Exccon.AtomSet.diff exccon_ifvars exccon_bvars) in
      let func_fvars = Func.AtomSet.union func_fvars (Func.AtomSet.diff func_ifvars func_bvars) in
      let sorte_fvars = Sorte.AtomSet.union sorte_fvars (Sorte.AtomSet.diff sorte_ifvars sorte_bvars) in
      (datacon_fvars, datatype_fvars, exccon_fvars, func_fvars, sorte_fvars, var_fvars)

class map = object(self)

  method asorte : asorte -> asorte = function
  (_sorte0) ->
    (_sorte0)

  method sorts : sorts -> sorts = function
  (_asortes0) ->
    (List.map (self#asorte) _asortes0)

  method adatatype : adatatype -> adatatype = function
  (_datatype0) ->
    (_datatype0)

  method adatacon : adatacon -> adatacon = function
  (_datacon0) ->
    (_datacon0)

  method aexccon : aexccon -> aexccon = function
  (_exccon0) ->
    (_exccon0)

  method avar : avar -> avar = function
  (_var0) ->
    (_var0)

  method avaro : avaro -> avaro = function
  (_avars0) ->
    (option_map (self#avar) _avars0)

  method set_function : set_function -> set_function = function
  | SFSupport ->
      self#sfsupport
  | SFOuter ->
      self#sfouter
  | SFInner ->
      self#sfinner
  | SFBound ->
      self#sfbound

  method sfsupport : set_function = SFSupport

  method sfouter : set_function = SFOuter

  method sfinner : set_function = SFInner

  method sfbound : set_function = SFBound

  method value : value -> value = function
  | VVar (_var0) ->
      self#vvar (_var0)
  | VBool (_x0) ->
      self#vbool (_x0)
  | VData (_datacon1, _values0) ->
      self#vdata (_datacon1, _values0)

  method vvar : var -> value = 
  function (_var0) -> 
      VVar (_var0)

  method vbool : ( bool ) -> value = 
  function (_x0) -> 
      VBool (_x0)

  method vdata : datacon * value list -> value = 
  function (_datacon1, _values0) -> 
      VData (_datacon1, List.map (self#value) _values0)

  method value_tuple : value_tuple -> value_tuple = function
  | VTComponent (_value0) ->
      self#vtcomponent (_value0)
  | VTInner (_sorts1, _value_tuple0) ->
      self#vtinner (_sorts1, _value_tuple0)
  | VTOuter (_sorts1, _value_tuple0) ->
      self#vtouter (_sorts1, _value_tuple0)
  | VTAbstraction (_sorts1, _value_tuple0) ->
      self#vtabstraction (_sorts1, _value_tuple0)
  | VTTuple (_value_tuples0) ->
      self#vttuple (_value_tuples0)

  method vtcomponent : value -> value_tuple = 
  function (_value0) -> 
      VTComponent ((self#value) _value0)

  method vtinner : sorts * value_tuple -> value_tuple = 
  function (_sorts1, _value_tuple0) -> 
      VTInner ((self#sorts) _sorts1, (self#value_tuple) _value_tuple0)

  method vtouter : sorts * value_tuple -> value_tuple = 
  function (_sorts1, _value_tuple0) -> 
      VTOuter ((self#sorts) _sorts1, (self#value_tuple) _value_tuple0)

  method vtabstraction : sorts * value_tuple -> value_tuple = 
  function (_sorts1, _value_tuple0) -> 
      VTAbstraction ((self#sorts) _sorts1, (self#value_tuple) _value_tuple0)

  method vttuple : value_tuple list -> value_tuple = 
  function (_value_tuples0) -> 
      VTTuple (List.map (self#value_tuple) _value_tuples0)

  method setsetset_operator : setsetset_operator -> setsetset_operator = function
  | OpUnion ->
      self#opunion
  | OpIntersection ->
      self#opintersection
  | OpDifference ->
      self#opdifference

  method opunion : setsetset_operator = OpUnion

  method opintersection : setsetset_operator = OpIntersection

  method opdifference : setsetset_operator = OpDifference

  method raw_set_expression : raw_set_expression -> raw_set_expression = function
  | SEmpty ->
      self#sempty
  | SUniverse ->
      self#suniverse
  | SSort (_sorte0) ->
      self#ssort (_sorte0)
  | SApp (_set_function2, _asortes1, _value_tuple0) ->
      self#sapp (_set_function2, _asortes1, _value_tuple0)
  | SAssocOp (_setsetset_operator1, _set_expressions0) ->
      self#sassocop (_setsetset_operator1, _set_expressions0)
  | SConditional (_contrainte2, _set_expression1, _set_expression0) ->
      self#sconditional (_contrainte2, _set_expression1, _set_expression0)

  method sempty : raw_set_expression = SEmpty

  method suniverse : raw_set_expression = SUniverse

  method ssort : sorte -> raw_set_expression = 
  function (_sorte0) -> 
      SSort (_sorte0)

  method sapp : set_function * asorte option * value_tuple -> raw_set_expression = 
  function (_set_function2, _asortes1, _value_tuple0) -> 
      SApp ((self#set_function) _set_function2, option_map (self#asorte) _asortes1, (self#value_tuple) _value_tuple0)

  method sassocop : setsetset_operator * set_expression list -> raw_set_expression = 
  function (_setsetset_operator1, _set_expressions0) -> 
      SAssocOp ((self#setsetset_operator) _setsetset_operator1, List.map (self#set_expression) _set_expressions0)

  method sconditional : contrainte * set_expression * set_expression -> raw_set_expression = 
  function (_contrainte2, _set_expression1, _set_expression0) -> 
      SConditional ((self#contrainte) _contrainte2, (self#set_expression) _set_expression1, (self#set_expression) _set_expression0)

  method set_expression : set_expression -> set_expression = function
  (_raw_set_expressions0) ->
    (Annotation.map (self#raw_set_expression) _raw_set_expressions0)

  method setsetbool_operator : setsetbool_operator -> setsetbool_operator = function
  | OpSubset ->
      self#opsubset
  | OpEqual ->
      self#opequal
  | OpNotEqual ->
      self#opnotequal
  | OpDisjoint ->
      self#opdisjoint

  method opsubset : setsetbool_operator = OpSubset

  method opequal : setsetbool_operator = OpEqual

  method opnotequal : setsetbool_operator = OpNotEqual

  method opdisjoint : setsetbool_operator = OpDisjoint

  method boolboolbool_operator : boolboolbool_operator -> boolboolbool_operator = function
  | OpConjunction ->
      self#opconjunction
  | OpDisjunction ->
      self#opdisjunction
  | OpImplication ->
      self#opimplication
  | OpEquivalence ->
      self#opequivalence

  method opconjunction : boolboolbool_operator = OpConjunction

  method opdisjunction : boolboolbool_operator = OpDisjunction

  method opimplication : boolboolbool_operator = OpImplication

  method opequivalence : boolboolbool_operator = OpEquivalence

  method raw_constraint : raw_constraint -> raw_constraint = function
  | FTrue ->
      self#ftrue
  | FFalse ->
      self#ffalse
  | FBoolVar (_var0) ->
      self#fboolvar (_var0)
  | FNot (_contrainte0) ->
      self#fnot (_contrainte0)
  | FBoolAssocOp (_boolboolbool_operator1, _contraintes0) ->
      self#fboolassocop (_boolboolbool_operator1, _contraintes0)
  | FSetBinOp (_set_expression2, _setsetbool_operator1, _set_expression0) ->
      self#fsetbinop (_set_expression2, _setsetbool_operator1, _set_expression0)

  method ftrue : raw_constraint = FTrue

  method ffalse : raw_constraint = FFalse

  method fboolvar : var -> raw_constraint = 
  function (_var0) -> 
      FBoolVar (_var0)

  method fnot : contrainte -> raw_constraint = 
  function (_contrainte0) -> 
      FNot ((self#contrainte) _contrainte0)

  method fboolassocop : boolboolbool_operator * contrainte list -> raw_constraint = 
  function (_boolboolbool_operator1, _contraintes0) -> 
      FBoolAssocOp ((self#boolboolbool_operator) _boolboolbool_operator1, List.map (self#contrainte) _contraintes0)

  method fsetbinop : set_expression * setsetbool_operator * set_expression -> raw_constraint = 
  function (_set_expression2, _setsetbool_operator1, _set_expression0) -> 
      FSetBinOp ((self#set_expression) _set_expression2, (self#setsetbool_operator) _setsetbool_operator1, (self#set_expression) _set_expression0)

  method contrainte : contrainte -> contrainte = function
  (_raw_constraints0) ->
    (Annotation.map (self#raw_constraint) _raw_constraints0)

  method raw_type : raw_type -> raw_type = function
  | TAtom (_sorte0) ->
      self#tatom (_sorte0)
  | TAtomSet (_sorte0) ->
      self#tatomset (_sorte0)
  | TBool ->
      self#tbool
  | TData (_datatype0) ->
      self#tdata (_datatype0)

  method tatom : sorte -> raw_type = 
  function (_sorte0) -> 
      TAtom (_sorte0)

  method tatomset : sorte -> raw_type = 
  function (_sorte0) -> 
      TAtomSet (_sorte0)

  method tbool : raw_type = TBool

  method tdata : datatype -> raw_type = 
  function (_datatype0) -> 
      TData (_datatype0)

  method typ : typ -> typ = function
  (_raw_types0) ->
    (Annotation.map (self#raw_type) _raw_types0)

  method raw_tuple : raw_tuple -> raw_tuple = function
  | TComponent (_typ0) ->
      self#tcomponent (_typ0)
  | TInner (_sorts1, _tuple0) ->
      self#tinner (_sorts1, _tuple0)
  | TOuter (_sorts1, _tuple0) ->
      self#touter (_sorts1, _tuple0)
  | TAbstraction (_sorts1, _tuple0) ->
      self#tabstraction (_sorts1, _tuple0)
  | TTuple (_tuples0) ->
      self#ttuple (_tuples0)
  | TName (_var1, _tuple0) ->
      self#tname (_var1, _tuple0)

  method tcomponent : typ -> raw_tuple = 
  function (_typ0) -> 
      TComponent ((self#typ) _typ0)

  method tinner : sorts * tuple -> raw_tuple = 
  function (_sorts1, _tuple0) -> 
      TInner ((self#sorts) _sorts1, (self#tuple) _tuple0)

  method touter : sorts * tuple -> raw_tuple = 
  function (_sorts1, _tuple0) -> 
      TOuter ((self#sorts) _sorts1, (self#tuple) _tuple0)

  method tabstraction : sorts * tuple -> raw_tuple = 
  function (_sorts1, _tuple0) -> 
      TAbstraction ((self#sorts) _sorts1, (self#tuple) _tuple0)

  method ttuple : tuple list -> raw_tuple = 
  function (_tuples0) -> 
      TTuple (List.map (self#tuple) _tuples0)

  method tname : var * tuple -> raw_tuple = 
  function (_var1, _tuple0) -> 
      TName (_var1, (self#tuple) _tuple0)

  method tuple : tuple -> tuple = function
  (_raw_tuples0) ->
    (Annotation.map (self#raw_tuple) _raw_tuples0)

  method guarded_tuple : guarded_tuple -> guarded_tuple = function
  (_tuple1, _contrainte0) ->
    ((self#tuple) _tuple1, (self#contrainte) _contrainte0)

  method guarded_params : guarded_params -> guarded_params = function
  (_avaros1, _contrainte0) ->
    (List.map (self#avaro) _avaros1, (self#contrainte) _contrainte0)

  method datatypedef : datatypedef -> datatypedef = function
  { datatype_name = _datatype2; datatype_sorts = _sorts1; datatype_constructors = _datacondefs0 } ->
    { datatype_name = self#datatype_name _datatype2; datatype_sorts = self#datatype_sorts _sorts1; datatype_constructors = self#datatype_constructors _datacondefs0 }

  method datatype_name : datatype -> datatype = 
  function _datatype0 -> _datatype0

  method datatype_sorts : sorts -> sorts = 
  function _sorts0 -> (self#sorts) _sorts0

  method datatype_constructors : datacondef list -> datacondef list = 
  function _datacondefs0 -> List.map (self#datacondef) _datacondefs0

  method datacondef : datacondef -> datacondef = function
  (_datacon1, _dataconparams0) ->
    (_datacon1, (self#dataconparams) _dataconparams0)

  method dataconparams : dataconparams -> dataconparams = function
  (_guarded_tuple0) ->
    (create_guarded_tuple (self#guarded_tuple (open_guarded_tuple _guarded_tuple0)))

  method lemmadef : lemmadef -> lemmadef = function
  (_lemma0) ->
    (create_lemma (self#lemma (open_lemma _lemma0)))

  method lemma : lemma -> lemma = function
  (_var2, _adatatype1, _contrainte0) ->
    (_var2, (self#adatatype) _adatatype1, (self#contrainte) _contrainte0)

  method excdef : excdef -> excdef = function
  (_exccon1, _dataconparams0) ->
    (_exccon1, (self#dataconparams) _dataconparams0)

  method raw_pattern : raw_pattern -> raw_pattern = function
  | PZero ->
      self#pzero
  | POne ->
      self#pone
  | PVar (_var0) ->
      self#pvar (_var0)
  | PBool (_x0) ->
      self#pbool (_x0)
  | PData (_adatacon1, _patterns0) ->
      self#pdata (_adatacon1, _patterns0)
  | PConjunction (_patterns0) ->
      self#pconjunction (_patterns0)
  | PDisjunction (_patterns0) ->
      self#pdisjunction (_patterns0)
  | PTuple (_patterns0) ->
      self#ptuple (_patterns0)

  method pzero : raw_pattern = PZero

  method pone : raw_pattern = POne

  method pvar : var -> raw_pattern = 
  function (_var0) -> 
      PVar (_var0)

  method pbool : ( bool ) -> raw_pattern = 
  function (_x0) -> 
      PBool (_x0)

  method pdata : adatacon * pattern list -> raw_pattern = 
  function (_adatacon1, _patterns0) -> 
      PData ((self#adatacon) _adatacon1, List.map (self#pattern) _patterns0)

  method pconjunction : pattern list -> raw_pattern = 
  function (_patterns0) -> 
      PConjunction (List.map (self#pattern) _patterns0)

  method pdisjunction : pattern list -> raw_pattern = 
  function (_patterns0) -> 
      PDisjunction (List.map (self#pattern) _patterns0)

  method ptuple : pattern list -> raw_pattern = 
  function (_patterns0) -> 
      PTuple (List.map (self#pattern) _patterns0)

  method pattern : pattern -> pattern = function
  (_raw_patterns0) ->
    (Annotation.map (self#raw_pattern) _raw_patterns0)

  method raw_exception_pattern : raw_exception_pattern -> raw_exception_pattern = function
  | EPData (_aexccon1, _avaros0) ->
      self#epdata (_aexccon1, _avaros0)
  | EPDisjunction (_exception_patterns0) ->
      self#epdisjunction (_exception_patterns0)

  method epdata : aexccon * avaro list -> raw_exception_pattern = 
  function (_aexccon1, _avaros0) -> 
      EPData ((self#aexccon) _aexccon1, List.map (self#avaro) _avaros0)

  method epdisjunction : exception_pattern list -> raw_exception_pattern = 
  function (_exception_patterns0) -> 
      EPDisjunction (List.map (self#exception_pattern) _exception_patterns0)

  method exception_pattern : exception_pattern -> exception_pattern = function
  (_raw_exception_patterns0) ->
    (Annotation.map (self#raw_exception_pattern) _raw_exception_patterns0)

  method time : time -> time = function
  | CompileTime ->
      self#compiletime
  | RunTime ->
      self#runtime

  method compiletime : time = CompileTime

  method runtime : time = RunTime

  method raw_expression : raw_expression -> raw_expression = function
  | EVar (_var0) ->
      self#evar (_var0)
  | EBool (_x0) ->
      self#ebool (_x0)
  | EData (_datacon1, _expressions0) ->
      self#edata (_datacon1, _expressions0)
  | EFresh (_fresh_binding0) ->
      self#efresh (_fresh_binding0)
  | ECase (_expression1, _branchs0) ->
      self#ecase (_expression1, _branchs0)
  | ECall (_func1, _expression0) ->
      self#ecall (_func1, _expression0)
  | ETuple (_expressions0) ->
      self#etuple (_expressions0)
  | ERaise (_exccon1, _expressions0) ->
      self#eraise (_exccon1, _expressions0)
  | ELetWhereUnless (_expression2, _handlers1, _letw_binding0) ->
      self#eletwhereunless (_expression2, _handlers1, _letw_binding0)
  | ETryUnless (_expression1, _handlers0) ->
      self#etryunless (_expression1, _handlers0)
  | EAssert (_time2, _contrainte1, _expression0) ->
      self#eassert (_time2, _contrainte1, _expression0)
  | EAssertFalse (_time0) ->
      self#eassertfalse (_time0)

  method evar : var -> raw_expression = 
  function (_var0) -> 
      EVar (_var0)

  method ebool : ( bool ) -> raw_expression = 
  function (_x0) -> 
      EBool (_x0)

  method edata : datacon * expression list -> raw_expression = 
  function (_datacon1, _expressions0) -> 
      EData (_datacon1, List.map (self#expression) _expressions0)

  method efresh : opaque_fresh_binding -> raw_expression = 
  function (_fresh_binding0) -> 
      EFresh (create_fresh_binding (self#fresh_binding (open_fresh_binding _fresh_binding0)))

  method ecase : expression * branch list -> raw_expression = 
  function (_expression1, _branchs0) -> 
      ECase ((self#expression) _expression1, List.map (self#branch) _branchs0)

  method ecall : func * expression -> raw_expression = 
  function (_func1, _expression0) -> 
      ECall (_func1, (self#expression) _expression0)

  method etuple : expression list -> raw_expression = 
  function (_expressions0) -> 
      ETuple (List.map (self#expression) _expressions0)

  method eraise : exccon * expression list -> raw_expression = 
  function (_exccon1, _expressions0) -> 
      ERaise (_exccon1, List.map (self#expression) _expressions0)

  method eletwhereunless : expression * handler list * opaque_letw_binding -> raw_expression = 
  function (_expression2, _handlers1, _letw_binding0) -> 
      ELetWhereUnless ((self#expression) _expression2, List.map (self#handler) _handlers1, create_letw_binding (self#letw_binding (open_letw_binding _letw_binding0)))

  method etryunless : expression * handler list -> raw_expression = 
  function (_expression1, _handlers0) -> 
      ETryUnless ((self#expression) _expression1, List.map (self#handler) _handlers0)

  method eassert : time * contrainte * expression -> raw_expression = 
  function (_time2, _contrainte1, _expression0) -> 
      EAssert ((self#time) _time2, (self#contrainte) _contrainte1, (self#expression) _expression0)

  method eassertfalse : time -> raw_expression = 
  function (_time0) -> 
      EAssertFalse ((self#time) _time0)

  method expression : expression -> expression = function
  (_raw_expressions0) ->
    (Annotation.map (self#raw_expression) _raw_expressions0)

  method fresh_binding : fresh_binding -> fresh_binding = function
  (_asorte2, _avars1, _expression0) ->
    ((self#asorte) _asorte2, List.map (self#avar) _avars1, (self#expression) _expression0)

  method letw_binding : letw_binding -> letw_binding = function
  (_avaros2, _contrainte1, _expression0) ->
    (List.map (self#avaro) _avaros2, (self#contrainte) _contrainte1, (self#expression) _expression0)

  method pat_binding : pat_binding -> pat_binding = function
  (_pattern1, _expression0) ->
    ((self#pattern) _pattern1, (self#expression) _expression0)

  method branch : branch -> branch = function
  (_pat_binding0) ->
    (create_pat_binding (self#pat_binding (open_pat_binding _pat_binding0)))

  method exc_pat_binding : exc_pat_binding -> exc_pat_binding = function
  (_exception_pattern1, _expression0) ->
    ((self#exception_pattern) _exception_pattern1, (self#expression) _expression0)

  method handler : handler -> handler = function
  (_exc_pat_binding0) ->
    (create_exc_pat_binding (self#exc_pat_binding (open_exc_pat_binding _exc_pat_binding0)))

  method normal_postcondition : normal_postcondition -> normal_postcondition = function
  (_guarded_tuple0) ->
    (create_guarded_tuple (self#guarded_tuple (open_guarded_tuple _guarded_tuple0)))

  method exceptional_postcondition : exceptional_postcondition -> exceptional_postcondition = function
  (_exccon1, _guarded_params0) ->
    (_exccon1, create_guarded_params (self#guarded_params (open_guarded_params _guarded_params0)))

  method postcondition : postcondition -> postcondition = function
  { post_normal = _normal_postcondition2; post_exceptional = _exceptional_postconditions1; post_other_exceptions = _x0 } ->
    { post_normal = self#post_normal _normal_postcondition2; post_exceptional = self#post_exceptional _exceptional_postconditions1; post_other_exceptions = self#post_other_exceptions _x0 }

  method post_normal : normal_postcondition -> normal_postcondition = 
  function _normal_postcondition0 -> (self#normal_postcondition) _normal_postcondition0

  method post_exceptional : exceptional_postcondition list -> exceptional_postcondition list = 
  function _exceptional_postconditions0 -> List.map (self#exceptional_postcondition) _exceptional_postconditions0

  method post_other_exceptions : ( bool ) -> ( bool ) = 
  function _x0 -> _x0

  method fundef : fundef -> fundef = function
  { fun_params = _tuple3; fun_precondition = _contrainte2; fun_postcondition = _postcondition1; fun_body = _expression0 } ->
    { fun_params = self#fun_params _tuple3; fun_precondition = self#fun_precondition _contrainte2; fun_postcondition = self#fun_postcondition _postcondition1; fun_body = self#fun_body _expression0 }

  method fun_params : tuple -> tuple = 
  function _tuple0 -> (self#tuple) _tuple0

  method fun_precondition : contrainte -> contrainte = 
  function _contrainte0 -> (self#contrainte) _contrainte0

  method fun_postcondition : postcondition -> postcondition = 
  function _postcondition0 -> (self#postcondition) _postcondition0

  method fun_body : expression -> expression = 
  function _expression0 -> (self#expression) _expression0

  method fundef1 : fundef1 -> fundef1 = function
  (_fundef0) ->
    (create_fundef (self#fundef (open_fundef _fundef0)))

  method def : def -> def = function
  | DefSort (_sorte0) ->
      self#defsort (_sorte0)
  | DefDataType (_datatypedef0) ->
      self#defdatatype (_datatypedef0)
  | DefLemma (_lemmadef0) ->
      self#deflemma (_lemmadef0)
  | DefException (_excdef0) ->
      self#defexception (_excdef0)
  | DefFun (_func1, _fundef10) ->
      self#deffun (_func1, _fundef10)
  | DefBuiltin (_func0) ->
      self#defbuiltin (_func0)

  method defsort : sorte -> def = 
  function (_sorte0) -> 
      DefSort (_sorte0)

  method defdatatype : datatypedef -> def = 
  function (_datatypedef0) -> 
      DefDataType ((self#datatypedef) _datatypedef0)

  method deflemma : lemmadef -> def = 
  function (_lemmadef0) -> 
      DefLemma ((self#lemmadef) _lemmadef0)

  method defexception : excdef -> def = 
  function (_excdef0) -> 
      DefException ((self#excdef) _excdef0)

  method deffun : func * fundef1 -> def = 
  function (_func1, _fundef10) -> 
      DefFun (_func1, (self#fundef1) _fundef10)

  method defbuiltin : func -> def = 
  function (_func0) -> 
      DefBuiltin (_func0)

  method defs : defs -> defs = function
  (_defs0) ->
    (List.map (self#def) _defs0)

  method program : program -> program = function
  (_defs0) ->
    (create_defs (self#defs (open_defs _defs0)))

end

class [ 'accumulator ] fold = object(self)

  method asorte : 'accumulator -> asorte -> 'accumulator = fun accu -> function
  (_sorte0) ->
      accu

  method sorts : 'accumulator -> sorts -> 'accumulator = fun accu -> function
  (_asortes0) ->
      let accu = List.fold_left (self#asorte) accu _asortes0 in
      accu

  method adatatype : 'accumulator -> adatatype -> 'accumulator = fun accu -> function
  (_datatype0) ->
      accu

  method adatacon : 'accumulator -> adatacon -> 'accumulator = fun accu -> function
  (_datacon0) ->
      accu

  method aexccon : 'accumulator -> aexccon -> 'accumulator = fun accu -> function
  (_exccon0) ->
      accu

  method avar : 'accumulator -> avar -> 'accumulator = fun accu -> function
  (_var0) ->
      accu

  method avaro : 'accumulator -> avaro -> 'accumulator = fun accu -> function
  (_avars0) ->
      let accu = option_fold (self#avar) accu _avars0 in
      accu

  method set_function : 'accumulator -> set_function -> 'accumulator = fun accu -> function
  | SFSupport ->
      self#sfsupport accu
  | SFOuter ->
      self#sfouter accu
  | SFInner ->
      self#sfinner accu
  | SFBound ->
      self#sfbound accu

  method sfsupport : 'accumulator -> 'accumulator = fun accu ->       accu

  method sfouter : 'accumulator -> 'accumulator = fun accu ->       accu

  method sfinner : 'accumulator -> 'accumulator = fun accu ->       accu

  method sfbound : 'accumulator -> 'accumulator = fun accu ->       accu

  method value : 'accumulator -> value -> 'accumulator = fun accu -> function
  | VVar (_var0) ->
      self#vvar accu (_var0)
  | VBool (_x0) ->
      self#vbool accu (_x0)
  | VData (_datacon1, _values0) ->
      self#vdata accu (_datacon1, _values0)

  method vvar : 'accumulator -> var -> 'accumulator = fun accu -> 
  function (_var0) -> 
      accu

  method vbool : 'accumulator -> ( bool ) -> 'accumulator = fun accu -> 
  function (_x0) -> 
      accu

  method vdata : 'accumulator -> datacon * value list -> 'accumulator = fun accu -> 
  function (_datacon1, _values0) -> 
      let accu = List.fold_left (self#value) accu _values0 in
      accu

  method value_tuple : 'accumulator -> value_tuple -> 'accumulator = fun accu -> function
  | VTComponent (_value0) ->
      self#vtcomponent accu (_value0)
  | VTInner (_sorts1, _value_tuple0) ->
      self#vtinner accu (_sorts1, _value_tuple0)
  | VTOuter (_sorts1, _value_tuple0) ->
      self#vtouter accu (_sorts1, _value_tuple0)
  | VTAbstraction (_sorts1, _value_tuple0) ->
      self#vtabstraction accu (_sorts1, _value_tuple0)
  | VTTuple (_value_tuples0) ->
      self#vttuple accu (_value_tuples0)

  method vtcomponent : 'accumulator -> value -> 'accumulator = fun accu -> 
  function (_value0) -> 
      let accu = (self#value) accu _value0 in
      accu

  method vtinner : 'accumulator -> sorts * value_tuple -> 'accumulator = fun accu -> 
  function (_sorts1, _value_tuple0) -> 
      let accu = (self#sorts) accu _sorts1 in
      let accu = (self#value_tuple) accu _value_tuple0 in
      accu

  method vtouter : 'accumulator -> sorts * value_tuple -> 'accumulator = fun accu -> 
  function (_sorts1, _value_tuple0) -> 
      let accu = (self#sorts) accu _sorts1 in
      let accu = (self#value_tuple) accu _value_tuple0 in
      accu

  method vtabstraction : 'accumulator -> sorts * value_tuple -> 'accumulator = fun accu -> 
  function (_sorts1, _value_tuple0) -> 
      let accu = (self#sorts) accu _sorts1 in
      let accu = (self#value_tuple) accu _value_tuple0 in
      accu

  method vttuple : 'accumulator -> value_tuple list -> 'accumulator = fun accu -> 
  function (_value_tuples0) -> 
      let accu = List.fold_left (self#value_tuple) accu _value_tuples0 in
      accu

  method setsetset_operator : 'accumulator -> setsetset_operator -> 'accumulator = fun accu -> function
  | OpUnion ->
      self#opunion accu
  | OpIntersection ->
      self#opintersection accu
  | OpDifference ->
      self#opdifference accu

  method opunion : 'accumulator -> 'accumulator = fun accu ->       accu

  method opintersection : 'accumulator -> 'accumulator = fun accu ->       accu

  method opdifference : 'accumulator -> 'accumulator = fun accu ->       accu

  method raw_set_expression : 'accumulator -> raw_set_expression -> 'accumulator = fun accu -> function
  | SEmpty ->
      self#sempty accu
  | SUniverse ->
      self#suniverse accu
  | SSort (_sorte0) ->
      self#ssort accu (_sorte0)
  | SApp (_set_function2, _asortes1, _value_tuple0) ->
      self#sapp accu (_set_function2, _asortes1, _value_tuple0)
  | SAssocOp (_setsetset_operator1, _set_expressions0) ->
      self#sassocop accu (_setsetset_operator1, _set_expressions0)
  | SConditional (_contrainte2, _set_expression1, _set_expression0) ->
      self#sconditional accu (_contrainte2, _set_expression1, _set_expression0)

  method sempty : 'accumulator -> 'accumulator = fun accu ->       accu

  method suniverse : 'accumulator -> 'accumulator = fun accu ->       accu

  method ssort : 'accumulator -> sorte -> 'accumulator = fun accu -> 
  function (_sorte0) -> 
      accu

  method sapp : 'accumulator -> set_function * asorte option * value_tuple -> 'accumulator = fun accu -> 
  function (_set_function2, _asortes1, _value_tuple0) -> 
      let accu = (self#set_function) accu _set_function2 in
      let accu = option_fold (self#asorte) accu _asortes1 in
      let accu = (self#value_tuple) accu _value_tuple0 in
      accu

  method sassocop : 'accumulator -> setsetset_operator * set_expression list -> 'accumulator = fun accu -> 
  function (_setsetset_operator1, _set_expressions0) -> 
      let accu = (self#setsetset_operator) accu _setsetset_operator1 in
      let accu = List.fold_left (self#set_expression) accu _set_expressions0 in
      accu

  method sconditional : 'accumulator -> contrainte * set_expression * set_expression -> 'accumulator = fun accu -> 
  function (_contrainte2, _set_expression1, _set_expression0) -> 
      let accu = (self#contrainte) accu _contrainte2 in
      let accu = (self#set_expression) accu _set_expression1 in
      let accu = (self#set_expression) accu _set_expression0 in
      accu

  method set_expression : 'accumulator -> set_expression -> 'accumulator = fun accu -> function
  (_raw_set_expressions0) ->
      let accu = Annotation.fold (self#raw_set_expression) accu _raw_set_expressions0 in
      accu

  method setsetbool_operator : 'accumulator -> setsetbool_operator -> 'accumulator = fun accu -> function
  | OpSubset ->
      self#opsubset accu
  | OpEqual ->
      self#opequal accu
  | OpNotEqual ->
      self#opnotequal accu
  | OpDisjoint ->
      self#opdisjoint accu

  method opsubset : 'accumulator -> 'accumulator = fun accu ->       accu

  method opequal : 'accumulator -> 'accumulator = fun accu ->       accu

  method opnotequal : 'accumulator -> 'accumulator = fun accu ->       accu

  method opdisjoint : 'accumulator -> 'accumulator = fun accu ->       accu

  method boolboolbool_operator : 'accumulator -> boolboolbool_operator -> 'accumulator = fun accu -> function
  | OpConjunction ->
      self#opconjunction accu
  | OpDisjunction ->
      self#opdisjunction accu
  | OpImplication ->
      self#opimplication accu
  | OpEquivalence ->
      self#opequivalence accu

  method opconjunction : 'accumulator -> 'accumulator = fun accu ->       accu

  method opdisjunction : 'accumulator -> 'accumulator = fun accu ->       accu

  method opimplication : 'accumulator -> 'accumulator = fun accu ->       accu

  method opequivalence : 'accumulator -> 'accumulator = fun accu ->       accu

  method raw_constraint : 'accumulator -> raw_constraint -> 'accumulator = fun accu -> function
  | FTrue ->
      self#ftrue accu
  | FFalse ->
      self#ffalse accu
  | FBoolVar (_var0) ->
      self#fboolvar accu (_var0)
  | FNot (_contrainte0) ->
      self#fnot accu (_contrainte0)
  | FBoolAssocOp (_boolboolbool_operator1, _contraintes0) ->
      self#fboolassocop accu (_boolboolbool_operator1, _contraintes0)
  | FSetBinOp (_set_expression2, _setsetbool_operator1, _set_expression0) ->
      self#fsetbinop accu (_set_expression2, _setsetbool_operator1, _set_expression0)

  method ftrue : 'accumulator -> 'accumulator = fun accu ->       accu

  method ffalse : 'accumulator -> 'accumulator = fun accu ->       accu

  method fboolvar : 'accumulator -> var -> 'accumulator = fun accu -> 
  function (_var0) -> 
      accu

  method fnot : 'accumulator -> contrainte -> 'accumulator = fun accu -> 
  function (_contrainte0) -> 
      let accu = (self#contrainte) accu _contrainte0 in
      accu

  method fboolassocop : 'accumulator -> boolboolbool_operator * contrainte list -> 'accumulator = fun accu -> 
  function (_boolboolbool_operator1, _contraintes0) -> 
      let accu = (self#boolboolbool_operator) accu _boolboolbool_operator1 in
      let accu = List.fold_left (self#contrainte) accu _contraintes0 in
      accu

  method fsetbinop : 'accumulator -> set_expression * setsetbool_operator * set_expression -> 'accumulator = fun accu -> 
  function (_set_expression2, _setsetbool_operator1, _set_expression0) -> 
      let accu = (self#set_expression) accu _set_expression2 in
      let accu = (self#setsetbool_operator) accu _setsetbool_operator1 in
      let accu = (self#set_expression) accu _set_expression0 in
      accu

  method contrainte : 'accumulator -> contrainte -> 'accumulator = fun accu -> function
  (_raw_constraints0) ->
      let accu = Annotation.fold (self#raw_constraint) accu _raw_constraints0 in
      accu

  method raw_type : 'accumulator -> raw_type -> 'accumulator = fun accu -> function
  | TAtom (_sorte0) ->
      self#tatom accu (_sorte0)
  | TAtomSet (_sorte0) ->
      self#tatomset accu (_sorte0)
  | TBool ->
      self#tbool accu
  | TData (_datatype0) ->
      self#tdata accu (_datatype0)

  method tatom : 'accumulator -> sorte -> 'accumulator = fun accu -> 
  function (_sorte0) -> 
      accu

  method tatomset : 'accumulator -> sorte -> 'accumulator = fun accu -> 
  function (_sorte0) -> 
      accu

  method tbool : 'accumulator -> 'accumulator = fun accu ->       accu

  method tdata : 'accumulator -> datatype -> 'accumulator = fun accu -> 
  function (_datatype0) -> 
      accu

  method typ : 'accumulator -> typ -> 'accumulator = fun accu -> function
  (_raw_types0) ->
      let accu = Annotation.fold (self#raw_type) accu _raw_types0 in
      accu

  method raw_tuple : 'accumulator -> raw_tuple -> 'accumulator = fun accu -> function
  | TComponent (_typ0) ->
      self#tcomponent accu (_typ0)
  | TInner (_sorts1, _tuple0) ->
      self#tinner accu (_sorts1, _tuple0)
  | TOuter (_sorts1, _tuple0) ->
      self#touter accu (_sorts1, _tuple0)
  | TAbstraction (_sorts1, _tuple0) ->
      self#tabstraction accu (_sorts1, _tuple0)
  | TTuple (_tuples0) ->
      self#ttuple accu (_tuples0)
  | TName (_var1, _tuple0) ->
      self#tname accu (_var1, _tuple0)

  method tcomponent : 'accumulator -> typ -> 'accumulator = fun accu -> 
  function (_typ0) -> 
      let accu = (self#typ) accu _typ0 in
      accu

  method tinner : 'accumulator -> sorts * tuple -> 'accumulator = fun accu -> 
  function (_sorts1, _tuple0) -> 
      let accu = (self#sorts) accu _sorts1 in
      let accu = (self#tuple) accu _tuple0 in
      accu

  method touter : 'accumulator -> sorts * tuple -> 'accumulator = fun accu -> 
  function (_sorts1, _tuple0) -> 
      let accu = (self#sorts) accu _sorts1 in
      let accu = (self#tuple) accu _tuple0 in
      accu

  method tabstraction : 'accumulator -> sorts * tuple -> 'accumulator = fun accu -> 
  function (_sorts1, _tuple0) -> 
      let accu = (self#sorts) accu _sorts1 in
      let accu = (self#tuple) accu _tuple0 in
      accu

  method ttuple : 'accumulator -> tuple list -> 'accumulator = fun accu -> 
  function (_tuples0) -> 
      let accu = List.fold_left (self#tuple) accu _tuples0 in
      accu

  method tname : 'accumulator -> var * tuple -> 'accumulator = fun accu -> 
  function (_var1, _tuple0) -> 
      let accu = (self#tuple) accu _tuple0 in
      accu

  method tuple : 'accumulator -> tuple -> 'accumulator = fun accu -> function
  (_raw_tuples0) ->
      let accu = Annotation.fold (self#raw_tuple) accu _raw_tuples0 in
      accu

  method guarded_tuple : 'accumulator -> guarded_tuple -> 'accumulator = fun accu -> function
  (_tuple1, _contrainte0) ->
      let accu = (self#tuple) accu _tuple1 in
      let accu = (self#contrainte) accu _contrainte0 in
      accu

  method guarded_params : 'accumulator -> guarded_params -> 'accumulator = fun accu -> function
  (_avaros1, _contrainte0) ->
      let accu = List.fold_left (self#avaro) accu _avaros1 in
      let accu = (self#contrainte) accu _contrainte0 in
      accu

  method datatypedef : 'accumulator -> datatypedef -> 'accumulator = fun accu -> function
  { datatype_name = _datatype2; datatype_sorts = _sorts1; datatype_constructors = _datacondefs0 } ->
      let accu = self#datatype_name accu _datatype2 in
      let accu = self#datatype_sorts accu _sorts1 in
      let accu = self#datatype_constructors accu _datacondefs0 in
      accu

  method datatype_name : 'accumulator -> datatype -> 'accumulator = fun accu -> 
  function _datatype0 ->
      accu

  method datatype_sorts : 'accumulator -> sorts -> 'accumulator = fun accu -> 
  function _sorts0 ->
      let accu = (self#sorts) accu _sorts0 in
      accu

  method datatype_constructors : 'accumulator -> datacondef list -> 'accumulator = fun accu -> 
  function _datacondefs0 ->
      let accu = List.fold_left (self#datacondef) accu _datacondefs0 in
      accu

  method datacondef : 'accumulator -> datacondef -> 'accumulator = fun accu -> function
  (_datacon1, _dataconparams0) ->
      let accu = (self#dataconparams) accu _dataconparams0 in
      accu

  method dataconparams : 'accumulator -> dataconparams -> 'accumulator = fun accu -> function
  (_guarded_tuple0) ->
      let accu = self#guarded_tuple accu (open_guarded_tuple _guarded_tuple0) in
      accu

  method lemmadef : 'accumulator -> lemmadef -> 'accumulator = fun accu -> function
  (_lemma0) ->
      let accu = self#lemma accu (open_lemma _lemma0) in
      accu

  method lemma : 'accumulator -> lemma -> 'accumulator = fun accu -> function
  (_var2, _adatatype1, _contrainte0) ->
      let accu = (self#adatatype) accu _adatatype1 in
      let accu = (self#contrainte) accu _contrainte0 in
      accu

  method excdef : 'accumulator -> excdef -> 'accumulator = fun accu -> function
  (_exccon1, _dataconparams0) ->
      let accu = (self#dataconparams) accu _dataconparams0 in
      accu

  method raw_pattern : 'accumulator -> raw_pattern -> 'accumulator = fun accu -> function
  | PZero ->
      self#pzero accu
  | POne ->
      self#pone accu
  | PVar (_var0) ->
      self#pvar accu (_var0)
  | PBool (_x0) ->
      self#pbool accu (_x0)
  | PData (_adatacon1, _patterns0) ->
      self#pdata accu (_adatacon1, _patterns0)
  | PConjunction (_patterns0) ->
      self#pconjunction accu (_patterns0)
  | PDisjunction (_patterns0) ->
      self#pdisjunction accu (_patterns0)
  | PTuple (_patterns0) ->
      self#ptuple accu (_patterns0)

  method pzero : 'accumulator -> 'accumulator = fun accu ->       accu

  method pone : 'accumulator -> 'accumulator = fun accu ->       accu

  method pvar : 'accumulator -> var -> 'accumulator = fun accu -> 
  function (_var0) -> 
      accu

  method pbool : 'accumulator -> ( bool ) -> 'accumulator = fun accu -> 
  function (_x0) -> 
      accu

  method pdata : 'accumulator -> adatacon * pattern list -> 'accumulator = fun accu -> 
  function (_adatacon1, _patterns0) -> 
      let accu = (self#adatacon) accu _adatacon1 in
      let accu = List.fold_left (self#pattern) accu _patterns0 in
      accu

  method pconjunction : 'accumulator -> pattern list -> 'accumulator = fun accu -> 
  function (_patterns0) -> 
      let accu = List.fold_left (self#pattern) accu _patterns0 in
      accu

  method pdisjunction : 'accumulator -> pattern list -> 'accumulator = fun accu -> 
  function (_patterns0) -> 
      let accu = List.fold_left (self#pattern) accu _patterns0 in
      accu

  method ptuple : 'accumulator -> pattern list -> 'accumulator = fun accu -> 
  function (_patterns0) -> 
      let accu = List.fold_left (self#pattern) accu _patterns0 in
      accu

  method pattern : 'accumulator -> pattern -> 'accumulator = fun accu -> function
  (_raw_patterns0) ->
      let accu = Annotation.fold (self#raw_pattern) accu _raw_patterns0 in
      accu

  method raw_exception_pattern : 'accumulator -> raw_exception_pattern -> 'accumulator = fun accu -> function
  | EPData (_aexccon1, _avaros0) ->
      self#epdata accu (_aexccon1, _avaros0)
  | EPDisjunction (_exception_patterns0) ->
      self#epdisjunction accu (_exception_patterns0)

  method epdata : 'accumulator -> aexccon * avaro list -> 'accumulator = fun accu -> 
  function (_aexccon1, _avaros0) -> 
      let accu = (self#aexccon) accu _aexccon1 in
      let accu = List.fold_left (self#avaro) accu _avaros0 in
      accu

  method epdisjunction : 'accumulator -> exception_pattern list -> 'accumulator = fun accu -> 
  function (_exception_patterns0) -> 
      let accu = List.fold_left (self#exception_pattern) accu _exception_patterns0 in
      accu

  method exception_pattern : 'accumulator -> exception_pattern -> 'accumulator = fun accu -> function
  (_raw_exception_patterns0) ->
      let accu = Annotation.fold (self#raw_exception_pattern) accu _raw_exception_patterns0 in
      accu

  method time : 'accumulator -> time -> 'accumulator = fun accu -> function
  | CompileTime ->
      self#compiletime accu
  | RunTime ->
      self#runtime accu

  method compiletime : 'accumulator -> 'accumulator = fun accu ->       accu

  method runtime : 'accumulator -> 'accumulator = fun accu ->       accu

  method raw_expression : 'accumulator -> raw_expression -> 'accumulator = fun accu -> function
  | EVar (_var0) ->
      self#evar accu (_var0)
  | EBool (_x0) ->
      self#ebool accu (_x0)
  | EData (_datacon1, _expressions0) ->
      self#edata accu (_datacon1, _expressions0)
  | EFresh (_fresh_binding0) ->
      self#efresh accu (_fresh_binding0)
  | ECase (_expression1, _branchs0) ->
      self#ecase accu (_expression1, _branchs0)
  | ECall (_func1, _expression0) ->
      self#ecall accu (_func1, _expression0)
  | ETuple (_expressions0) ->
      self#etuple accu (_expressions0)
  | ERaise (_exccon1, _expressions0) ->
      self#eraise accu (_exccon1, _expressions0)
  | ELetWhereUnless (_expression2, _handlers1, _letw_binding0) ->
      self#eletwhereunless accu (_expression2, _handlers1, _letw_binding0)
  | ETryUnless (_expression1, _handlers0) ->
      self#etryunless accu (_expression1, _handlers0)
  | EAssert (_time2, _contrainte1, _expression0) ->
      self#eassert accu (_time2, _contrainte1, _expression0)
  | EAssertFalse (_time0) ->
      self#eassertfalse accu (_time0)

  method evar : 'accumulator -> var -> 'accumulator = fun accu -> 
  function (_var0) -> 
      accu

  method ebool : 'accumulator -> ( bool ) -> 'accumulator = fun accu -> 
  function (_x0) -> 
      accu

  method edata : 'accumulator -> datacon * expression list -> 'accumulator = fun accu -> 
  function (_datacon1, _expressions0) -> 
      let accu = List.fold_left (self#expression) accu _expressions0 in
      accu

  method efresh : 'accumulator -> opaque_fresh_binding -> 'accumulator = fun accu -> 
  function (_fresh_binding0) -> 
      let accu = self#fresh_binding accu (open_fresh_binding _fresh_binding0) in
      accu

  method ecase : 'accumulator -> expression * branch list -> 'accumulator = fun accu -> 
  function (_expression1, _branchs0) -> 
      let accu = (self#expression) accu _expression1 in
      let accu = List.fold_left (self#branch) accu _branchs0 in
      accu

  method ecall : 'accumulator -> func * expression -> 'accumulator = fun accu -> 
  function (_func1, _expression0) -> 
      let accu = (self#expression) accu _expression0 in
      accu

  method etuple : 'accumulator -> expression list -> 'accumulator = fun accu -> 
  function (_expressions0) -> 
      let accu = List.fold_left (self#expression) accu _expressions0 in
      accu

  method eraise : 'accumulator -> exccon * expression list -> 'accumulator = fun accu -> 
  function (_exccon1, _expressions0) -> 
      let accu = List.fold_left (self#expression) accu _expressions0 in
      accu

  method eletwhereunless : 'accumulator -> expression * handler list * opaque_letw_binding -> 'accumulator = fun accu -> 
  function (_expression2, _handlers1, _letw_binding0) -> 
      let accu = (self#expression) accu _expression2 in
      let accu = List.fold_left (self#handler) accu _handlers1 in
      let accu = self#letw_binding accu (open_letw_binding _letw_binding0) in
      accu

  method etryunless : 'accumulator -> expression * handler list -> 'accumulator = fun accu -> 
  function (_expression1, _handlers0) -> 
      let accu = (self#expression) accu _expression1 in
      let accu = List.fold_left (self#handler) accu _handlers0 in
      accu

  method eassert : 'accumulator -> time * contrainte * expression -> 'accumulator = fun accu -> 
  function (_time2, _contrainte1, _expression0) -> 
      let accu = (self#time) accu _time2 in
      let accu = (self#contrainte) accu _contrainte1 in
      let accu = (self#expression) accu _expression0 in
      accu

  method eassertfalse : 'accumulator -> time -> 'accumulator = fun accu -> 
  function (_time0) -> 
      let accu = (self#time) accu _time0 in
      accu

  method expression : 'accumulator -> expression -> 'accumulator = fun accu -> function
  (_raw_expressions0) ->
      let accu = Annotation.fold (self#raw_expression) accu _raw_expressions0 in
      accu

  method fresh_binding : 'accumulator -> fresh_binding -> 'accumulator = fun accu -> function
  (_asorte2, _avars1, _expression0) ->
      let accu = (self#asorte) accu _asorte2 in
      let accu = List.fold_left (self#avar) accu _avars1 in
      let accu = (self#expression) accu _expression0 in
      accu

  method letw_binding : 'accumulator -> letw_binding -> 'accumulator = fun accu -> function
  (_avaros2, _contrainte1, _expression0) ->
      let accu = List.fold_left (self#avaro) accu _avaros2 in
      let accu = (self#contrainte) accu _contrainte1 in
      let accu = (self#expression) accu _expression0 in
      accu

  method pat_binding : 'accumulator -> pat_binding -> 'accumulator = fun accu -> function
  (_pattern1, _expression0) ->
      let accu = (self#pattern) accu _pattern1 in
      let accu = (self#expression) accu _expression0 in
      accu

  method branch : 'accumulator -> branch -> 'accumulator = fun accu -> function
  (_pat_binding0) ->
      let accu = self#pat_binding accu (open_pat_binding _pat_binding0) in
      accu

  method exc_pat_binding : 'accumulator -> exc_pat_binding -> 'accumulator = fun accu -> function
  (_exception_pattern1, _expression0) ->
      let accu = (self#exception_pattern) accu _exception_pattern1 in
      let accu = (self#expression) accu _expression0 in
      accu

  method handler : 'accumulator -> handler -> 'accumulator = fun accu -> function
  (_exc_pat_binding0) ->
      let accu = self#exc_pat_binding accu (open_exc_pat_binding _exc_pat_binding0) in
      accu

  method normal_postcondition : 'accumulator -> normal_postcondition -> 'accumulator = fun accu -> function
  (_guarded_tuple0) ->
      let accu = self#guarded_tuple accu (open_guarded_tuple _guarded_tuple0) in
      accu

  method exceptional_postcondition : 'accumulator -> exceptional_postcondition -> 'accumulator = fun accu -> function
  (_exccon1, _guarded_params0) ->
      let accu = self#guarded_params accu (open_guarded_params _guarded_params0) in
      accu

  method postcondition : 'accumulator -> postcondition -> 'accumulator = fun accu -> function
  { post_normal = _normal_postcondition2; post_exceptional = _exceptional_postconditions1; post_other_exceptions = _x0 } ->
      let accu = self#post_normal accu _normal_postcondition2 in
      let accu = self#post_exceptional accu _exceptional_postconditions1 in
      let accu = self#post_other_exceptions accu _x0 in
      accu

  method post_normal : 'accumulator -> normal_postcondition -> 'accumulator = fun accu -> 
  function _normal_postcondition0 ->
      let accu = (self#normal_postcondition) accu _normal_postcondition0 in
      accu

  method post_exceptional : 'accumulator -> exceptional_postcondition list -> 'accumulator = fun accu -> 
  function _exceptional_postconditions0 ->
      let accu = List.fold_left (self#exceptional_postcondition) accu _exceptional_postconditions0 in
      accu

  method post_other_exceptions : 'accumulator -> ( bool ) -> 'accumulator = fun accu -> 
  function _x0 ->
      accu

  method fundef : 'accumulator -> fundef -> 'accumulator = fun accu -> function
  { fun_params = _tuple3; fun_precondition = _contrainte2; fun_postcondition = _postcondition1; fun_body = _expression0 } ->
      let accu = self#fun_params accu _tuple3 in
      let accu = self#fun_precondition accu _contrainte2 in
      let accu = self#fun_postcondition accu _postcondition1 in
      let accu = self#fun_body accu _expression0 in
      accu

  method fun_params : 'accumulator -> tuple -> 'accumulator = fun accu -> 
  function _tuple0 ->
      let accu = (self#tuple) accu _tuple0 in
      accu

  method fun_precondition : 'accumulator -> contrainte -> 'accumulator = fun accu -> 
  function _contrainte0 ->
      let accu = (self#contrainte) accu _contrainte0 in
      accu

  method fun_postcondition : 'accumulator -> postcondition -> 'accumulator = fun accu -> 
  function _postcondition0 ->
      let accu = (self#postcondition) accu _postcondition0 in
      accu

  method fun_body : 'accumulator -> expression -> 'accumulator = fun accu -> 
  function _expression0 ->
      let accu = (self#expression) accu _expression0 in
      accu

  method fundef1 : 'accumulator -> fundef1 -> 'accumulator = fun accu -> function
  (_fundef0) ->
      let accu = self#fundef accu (open_fundef _fundef0) in
      accu

  method def : 'accumulator -> def -> 'accumulator = fun accu -> function
  | DefSort (_sorte0) ->
      self#defsort accu (_sorte0)
  | DefDataType (_datatypedef0) ->
      self#defdatatype accu (_datatypedef0)
  | DefLemma (_lemmadef0) ->
      self#deflemma accu (_lemmadef0)
  | DefException (_excdef0) ->
      self#defexception accu (_excdef0)
  | DefFun (_func1, _fundef10) ->
      self#deffun accu (_func1, _fundef10)
  | DefBuiltin (_func0) ->
      self#defbuiltin accu (_func0)

  method defsort : 'accumulator -> sorte -> 'accumulator = fun accu -> 
  function (_sorte0) -> 
      accu

  method defdatatype : 'accumulator -> datatypedef -> 'accumulator = fun accu -> 
  function (_datatypedef0) -> 
      let accu = (self#datatypedef) accu _datatypedef0 in
      accu

  method deflemma : 'accumulator -> lemmadef -> 'accumulator = fun accu -> 
  function (_lemmadef0) -> 
      let accu = (self#lemmadef) accu _lemmadef0 in
      accu

  method defexception : 'accumulator -> excdef -> 'accumulator = fun accu -> 
  function (_excdef0) -> 
      let accu = (self#excdef) accu _excdef0 in
      accu

  method deffun : 'accumulator -> func * fundef1 -> 'accumulator = fun accu -> 
  function (_func1, _fundef10) -> 
      let accu = (self#fundef1) accu _fundef10 in
      accu

  method defbuiltin : 'accumulator -> func -> 'accumulator = fun accu -> 
  function (_func0) -> 
      accu

  method defs : 'accumulator -> defs -> 'accumulator = fun accu -> function
  (_defs0) ->
      let accu = List.fold_left (self#def) accu _defs0 in
      accu

  method program : 'accumulator -> program -> 'accumulator = fun accu -> function
  (_defs0) ->
      let accu = self#defs accu (open_defs _defs0) in
      accu

end
