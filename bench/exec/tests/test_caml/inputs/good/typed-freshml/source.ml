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

module Valfun = AlphaLib.Atom.Make(Identifier)

module Var = AlphaLib.Atom.Make(Identifier)

module Raw = struct

type var =
  Identifier.t

 and datacon =
  Identifier.t

 and datatype =
  Identifier.t

 and valfun =
  Identifier.t

 and typ = 
  | TData of datatype
  | TAtom
  | TAtomSet
  | TBool

 and layout = 
  | LComponent of mono option * typ
  | LInner of llayout
  | LOuter of llayout
  | LAbstraction of llayout
  | LTuple of llayout list

 and llayout = 
  layout Location.t

 and set_function = 
  | SFSupport
  | SFOuter
  | SFInner
  | SFBound

 and set_entity = 
  set_function * var

 and setsetset_operator = 
  | OpUnion
  | OpIntersection
  | OpDifference

 and set_expression = 
  | SEEmpty
  | SEApp of set_entity
  | SEAssocOp of setsetset_operator * set_expression list
  | SEConditional of contrainte * set_expression * set_expression

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

 and contrainte = 
  | FTrue
  | FFalse
  | FBoolVar of var
  | FNot of contrainte
  | FBoolAssocOp of boolboolbool_operator * contrainte list
  | FSetBinOp of set_expression * setsetbool_operator * set_expression

 and tag = 
  datacon

 and raw_pattern = 
  | PWildcard
  | PVar of var
  | PBool of ( bool )
  | PTagTuple of tag * pattern list

 and pattern = 
  raw_pattern Location.t

 and lexp = 
  expression Location.t

 and assertion = 
  | StaticAssertion
  | DynamicAssertion

 and expression = 
  | EVar of var
  | EBool of ( bool )
  | ETagTuple of tag * lexp list
  | EFresh of ofr
  | ECase of lexp * branch list
  | ECall of callee * lexp
  | EMulti of lexp list
  | ELetMulti of lexp * olm
  | EAssert of assertion * contrainte * lexp
  | EAbsurd
  | EFail
  | ENextCase

 and callee = 
  | CUser of valfun
  | CPrim of primitive

 and primitive = 
  | PrimBoolAnd
  | PrimBoolOr
  | PrimBoolNot
  | PrimAtomEquality
  | PrimGenericSupport
  | PrimGenericOuter
  | PrimGenericInner
  | PrimGenericBound
  | PrimSetEmpty
  | PrimSetMember
  | PrimSetAdd
  | PrimSetUnion
  | PrimSetInter
  | PrimSetMinus
  | PrimSetIsEmpty
  | PrimSetChoose
  | PrimSingletonRename

 and mono = 
  var

 and branch = 
  obr

 and def = 
  | DefValFun of valfun * fundef
  | DefDataType of datatype * datatypedef

 and fundef = 
  ofu

 and specification = 
  osp

 and postcondition = 
  opc

 and datatypedef = 
  kind * datacondef list

 and kind = 
  | KExpression
  | KPattern

 and datacondef = 
  datacon * datacondetails

 and datacondetails = 
  dcd

 and defs = 
  def list

 and program = 
  defs

 and ofr = 
  mono list * lexp

 and olm = 
  mono list * contrainte * lexp

 and obr = 
  pattern list * lexp

 and ofu = 
  mono list * contrainte * postcondition * lexp

 and osp = 
  mono list * contrainte * postcondition

 and opc = 
  mono list * contrainte

 and dcd = 
  llayout * contrainte

end

module Flat = struct

type var =
  Var.Atom.t

 and datacon =
  Datacon.Atom.t

 and datatype =
  Datatype.Atom.t

 and valfun =
  Valfun.Atom.t

 and typ = 
  | TData of datatype
  | TAtom
  | TAtomSet
  | TBool

 and layout = 
  | LComponent of mono option * typ
  | LInner of llayout
  | LOuter of llayout
  | LAbstraction of llayout
  | LTuple of llayout list

 and llayout = 
  layout Location.t

 and set_function = 
  | SFSupport
  | SFOuter
  | SFInner
  | SFBound

 and set_entity = 
  set_function * var

 and setsetset_operator = 
  | OpUnion
  | OpIntersection
  | OpDifference

 and set_expression = 
  | SEEmpty
  | SEApp of set_entity
  | SEAssocOp of setsetset_operator * set_expression list
  | SEConditional of contrainte * set_expression * set_expression

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

 and contrainte = 
  | FTrue
  | FFalse
  | FBoolVar of var
  | FNot of contrainte
  | FBoolAssocOp of boolboolbool_operator * contrainte list
  | FSetBinOp of set_expression * setsetbool_operator * set_expression

 and tag = 
  datacon

 and raw_pattern = 
  | PWildcard
  | PVar of var
  | PBool of ( bool )
  | PTagTuple of tag * pattern list

 and pattern = 
  raw_pattern Location.t

 and lexp = 
  expression Location.t

 and assertion = 
  | StaticAssertion
  | DynamicAssertion

 and expression = 
  | EVar of var
  | EBool of ( bool )
  | ETagTuple of tag * lexp list
  | EFresh of ofr
  | ECase of lexp * branch list
  | ECall of callee * lexp
  | EMulti of lexp list
  | ELetMulti of lexp * olm
  | EAssert of assertion * contrainte * lexp
  | EAbsurd
  | EFail
  | ENextCase

 and callee = 
  | CUser of valfun
  | CPrim of primitive

 and primitive = 
  | PrimBoolAnd
  | PrimBoolOr
  | PrimBoolNot
  | PrimAtomEquality
  | PrimGenericSupport
  | PrimGenericOuter
  | PrimGenericInner
  | PrimGenericBound
  | PrimSetEmpty
  | PrimSetMember
  | PrimSetAdd
  | PrimSetUnion
  | PrimSetInter
  | PrimSetMinus
  | PrimSetIsEmpty
  | PrimSetChoose
  | PrimSingletonRename

 and mono = 
  var

 and branch = 
  obr

 and def = 
  | DefValFun of valfun * fundef
  | DefDataType of datatype * datatypedef

 and fundef = 
  ofu

 and specification = 
  osp

 and postcondition = 
  opc

 and datatypedef = 
  kind * datacondef list

 and kind = 
  | KExpression
  | KPattern

 and datacondef = 
  datacon * datacondetails

 and datacondetails = 
  dcd

 and defs = 
  def list

 and program = 
  defs

 and ofr = 
  mono list * lexp

 and olm = 
  mono list * contrainte * lexp

 and obr = 
  pattern list * lexp

 and ofu = 
  mono list * contrainte * postcondition * lexp

 and osp = 
  mono list * contrainte * postcondition

 and opc = 
  mono list * contrainte

 and dcd = 
  llayout * contrainte

end

type var =
  Var.Atom.t

 and datacon =
  Datacon.Atom.t

 and datatype =
  Datatype.Atom.t

 and valfun =
  Valfun.Atom.t

 and typ = 
  | TData of datatype
  | TAtom
  | TAtomSet
  | TBool

 and layout = 
  | LComponent of mono option * typ
  | LInner of llayout
  | LOuter of llayout
  | LAbstraction of llayout
  | LTuple of llayout list

 and llayout = 
  layout Location.t

 and set_function = 
  | SFSupport
  | SFOuter
  | SFInner
  | SFBound

 and set_entity = 
  set_function * var

 and setsetset_operator = 
  | OpUnion
  | OpIntersection
  | OpDifference

 and set_expression = 
  | SEEmpty
  | SEApp of set_entity
  | SEAssocOp of setsetset_operator * set_expression list
  | SEConditional of contrainte * set_expression * set_expression

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

 and contrainte = 
  | FTrue
  | FFalse
  | FBoolVar of var
  | FNot of contrainte
  | FBoolAssocOp of boolboolbool_operator * contrainte list
  | FSetBinOp of set_expression * setsetbool_operator * set_expression

 and tag = 
  datacon

 and raw_pattern = 
  | PWildcard
  | PVar of var
  | PBool of ( bool )
  | PTagTuple of tag * pattern list

 and pattern = 
  raw_pattern Location.t

 and lexp = 
  expression Location.t

 and assertion = 
  | StaticAssertion
  | DynamicAssertion

 and expression = 
  | EVar of var
  | EBool of ( bool )
  | ETagTuple of tag * lexp list
  | EFresh of opaque_ofr
  | ECase of lexp * branch list
  | ECall of callee * lexp
  | EMulti of lexp list
  | ELetMulti of lexp * opaque_olm
  | EAssert of assertion * contrainte * lexp
  | EAbsurd
  | EFail
  | ENextCase

 and callee = 
  | CUser of valfun
  | CPrim of primitive

 and primitive = 
  | PrimBoolAnd
  | PrimBoolOr
  | PrimBoolNot
  | PrimAtomEquality
  | PrimGenericSupport
  | PrimGenericOuter
  | PrimGenericInner
  | PrimGenericBound
  | PrimSetEmpty
  | PrimSetMember
  | PrimSetAdd
  | PrimSetUnion
  | PrimSetInter
  | PrimSetMinus
  | PrimSetIsEmpty
  | PrimSetChoose
  | PrimSingletonRename

 and mono = 
  var

 and branch = 
  opaque_obr

 and def = 
  | DefValFun of valfun * fundef
  | DefDataType of datatype * datatypedef

 and fundef = 
  opaque_ofu

 and specification = 
  opaque_osp

 and postcondition = 
  opaque_opc

 and datatypedef = 
  kind * datacondef list

 and kind = 
  | KExpression
  | KPattern

 and datacondef = 
  datacon * datacondetails

 and datacondetails = 
  opaque_dcd

 and defs = 
  def list

 and opaque_defs = {
    mutable defs_delayed: Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t * Var.Subst.t;
    mutable defs: defs
  }

 and program = 
  opaque_defs

 and ofr = 
  mono list * lexp

 and opaque_ofr = {
    mutable ofr_delayed: Datacon.Subst.t * Valfun.Subst.t * Var.Subst.t;
    mutable ofr: ofr
  }

 and olm = 
  mono list * contrainte * lexp

 and opaque_olm = {
    mutable olm_delayed: Datacon.Subst.t * Valfun.Subst.t * Var.Subst.t;
    mutable olm: olm
  }

 and obr = 
  pattern list * lexp

 and opaque_obr = {
    mutable obr_delayed: Datacon.Subst.t * Valfun.Subst.t * Var.Subst.t;
    mutable obr: obr
  }

 and ofu = 
  mono list * contrainte * postcondition * lexp

 and opaque_ofu = {
    mutable ofu_delayed: Datacon.Subst.t * Valfun.Subst.t * Var.Subst.t;
    mutable ofu: ofu
  }

 and osp = 
  mono list * contrainte * postcondition

 and opaque_osp = {
    mutable osp_delayed: Var.Subst.t;
    mutable osp: osp
  }

 and opc = 
  mono list * contrainte

 and opaque_opc = {
    mutable opc_delayed: Var.Subst.t;
    mutable opc: opc
  }

 and dcd = 
  llayout * contrainte

 and opaque_dcd = {
    mutable dcd_delayed: Datatype.Subst.t * Var.Subst.t;
    mutable dcd: dcd
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

and import_typ : datatype Identifier.Map.t -> Raw.typ -> typ = fun (datatype_env) -> function
  | Raw.TData (_datatype0) ->
      TData (Datatype.find _datatype0 datatype_env)
  | Raw.TAtom ->
      TAtom
  | Raw.TAtomSet ->
      TAtomSet
  | Raw.TBool ->
      TBool

and subst_typ : Datatype.Subst.t -> typ -> typ = fun (datatype_env) -> function
  | TData (_datatype0) ->
      TData (Datatype.Subst.lookup _datatype0 datatype_env)
  | TAtom ->
      TAtom
  | TAtomSet ->
      TAtomSet
  | TBool ->
      TBool

and export_typ : Datatype.AtomIdMap.t -> typ -> Raw.typ = fun (datatype_m) -> function
  | TData (_datatype0) ->
      Raw.TData (Datatype.AtomIdMap.lookup _datatype0 datatype_m)
  | TAtom ->
      Raw.TAtom
  | TAtomSet ->
      Raw.TAtomSet
  | TBool ->
      Raw.TBool

and flatten_typ : typ -> Flat.typ = function
  | TData (_datatype0) ->
      Flat.TData (_datatype0)
  | TAtom ->
      Flat.TAtom
  | TAtomSet ->
      Flat.TAtomSet
  | TBool ->
      Flat.TBool

and unflatten_typ : Flat.typ -> typ = function
  | Flat.TData (_datatype0) ->
      TData (_datatype0)
  | Flat.TAtom ->
      TAtom
  | Flat.TAtomSet ->
      TAtomSet
  | Flat.TBool ->
      TBool

and free_typ : typ -> Datatype.AtomSet.t = 
  function typ -> free_accu_typ (Datatype.AtomSet.empty) typ

and equal_typ : typ -> typ -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_typ x1 x2

and aeq_typ : unit -> typ -> typ -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | TData (_datatype0), TData (_datatype1) ->
      if not (Datatype.Atom.equal _datatype0 _datatype1) then raise (Invalid_argument "aeq_typ");
      ()
  | TAtom, TAtom ->
      ()
  | TAtomSet, TAtomSet ->
      ()
  | TBool, TBool ->
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_typ")

and free_accu_typ = fun (datatype_fvars) -> function
  | TData (_datatype0) ->
      let datatype_fvars = Datatype.AtomSet.add _datatype0 datatype_fvars in
      (datatype_fvars)
  | TAtom ->
      (datatype_fvars)
  | TAtomSet ->
      (datatype_fvars)
  | TBool ->
      (datatype_fvars)

and subst_layout : Datatype.Subst.t * Var.Subst.t -> layout -> layout = fun (datatype_oenv, var_ienv) -> function
  | LComponent (_monos1, _typ0) ->
      LComponent (option_map (subst_mono (var_ienv)) _monos1, (subst_typ (datatype_oenv)) _typ0)
  | LInner (_llayout0) ->
      LInner ((subst_llayout (datatype_oenv, var_ienv)) _llayout0)
  | LOuter (_llayout0) ->
      LOuter ((subst_llayout (datatype_oenv, var_ienv)) _llayout0)
  | LAbstraction (_llayout0) ->
      LAbstraction ((subst_llayout (datatype_oenv, var_ienv)) _llayout0)
  | LTuple (_llayouts0) ->
      LTuple (List.map (subst_llayout (datatype_oenv, var_ienv)) _llayouts0)

and bound_layout : layout -> Var.AtomSet.t = 
  function layout -> bound_accu_layout (Var.AtomSet.empty) layout

and bound_free_layout : layout -> Var.AtomSet.t * Datatype.AtomSet.t = 
  function layout -> bound_free_accu_layout (Var.AtomSet.empty, Datatype.AtomSet.empty) layout

and equal_layout : layout -> layout -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_layout x1 x2

and import_layout = fun (datatype_oenv, var_ienv) -> function
  | Raw.LComponent (_monos1, _typ0) ->
      LComponent (option_map (import_mono (var_ienv)) _monos1, (import_typ (datatype_oenv)) _typ0)
  | Raw.LInner (_llayout0) ->
      LInner ((import_llayout (datatype_oenv, var_ienv)) _llayout0)
  | Raw.LOuter (_llayout0) ->
      LOuter ((import_llayout (datatype_oenv, var_ienv)) _llayout0)
  | Raw.LAbstraction (_llayout0) ->
      LAbstraction ((import_llayout (datatype_oenv, var_ienv)) _llayout0)
  | Raw.LTuple (_llayouts0) ->
      LTuple (List.map (import_llayout (datatype_oenv, var_ienv)) _llayouts0)

and bvi_accu_layout = fun (var_bvars) -> function
  | Raw.LComponent (_monos1, _typ0) ->
      let (var_bvars) = option_fold bvi_accu_mono (var_bvars) _monos1 in
      (var_bvars)
  | Raw.LInner (_llayout0) ->
      let (var_bvars) = bvi_accu_llayout (var_bvars) _llayout0 in
      (var_bvars)
  | Raw.LOuter (_llayout0) ->
      let (var_bvars) = bvi_accu_llayout (var_bvars) _llayout0 in
      (var_bvars)
  | Raw.LAbstraction (_llayout0) ->
      let (var_bvars) = bvi_accu_llayout (var_bvars) _llayout0 in
      (var_bvars)
  | Raw.LTuple (_llayouts0) ->
      let (var_bvars) = List.fold_left bvi_accu_llayout (var_bvars) _llayouts0 in
      (var_bvars)

and bvi_layout = 
  function layout -> bvi_accu_layout (Identifier.Map.empty) layout

and bound_accu_layout = fun (var_bvars) -> function
  | LComponent (_monos1, _typ0) ->
      let (var_bvars) = option_fold bound_accu_mono (var_bvars) _monos1 in
      (var_bvars)
  | LInner (_llayout0) ->
      let (var_bvars) = bound_accu_llayout (var_bvars) _llayout0 in
      (var_bvars)
  | LOuter (_llayout0) ->
      let (var_bvars) = bound_accu_llayout (var_bvars) _llayout0 in
      (var_bvars)
  | LAbstraction (_llayout0) ->
      let (var_bvars) = bound_accu_llayout (var_bvars) _llayout0 in
      (var_bvars)
  | LTuple (_llayouts0) ->
      let (var_bvars) = List.fold_left bound_accu_llayout (var_bvars) _llayouts0 in
      (var_bvars)

and export_layout : Datatype.AtomIdMap.t * Var.AtomIdMap.t -> layout -> Raw.layout = fun (datatype_om, var_im) -> function
  | LComponent (_monos1, _typ0) ->
      Raw.LComponent (option_map (export_mono (var_im)) _monos1, (export_typ (datatype_om)) _typ0)
  | LInner (_llayout0) ->
      Raw.LInner ((export_llayout (datatype_om, var_im)) _llayout0)
  | LOuter (_llayout0) ->
      Raw.LOuter ((export_llayout (datatype_om, var_im)) _llayout0)
  | LAbstraction (_llayout0) ->
      Raw.LAbstraction ((export_llayout (datatype_om, var_im)) _llayout0)
  | LTuple (_llayouts0) ->
      Raw.LTuple (List.map (export_llayout (datatype_om, var_im)) _llayouts0)

and flatten_layout : layout -> Flat.layout = function
  | LComponent (_monos1, _typ0) ->
      Flat.LComponent (option_map flatten_mono _monos1, flatten_typ _typ0)
  | LInner (_llayout0) ->
      Flat.LInner (flatten_llayout _llayout0)
  | LOuter (_llayout0) ->
      Flat.LOuter (flatten_llayout _llayout0)
  | LAbstraction (_llayout0) ->
      Flat.LAbstraction (flatten_llayout _llayout0)
  | LTuple (_llayouts0) ->
      Flat.LTuple (List.map flatten_llayout _llayouts0)

and unflatten_layout : Flat.layout -> layout = function
  | Flat.LComponent (_monos1, _typ0) ->
      LComponent (option_map unflatten_mono _monos1, unflatten_typ _typ0)
  | Flat.LInner (_llayout0) ->
      LInner (unflatten_llayout _llayout0)
  | Flat.LOuter (_llayout0) ->
      LOuter (unflatten_llayout _llayout0)
  | Flat.LAbstraction (_llayout0) ->
      LAbstraction (unflatten_llayout _llayout0)
  | Flat.LTuple (_llayouts0) ->
      LTuple (List.map unflatten_llayout _llayouts0)

and bound_free_accu_layout = fun (var_bvars, datatype_ofvars) -> function
  | LComponent (_monos1, _typ0) ->
      let (var_bvars) = option_fold bound_free_accu_mono (var_bvars) _monos1 in
      let (datatype_ofvars) = free_accu_typ (datatype_ofvars) _typ0 in
      (var_bvars, datatype_ofvars)
  | LInner (_llayout0) ->
      let (var_bvars, datatype_ofvars) = bound_free_accu_llayout (var_bvars, datatype_ofvars) _llayout0 in
      (var_bvars, datatype_ofvars)
  | LOuter (_llayout0) ->
      let (var_bvars, datatype_ofvars) = bound_free_accu_llayout (var_bvars, datatype_ofvars) _llayout0 in
      (var_bvars, datatype_ofvars)
  | LAbstraction (_llayout0) ->
      let (var_bvars, datatype_ofvars) = bound_free_accu_llayout (var_bvars, datatype_ofvars) _llayout0 in
      (var_bvars, datatype_ofvars)
  | LTuple (_llayouts0) ->
      let (var_bvars, datatype_ofvars) = List.fold_left bound_free_accu_llayout (var_bvars, datatype_ofvars) _llayouts0 in
      (var_bvars, datatype_ofvars)

and aeq_layout : unit -> layout -> layout -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | LComponent (_monos1, _typ0), LComponent (_monos3, _typ2) ->
      option_fold2 aeq_mono () _monos1 _monos3;
      aeq_typ () _typ0 _typ2;
      ()
  | LInner (_llayout0), LInner (_llayout1) ->
      aeq_llayout () _llayout0 _llayout1;
      ()
  | LOuter (_llayout0), LOuter (_llayout1) ->
      aeq_llayout () _llayout0 _llayout1;
      ()
  | LAbstraction (_llayout0), LAbstraction (_llayout1) ->
      aeq_llayout () _llayout0 _llayout1;
      ()
  | LTuple (_llayouts0), LTuple (_llayouts1) ->
      List.fold_left2 aeq_llayout () _llayouts0 _llayouts1;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_layout")

and freshen2_layout : Var.Subst.t * Var.Subst.t -> layout -> layout -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | LComponent (_monos1, _typ0), LComponent (_monos3, _typ2) ->
      let (var_env1, var_env2) = option_fold2 freshen2_mono (var_env1, var_env2) _monos1 _monos3 in
      (var_env1, var_env2)
  | LInner (_llayout0), LInner (_llayout1) ->
      let (var_env1, var_env2) = freshen2_llayout (var_env1, var_env2) _llayout0 _llayout1 in
      (var_env1, var_env2)
  | LOuter (_llayout0), LOuter (_llayout1) ->
      let (var_env1, var_env2) = freshen2_llayout (var_env1, var_env2) _llayout0 _llayout1 in
      (var_env1, var_env2)
  | LAbstraction (_llayout0), LAbstraction (_llayout1) ->
      let (var_env1, var_env2) = freshen2_llayout (var_env1, var_env2) _llayout0 _llayout1 in
      (var_env1, var_env2)
  | LTuple (_llayouts0), LTuple (_llayouts1) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_llayout (var_env1, var_env2) _llayouts0 _llayouts1 in
      (var_env1, var_env2)
  | _, _ ->
      raise (Invalid_argument "freshen2_layout")

and subst_llayout : Datatype.Subst.t * Var.Subst.t -> llayout -> llayout = fun (datatype_oenv, var_ienv) -> function
  (_layouts0) ->
    (Annotation.map (subst_layout (datatype_oenv, var_ienv)) _layouts0)

and bound_llayout : llayout -> Var.AtomSet.t = 
  function llayout -> bound_accu_llayout (Var.AtomSet.empty) llayout

and bound_free_llayout : llayout -> Var.AtomSet.t * Datatype.AtomSet.t = 
  function llayout -> bound_free_accu_llayout (Var.AtomSet.empty, Datatype.AtomSet.empty) llayout

and equal_llayout : llayout -> llayout -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_llayout x1 x2

and import_llayout = fun (datatype_oenv, var_ienv) -> function
  (_layouts0) ->
    (Annotation.map (import_layout (datatype_oenv, var_ienv)) _layouts0)

and bvi_accu_llayout = fun (var_bvars) -> function
  (_layouts0) ->
      let (var_bvars) = Annotation.fold bvi_accu_layout (var_bvars) _layouts0 in
      (var_bvars)

and bvi_llayout = 
  function llayout -> bvi_accu_llayout (Identifier.Map.empty) llayout

and bound_accu_llayout = fun (var_bvars) -> function
  (_layouts0) ->
      let (var_bvars) = Annotation.fold bound_accu_layout (var_bvars) _layouts0 in
      (var_bvars)

and export_llayout : Datatype.AtomIdMap.t * Var.AtomIdMap.t -> llayout -> Raw.llayout = fun (datatype_om, var_im) -> function
  (_layouts0) ->
    (Annotation.map (export_layout (datatype_om, var_im)) _layouts0)

and flatten_llayout : llayout -> Flat.llayout = function
  (_layouts0) ->
    (Annotation.map flatten_layout _layouts0)

and unflatten_llayout : Flat.llayout -> llayout = function
  (_layouts0) ->
    (Annotation.map unflatten_layout _layouts0)

and bound_free_accu_llayout = fun (var_bvars, datatype_ofvars) -> function
  (_layouts0) ->
      let (var_bvars, datatype_ofvars) = Annotation.fold bound_free_accu_layout (var_bvars, datatype_ofvars) _layouts0 in
      (var_bvars, datatype_ofvars)

and aeq_llayout : unit -> llayout -> llayout -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_layouts0), (_layouts1) ->
      Annotation.fold2 aeq_layout () _layouts0 _layouts1;
      ()

and freshen2_llayout : Var.Subst.t * Var.Subst.t -> llayout -> llayout -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_layouts0), (_layouts1) ->
      let (var_env1, var_env2) = Annotation.fold2 freshen2_layout (var_env1, var_env2) _layouts0 _layouts1 in
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

and import_set_entity : var Identifier.Map.t -> Raw.set_entity -> set_entity = fun (var_env) -> function
  (_set_function1, _var0) ->
    ((import_set_function ()) _set_function1, Var.find _var0 var_env)

and subst_set_entity : Var.Subst.t -> set_entity -> set_entity = fun (var_env) -> function
  (_set_function1, _var0) ->
    ((subst_set_function ()) _set_function1, Var.Subst.lookup _var0 var_env)

and export_set_entity : Var.AtomIdMap.t -> set_entity -> Raw.set_entity = fun (var_m) -> function
  (_set_function1, _var0) ->
    ((export_set_function ()) _set_function1, Var.AtomIdMap.lookup _var0 var_m)

and flatten_set_entity : set_entity -> Flat.set_entity = function
  (_set_function1, _var0) ->
    (flatten_set_function _set_function1, _var0)

and unflatten_set_entity : Flat.set_entity -> set_entity = function
  (_set_function1, _var0) ->
    (unflatten_set_function _set_function1, _var0)

and free_set_entity : set_entity -> Var.AtomSet.t = 
  function set_entity -> free_accu_set_entity (Var.AtomSet.empty) set_entity

and equal_set_entity : set_entity -> set_entity -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_set_entity x1 x2

and aeq_set_entity : unit -> set_entity -> set_entity -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_set_function1, _var0), (_set_function3, _var2) ->
      aeq_set_function () _set_function1 _set_function3;
      if not (Var.Atom.equal _var0 _var2) then raise (Invalid_argument "aeq_set_entity");
      ()

and free_accu_set_entity = fun (var_fvars) -> function
  (_set_function1, _var0) ->
      let () = free_accu_set_function () _set_function1 in 
      let var_fvars = Var.AtomSet.add _var0 var_fvars in
      (var_fvars)

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

and import_set_expression : var Identifier.Map.t -> Raw.set_expression -> set_expression = fun (var_env) -> function
  | Raw.SEEmpty ->
      SEEmpty
  | Raw.SEApp (_set_entity0) ->
      SEApp ((import_set_entity (var_env)) _set_entity0)
  | Raw.SEAssocOp (_setsetset_operator1, _set_expressions0) ->
      SEAssocOp ((import_setsetset_operator ()) _setsetset_operator1, List.map (import_set_expression (var_env)) _set_expressions0)
  | Raw.SEConditional (_contrainte2, _set_expression1, _set_expression0) ->
      SEConditional ((import_contrainte (var_env)) _contrainte2, (import_set_expression (var_env)) _set_expression1, (import_set_expression (var_env)) _set_expression0)

and subst_set_expression : Var.Subst.t -> set_expression -> set_expression = fun (var_env) -> function
  | SEEmpty ->
      SEEmpty
  | SEApp (_set_entity0) ->
      SEApp ((subst_set_entity (var_env)) _set_entity0)
  | SEAssocOp (_setsetset_operator1, _set_expressions0) ->
      SEAssocOp ((subst_setsetset_operator ()) _setsetset_operator1, List.map (subst_set_expression (var_env)) _set_expressions0)
  | SEConditional (_contrainte2, _set_expression1, _set_expression0) ->
      SEConditional ((subst_contrainte (var_env)) _contrainte2, (subst_set_expression (var_env)) _set_expression1, (subst_set_expression (var_env)) _set_expression0)

and export_set_expression : Var.AtomIdMap.t -> set_expression -> Raw.set_expression = fun (var_m) -> function
  | SEEmpty ->
      Raw.SEEmpty
  | SEApp (_set_entity0) ->
      Raw.SEApp ((export_set_entity (var_m)) _set_entity0)
  | SEAssocOp (_setsetset_operator1, _set_expressions0) ->
      Raw.SEAssocOp ((export_setsetset_operator ()) _setsetset_operator1, List.map (export_set_expression (var_m)) _set_expressions0)
  | SEConditional (_contrainte2, _set_expression1, _set_expression0) ->
      Raw.SEConditional ((export_contrainte (var_m)) _contrainte2, (export_set_expression (var_m)) _set_expression1, (export_set_expression (var_m)) _set_expression0)

and flatten_set_expression : set_expression -> Flat.set_expression = function
  | SEEmpty ->
      Flat.SEEmpty
  | SEApp (_set_entity0) ->
      Flat.SEApp (flatten_set_entity _set_entity0)
  | SEAssocOp (_setsetset_operator1, _set_expressions0) ->
      Flat.SEAssocOp (flatten_setsetset_operator _setsetset_operator1, List.map flatten_set_expression _set_expressions0)
  | SEConditional (_contrainte2, _set_expression1, _set_expression0) ->
      Flat.SEConditional (flatten_contrainte _contrainte2, flatten_set_expression _set_expression1, flatten_set_expression _set_expression0)

and unflatten_set_expression : Flat.set_expression -> set_expression = function
  | Flat.SEEmpty ->
      SEEmpty
  | Flat.SEApp (_set_entity0) ->
      SEApp (unflatten_set_entity _set_entity0)
  | Flat.SEAssocOp (_setsetset_operator1, _set_expressions0) ->
      SEAssocOp (unflatten_setsetset_operator _setsetset_operator1, List.map unflatten_set_expression _set_expressions0)
  | Flat.SEConditional (_contrainte2, _set_expression1, _set_expression0) ->
      SEConditional (unflatten_contrainte _contrainte2, unflatten_set_expression _set_expression1, unflatten_set_expression _set_expression0)

and free_set_expression : set_expression -> Var.AtomSet.t = 
  function set_expression -> free_accu_set_expression (Var.AtomSet.empty) set_expression

and equal_set_expression : set_expression -> set_expression -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_set_expression x1 x2

and aeq_set_expression : unit -> set_expression -> set_expression -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | SEEmpty, SEEmpty ->
      ()
  | SEApp (_set_entity0), SEApp (_set_entity1) ->
      aeq_set_entity () _set_entity0 _set_entity1;
      ()
  | SEAssocOp (_setsetset_operator1, _set_expressions0), SEAssocOp (_setsetset_operator3, _set_expressions2) ->
      aeq_setsetset_operator () _setsetset_operator1 _setsetset_operator3;
      List.fold_left2 aeq_set_expression () _set_expressions0 _set_expressions2;
      ()
  | SEConditional (_contrainte2, _set_expression1, _set_expression0), SEConditional (_contrainte5, _set_expression4, _set_expression3) ->
      aeq_contrainte () _contrainte2 _contrainte5;
      aeq_set_expression () _set_expression1 _set_expression4;
      aeq_set_expression () _set_expression0 _set_expression3;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_set_expression")

and free_accu_set_expression = fun (var_fvars) -> function
  | SEEmpty ->
      (var_fvars)
  | SEApp (_set_entity0) ->
      let (var_fvars) = free_accu_set_entity (var_fvars) _set_entity0 in 
      (var_fvars)
  | SEAssocOp (_setsetset_operator1, _set_expressions0) ->
      let () = free_accu_setsetset_operator () _setsetset_operator1 in 
      let (var_fvars) = List.fold_left free_accu_set_expression (var_fvars) _set_expressions0 in 
      (var_fvars)
  | SEConditional (_contrainte2, _set_expression1, _set_expression0) ->
      let (var_fvars) = free_accu_contrainte (var_fvars) _contrainte2 in 
      let (var_fvars) = free_accu_set_expression (var_fvars) _set_expression1 in 
      let (var_fvars) = free_accu_set_expression (var_fvars) _set_expression0 in 
      (var_fvars)

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

and import_contrainte : var Identifier.Map.t -> Raw.contrainte -> contrainte = fun (var_env) -> function
  | Raw.FTrue ->
      FTrue
  | Raw.FFalse ->
      FFalse
  | Raw.FBoolVar (_var0) ->
      FBoolVar (Var.find _var0 var_env)
  | Raw.FNot (_contrainte0) ->
      FNot ((import_contrainte (var_env)) _contrainte0)
  | Raw.FBoolAssocOp (_boolboolbool_operator1, _contraintes0) ->
      FBoolAssocOp ((import_boolboolbool_operator ()) _boolboolbool_operator1, List.map (import_contrainte (var_env)) _contraintes0)
  | Raw.FSetBinOp (_set_expression2, _setsetbool_operator1, _set_expression0) ->
      FSetBinOp ((import_set_expression (var_env)) _set_expression2, (import_setsetbool_operator ()) _setsetbool_operator1, (import_set_expression (var_env)) _set_expression0)

and subst_contrainte : Var.Subst.t -> contrainte -> contrainte = fun (var_env) -> function
  | FTrue ->
      FTrue
  | FFalse ->
      FFalse
  | FBoolVar (_var0) ->
      FBoolVar (Var.Subst.lookup _var0 var_env)
  | FNot (_contrainte0) ->
      FNot ((subst_contrainte (var_env)) _contrainte0)
  | FBoolAssocOp (_boolboolbool_operator1, _contraintes0) ->
      FBoolAssocOp ((subst_boolboolbool_operator ()) _boolboolbool_operator1, List.map (subst_contrainte (var_env)) _contraintes0)
  | FSetBinOp (_set_expression2, _setsetbool_operator1, _set_expression0) ->
      FSetBinOp ((subst_set_expression (var_env)) _set_expression2, (subst_setsetbool_operator ()) _setsetbool_operator1, (subst_set_expression (var_env)) _set_expression0)

and export_contrainte : Var.AtomIdMap.t -> contrainte -> Raw.contrainte = fun (var_m) -> function
  | FTrue ->
      Raw.FTrue
  | FFalse ->
      Raw.FFalse
  | FBoolVar (_var0) ->
      Raw.FBoolVar (Var.AtomIdMap.lookup _var0 var_m)
  | FNot (_contrainte0) ->
      Raw.FNot ((export_contrainte (var_m)) _contrainte0)
  | FBoolAssocOp (_boolboolbool_operator1, _contraintes0) ->
      Raw.FBoolAssocOp ((export_boolboolbool_operator ()) _boolboolbool_operator1, List.map (export_contrainte (var_m)) _contraintes0)
  | FSetBinOp (_set_expression2, _setsetbool_operator1, _set_expression0) ->
      Raw.FSetBinOp ((export_set_expression (var_m)) _set_expression2, (export_setsetbool_operator ()) _setsetbool_operator1, (export_set_expression (var_m)) _set_expression0)

and flatten_contrainte : contrainte -> Flat.contrainte = function
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

and unflatten_contrainte : Flat.contrainte -> contrainte = function
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

and free_contrainte : contrainte -> Var.AtomSet.t = 
  function contrainte -> free_accu_contrainte (Var.AtomSet.empty) contrainte

and equal_contrainte : contrainte -> contrainte -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_contrainte x1 x2

and aeq_contrainte : unit -> contrainte -> contrainte -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | FTrue, FTrue ->
      ()
  | FFalse, FFalse ->
      ()
  | FBoolVar (_var0), FBoolVar (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_contrainte");
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
      raise (Invalid_argument "aeq_contrainte")

and free_accu_contrainte = fun (var_fvars) -> function
  | FTrue ->
      (var_fvars)
  | FFalse ->
      (var_fvars)
  | FBoolVar (_var0) ->
      let var_fvars = Var.AtomSet.add _var0 var_fvars in
      (var_fvars)
  | FNot (_contrainte0) ->
      let (var_fvars) = free_accu_contrainte (var_fvars) _contrainte0 in 
      (var_fvars)
  | FBoolAssocOp (_boolboolbool_operator1, _contraintes0) ->
      let () = free_accu_boolboolbool_operator () _boolboolbool_operator1 in 
      let (var_fvars) = List.fold_left free_accu_contrainte (var_fvars) _contraintes0 in 
      (var_fvars)
  | FSetBinOp (_set_expression2, _setsetbool_operator1, _set_expression0) ->
      let (var_fvars) = free_accu_set_expression (var_fvars) _set_expression2 in 
      let () = free_accu_setsetbool_operator () _setsetbool_operator1 in 
      let (var_fvars) = free_accu_set_expression (var_fvars) _set_expression0 in 
      (var_fvars)

and import_tag : datacon Identifier.Map.t -> Raw.tag -> tag = fun (datacon_env) -> function
  (_datacon0) ->
    (Datacon.find _datacon0 datacon_env)

and subst_tag : Datacon.Subst.t -> tag -> tag = fun (datacon_env) -> function
  (_datacon0) ->
    (Datacon.Subst.lookup _datacon0 datacon_env)

and export_tag : Datacon.AtomIdMap.t -> tag -> Raw.tag = fun (datacon_m) -> function
  (_datacon0) ->
    (Datacon.AtomIdMap.lookup _datacon0 datacon_m)

and flatten_tag : tag -> Flat.tag = function
  (_datacon0) ->
    (_datacon0)

and unflatten_tag : Flat.tag -> tag = function
  (_datacon0) ->
    (_datacon0)

and free_tag : tag -> Datacon.AtomSet.t = 
  function tag -> free_accu_tag (Datacon.AtomSet.empty) tag

and equal_tag : tag -> tag -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_tag x1 x2

and aeq_tag : unit -> tag -> tag -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_datacon0), (_datacon1) ->
      if not (Datacon.Atom.equal _datacon0 _datacon1) then raise (Invalid_argument "aeq_tag");
      ()

and free_accu_tag = fun (datacon_fvars) -> function
  (_datacon0) ->
      let datacon_fvars = Datacon.AtomSet.add _datacon0 datacon_fvars in
      (datacon_fvars)

and subst_raw_pattern : Datacon.Subst.t * Var.Subst.t -> raw_pattern -> raw_pattern = fun (datacon_oenv, var_ienv) -> function
  | PWildcard ->
      PWildcard
  | PVar (_var0) ->
      PVar (Var.Subst.lookup _var0 var_ienv)
  | PBool (_x0) ->
      PBool (_x0)
  | PTagTuple (_tag1, _patterns0) ->
      PTagTuple ((subst_tag (datacon_oenv)) _tag1, List.map (subst_pattern (datacon_oenv, var_ienv)) _patterns0)

and bound_raw_pattern : raw_pattern -> Var.AtomSet.t = 
  function raw_pattern -> bound_accu_raw_pattern (Var.AtomSet.empty) raw_pattern

and bound_free_raw_pattern : raw_pattern -> Var.AtomSet.t * Datacon.AtomSet.t = 
  function raw_pattern -> bound_free_accu_raw_pattern (Var.AtomSet.empty, Datacon.AtomSet.empty) raw_pattern

and equal_raw_pattern : raw_pattern -> raw_pattern -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_raw_pattern x1 x2

and import_raw_pattern = fun (datacon_oenv, var_ienv) -> function
  | Raw.PWildcard ->
      PWildcard
  | Raw.PVar (_var0) ->
      PVar (Var.find _var0 var_ienv)
  | Raw.PBool (_x0) ->
      PBool (_x0)
  | Raw.PTagTuple (_tag1, _patterns0) ->
      PTagTuple ((import_tag (datacon_oenv)) _tag1, List.map (import_pattern (datacon_oenv, var_ienv)) _patterns0)

and bvi_accu_raw_pattern = fun (var_bvars) -> function
  | Raw.PWildcard ->
      (var_bvars)
  | Raw.PVar (_var0) ->
      let var_bvars = Identifier.Map.add _var0 () var_bvars in
      (var_bvars)
  | Raw.PBool (_x0) ->
      (var_bvars)
  | Raw.PTagTuple (_tag1, _patterns0) ->
      let (var_bvars) = List.fold_left bvi_accu_pattern (var_bvars) _patterns0 in
      (var_bvars)

and bvi_raw_pattern = 
  function raw_pattern -> bvi_accu_raw_pattern (Identifier.Map.empty) raw_pattern

and bound_accu_raw_pattern = fun (var_bvars) -> function
  | PWildcard ->
      (var_bvars)
  | PVar (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars)
  | PBool (_x0) ->
      (var_bvars)
  | PTagTuple (_tag1, _patterns0) ->
      let (var_bvars) = List.fold_left bound_accu_pattern (var_bvars) _patterns0 in
      (var_bvars)

and export_raw_pattern : Datacon.AtomIdMap.t * Var.AtomIdMap.t -> raw_pattern -> Raw.raw_pattern = fun (datacon_om, var_im) -> function
  | PWildcard ->
      Raw.PWildcard
  | PVar (_var0) ->
      Raw.PVar (Var.AtomIdMap.lookup _var0 var_im)
  | PBool (_x0) ->
      Raw.PBool (_x0)
  | PTagTuple (_tag1, _patterns0) ->
      Raw.PTagTuple ((export_tag (datacon_om)) _tag1, List.map (export_pattern (datacon_om, var_im)) _patterns0)

and flatten_raw_pattern : raw_pattern -> Flat.raw_pattern = function
  | PWildcard ->
      Flat.PWildcard
  | PVar (_var0) ->
      Flat.PVar (_var0)
  | PBool (_x0) ->
      Flat.PBool (_x0)
  | PTagTuple (_tag1, _patterns0) ->
      Flat.PTagTuple (flatten_tag _tag1, List.map flatten_pattern _patterns0)

and unflatten_raw_pattern : Flat.raw_pattern -> raw_pattern = function
  | Flat.PWildcard ->
      PWildcard
  | Flat.PVar (_var0) ->
      PVar (_var0)
  | Flat.PBool (_x0) ->
      PBool (_x0)
  | Flat.PTagTuple (_tag1, _patterns0) ->
      PTagTuple (unflatten_tag _tag1, List.map unflatten_pattern _patterns0)

and bound_free_accu_raw_pattern = fun (var_bvars, datacon_ofvars) -> function
  | PWildcard ->
      (var_bvars, datacon_ofvars)
  | PVar (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars, datacon_ofvars)
  | PBool (_x0) ->
      (var_bvars, datacon_ofvars)
  | PTagTuple (_tag1, _patterns0) ->
      let (datacon_ofvars) = free_accu_tag (datacon_ofvars) _tag1 in
      let (var_bvars, datacon_ofvars) = List.fold_left bound_free_accu_pattern (var_bvars, datacon_ofvars) _patterns0 in
      (var_bvars, datacon_ofvars)

and aeq_raw_pattern : unit -> raw_pattern -> raw_pattern -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | PWildcard, PWildcard ->
      ()
  | PVar (_var0), PVar (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_raw_pattern");
      ()
  | PBool (_x0), PBool (_x1) ->
      ()
  | PTagTuple (_tag1, _patterns0), PTagTuple (_tag3, _patterns2) ->
      aeq_tag () _tag1 _tag3;
      List.fold_left2 aeq_pattern () _patterns0 _patterns2;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_raw_pattern")

and freshen2_raw_pattern : Var.Subst.t * Var.Subst.t -> raw_pattern -> raw_pattern -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | PWildcard, PWildcard ->
      (var_env1, var_env2)
  | PVar (_var0), PVar (_var1) ->
      let var_env1, var_env2 = Var.Subst.freshen2 _var0 var_env1 _var1 var_env2 in
      (var_env1, var_env2)
  | PBool (_x0), PBool (_x1) ->
      (var_env1, var_env2)
  | PTagTuple (_tag1, _patterns0), PTagTuple (_tag3, _patterns2) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_pattern (var_env1, var_env2) _patterns0 _patterns2 in
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

and import_lexp : datacon Identifier.Map.t * valfun Identifier.Map.t * var Identifier.Map.t -> Raw.lexp -> lexp = fun (datacon_env, valfun_env, var_env) -> function
  (_expressions0) ->
    (Annotation.map (import_expression (datacon_env, valfun_env, var_env)) _expressions0)

and subst_lexp : Datacon.Subst.t * Valfun.Subst.t * Var.Subst.t -> lexp -> lexp = fun (datacon_env, valfun_env, var_env) -> function
  (_expressions0) ->
    (Annotation.map (subst_expression (datacon_env, valfun_env, var_env)) _expressions0)

and export_lexp : Datacon.AtomIdMap.t * Valfun.AtomIdMap.t * Var.AtomIdMap.t -> lexp -> Raw.lexp = fun (datacon_m, valfun_m, var_m) -> function
  (_expressions0) ->
    (Annotation.map (export_expression (datacon_m, valfun_m, var_m)) _expressions0)

and flatten_lexp : lexp -> Flat.lexp = function
  (_expressions0) ->
    (Annotation.map flatten_expression _expressions0)

and unflatten_lexp : Flat.lexp -> lexp = function
  (_expressions0) ->
    (Annotation.map unflatten_expression _expressions0)

and free_lexp : lexp -> Datacon.AtomSet.t * Valfun.AtomSet.t * Var.AtomSet.t = 
  function lexp -> free_accu_lexp (Datacon.AtomSet.empty, Valfun.AtomSet.empty, Var.AtomSet.empty) lexp

and equal_lexp : lexp -> lexp -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_lexp x1 x2

and aeq_lexp : unit -> lexp -> lexp -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_expressions0), (_expressions1) ->
      Annotation.fold2 aeq_expression () _expressions0 _expressions1;
      ()

and free_accu_lexp = fun (datacon_fvars, valfun_fvars, var_fvars) -> function
  (_expressions0) ->
      let (datacon_fvars, valfun_fvars, var_fvars) = Annotation.fold free_accu_expression (datacon_fvars, valfun_fvars, var_fvars) _expressions0 in 
      (datacon_fvars, valfun_fvars, var_fvars)

and import_assertion : unit -> Raw.assertion -> assertion = fun () -> function
  | Raw.StaticAssertion ->
      StaticAssertion
  | Raw.DynamicAssertion ->
      DynamicAssertion

and subst_assertion : unit -> assertion -> assertion = fun () -> function
  | StaticAssertion ->
      StaticAssertion
  | DynamicAssertion ->
      DynamicAssertion

and export_assertion : unit -> assertion -> Raw.assertion = fun () -> function
  | StaticAssertion ->
      Raw.StaticAssertion
  | DynamicAssertion ->
      Raw.DynamicAssertion

and flatten_assertion : assertion -> Flat.assertion = function
  | StaticAssertion ->
      Flat.StaticAssertion
  | DynamicAssertion ->
      Flat.DynamicAssertion

and unflatten_assertion : Flat.assertion -> assertion = function
  | Flat.StaticAssertion ->
      StaticAssertion
  | Flat.DynamicAssertion ->
      DynamicAssertion

and free_assertion : assertion -> unit = 
  function assertion -> free_accu_assertion () assertion

and equal_assertion : assertion -> assertion -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_assertion x1 x2

and aeq_assertion : unit -> assertion -> assertion -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | StaticAssertion, StaticAssertion ->
      ()
  | DynamicAssertion, DynamicAssertion ->
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_assertion")

and free_accu_assertion = fun () -> function
  | StaticAssertion ->
      ()
  | DynamicAssertion ->
      ()

and import_expression : datacon Identifier.Map.t * valfun Identifier.Map.t * var Identifier.Map.t -> Raw.expression -> expression = fun (datacon_env, valfun_env, var_env) -> function
  | Raw.EVar (_var0) ->
      EVar (Var.find _var0 var_env)
  | Raw.EBool (_x0) ->
      EBool (_x0)
  | Raw.ETagTuple (_tag1, _lexps0) ->
      ETagTuple ((import_tag (datacon_env)) _tag1, List.map (import_lexp (datacon_env, valfun_env, var_env)) _lexps0)
  | Raw.EFresh (_ofr0) ->
      let (var_bvars) = bvi_ofr _ofr0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _ofr0 = import_ofr (datacon_env, valfun_env, var_ienv) _ofr0 in
      EFresh (create_ofr _ofr0)
  | Raw.ECase (_lexp1, _branchs0) ->
      ECase ((import_lexp (datacon_env, valfun_env, var_env)) _lexp1, List.map (import_branch (datacon_env, valfun_env, var_env)) _branchs0)
  | Raw.ECall (_callee1, _lexp0) ->
      ECall ((import_callee (valfun_env)) _callee1, (import_lexp (datacon_env, valfun_env, var_env)) _lexp0)
  | Raw.EMulti (_lexps0) ->
      EMulti (List.map (import_lexp (datacon_env, valfun_env, var_env)) _lexps0)
  | Raw.ELetMulti (_lexp1, _olm0) ->
      let (var_bvars) = bvi_olm _olm0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _olm0 = import_olm (datacon_env, valfun_env, var_ienv) _olm0 in
      ELetMulti ((import_lexp (datacon_env, valfun_env, var_env)) _lexp1, create_olm _olm0)
  | Raw.EAssert (_assertion2, _contrainte1, _lexp0) ->
      EAssert ((import_assertion ()) _assertion2, (import_contrainte (var_env)) _contrainte1, (import_lexp (datacon_env, valfun_env, var_env)) _lexp0)
  | Raw.EAbsurd ->
      EAbsurd
  | Raw.EFail ->
      EFail
  | Raw.ENextCase ->
      ENextCase

and subst_expression : Datacon.Subst.t * Valfun.Subst.t * Var.Subst.t -> expression -> expression = fun (datacon_env, valfun_env, var_env) -> function
  | EVar (_var0) ->
      EVar (Var.Subst.lookup _var0 var_env)
  | EBool (_x0) ->
      EBool (_x0)
  | ETagTuple (_tag1, _lexps0) ->
      ETagTuple ((subst_tag (datacon_env)) _tag1, List.map (subst_lexp (datacon_env, valfun_env, var_env)) _lexps0)
  | EFresh (_ofr0) ->
      EFresh (apply_ofr (datacon_env, valfun_env, var_env) _ofr0)
  | ECase (_lexp1, _branchs0) ->
      ECase ((subst_lexp (datacon_env, valfun_env, var_env)) _lexp1, List.map (subst_branch (datacon_env, valfun_env, var_env)) _branchs0)
  | ECall (_callee1, _lexp0) ->
      ECall ((subst_callee (valfun_env)) _callee1, (subst_lexp (datacon_env, valfun_env, var_env)) _lexp0)
  | EMulti (_lexps0) ->
      EMulti (List.map (subst_lexp (datacon_env, valfun_env, var_env)) _lexps0)
  | ELetMulti (_lexp1, _olm0) ->
      ELetMulti ((subst_lexp (datacon_env, valfun_env, var_env)) _lexp1, apply_olm (datacon_env, valfun_env, var_env) _olm0)
  | EAssert (_assertion2, _contrainte1, _lexp0) ->
      EAssert ((subst_assertion ()) _assertion2, (subst_contrainte (var_env)) _contrainte1, (subst_lexp (datacon_env, valfun_env, var_env)) _lexp0)
  | EAbsurd ->
      EAbsurd
  | EFail ->
      EFail
  | ENextCase ->
      ENextCase

and export_expression : Datacon.AtomIdMap.t * Valfun.AtomIdMap.t * Var.AtomIdMap.t -> expression -> Raw.expression = fun (datacon_m, valfun_m, var_m) -> function
  | EVar (_var0) ->
      Raw.EVar (Var.AtomIdMap.lookup _var0 var_m)
  | EBool (_x0) ->
      Raw.EBool (_x0)
  | ETagTuple (_tag1, _lexps0) ->
      Raw.ETagTuple ((export_tag (datacon_m)) _tag1, List.map (export_lexp (datacon_m, valfun_m, var_m)) _lexps0)
  | EFresh (_ofr0) ->
      let ofr = open_ofr _ofr0 in
      let (var_bvars) = bound_ofr ofr in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.EFresh (export_ofr (datacon_m, valfun_m, var_im) ofr)
  | ECase (_lexp1, _branchs0) ->
      Raw.ECase ((export_lexp (datacon_m, valfun_m, var_m)) _lexp1, List.map (export_branch (datacon_m, valfun_m, var_m)) _branchs0)
  | ECall (_callee1, _lexp0) ->
      Raw.ECall ((export_callee (valfun_m)) _callee1, (export_lexp (datacon_m, valfun_m, var_m)) _lexp0)
  | EMulti (_lexps0) ->
      Raw.EMulti (List.map (export_lexp (datacon_m, valfun_m, var_m)) _lexps0)
  | ELetMulti (_lexp1, _olm0) ->
      let olm = open_olm _olm0 in
      let (var_bvars) = bound_olm olm in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
      Raw.ELetMulti ((export_lexp (datacon_m, valfun_m, var_m)) _lexp1, export_olm (datacon_m, valfun_m, var_im) olm)
  | EAssert (_assertion2, _contrainte1, _lexp0) ->
      Raw.EAssert ((export_assertion ()) _assertion2, (export_contrainte (var_m)) _contrainte1, (export_lexp (datacon_m, valfun_m, var_m)) _lexp0)
  | EAbsurd ->
      Raw.EAbsurd
  | EFail ->
      Raw.EFail
  | ENextCase ->
      Raw.ENextCase

and flatten_expression : expression -> Flat.expression = function
  | EVar (_var0) ->
      Flat.EVar (_var0)
  | EBool (_x0) ->
      Flat.EBool (_x0)
  | ETagTuple (_tag1, _lexps0) ->
      Flat.ETagTuple (flatten_tag _tag1, List.map flatten_lexp _lexps0)
  | EFresh (_ofr0) ->
      let ofr = open_ofr _ofr0 in
      Flat.EFresh (flatten_ofr ofr)
  | ECase (_lexp1, _branchs0) ->
      Flat.ECase (flatten_lexp _lexp1, List.map flatten_branch _branchs0)
  | ECall (_callee1, _lexp0) ->
      Flat.ECall (flatten_callee _callee1, flatten_lexp _lexp0)
  | EMulti (_lexps0) ->
      Flat.EMulti (List.map flatten_lexp _lexps0)
  | ELetMulti (_lexp1, _olm0) ->
      let olm = open_olm _olm0 in
      Flat.ELetMulti (flatten_lexp _lexp1, flatten_olm olm)
  | EAssert (_assertion2, _contrainte1, _lexp0) ->
      Flat.EAssert (flatten_assertion _assertion2, flatten_contrainte _contrainte1, flatten_lexp _lexp0)
  | EAbsurd ->
      Flat.EAbsurd
  | EFail ->
      Flat.EFail
  | ENextCase ->
      Flat.ENextCase

and unflatten_expression : Flat.expression -> expression = function
  | Flat.EVar (_var0) ->
      EVar (_var0)
  | Flat.EBool (_x0) ->
      EBool (_x0)
  | Flat.ETagTuple (_tag1, _lexps0) ->
      ETagTuple (unflatten_tag _tag1, List.map unflatten_lexp _lexps0)
  | Flat.EFresh (_ofr0) ->
      let ofr = unflatten_ofr _ofr0 in
      EFresh (create_ofr ofr)
  | Flat.ECase (_lexp1, _branchs0) ->
      ECase (unflatten_lexp _lexp1, List.map unflatten_branch _branchs0)
  | Flat.ECall (_callee1, _lexp0) ->
      ECall (unflatten_callee _callee1, unflatten_lexp _lexp0)
  | Flat.EMulti (_lexps0) ->
      EMulti (List.map unflatten_lexp _lexps0)
  | Flat.ELetMulti (_lexp1, _olm0) ->
      let olm = unflatten_olm _olm0 in
      ELetMulti (unflatten_lexp _lexp1, create_olm olm)
  | Flat.EAssert (_assertion2, _contrainte1, _lexp0) ->
      EAssert (unflatten_assertion _assertion2, unflatten_contrainte _contrainte1, unflatten_lexp _lexp0)
  | Flat.EAbsurd ->
      EAbsurd
  | Flat.EFail ->
      EFail
  | Flat.ENextCase ->
      ENextCase

and free_expression : expression -> Datacon.AtomSet.t * Valfun.AtomSet.t * Var.AtomSet.t = 
  function expression -> free_accu_expression (Datacon.AtomSet.empty, Valfun.AtomSet.empty, Var.AtomSet.empty) expression

and equal_expression : expression -> expression -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_expression x1 x2

and aeq_expression : unit -> expression -> expression -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | EVar (_var0), EVar (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_expression");
      ()
  | EBool (_x0), EBool (_x1) ->
      ()
  | ETagTuple (_tag1, _lexps0), ETagTuple (_tag3, _lexps2) ->
      aeq_tag () _tag1 _tag3;
      List.fold_left2 aeq_lexp () _lexps0 _lexps2;
      ()
  | EFresh (_ofr0), EFresh (_ofr1) ->
      let _ofr0, _ofr1 = open2i_ofr _ofr0 _ofr1 in
      aeq_ofr () _ofr0 _ofr1;
      ()
  | ECase (_lexp1, _branchs0), ECase (_lexp3, _branchs2) ->
      aeq_lexp () _lexp1 _lexp3;
      List.fold_left2 aeq_branch () _branchs0 _branchs2;
      ()
  | ECall (_callee1, _lexp0), ECall (_callee3, _lexp2) ->
      aeq_callee () _callee1 _callee3;
      aeq_lexp () _lexp0 _lexp2;
      ()
  | EMulti (_lexps0), EMulti (_lexps1) ->
      List.fold_left2 aeq_lexp () _lexps0 _lexps1;
      ()
  | ELetMulti (_lexp1, _olm0), ELetMulti (_lexp3, _olm2) ->
      aeq_lexp () _lexp1 _lexp3;
      let _olm0, _olm2 = open2i_olm _olm0 _olm2 in
      aeq_olm () _olm0 _olm2;
      ()
  | EAssert (_assertion2, _contrainte1, _lexp0), EAssert (_assertion5, _contrainte4, _lexp3) ->
      aeq_assertion () _assertion2 _assertion5;
      aeq_contrainte () _contrainte1 _contrainte4;
      aeq_lexp () _lexp0 _lexp3;
      ()
  | EAbsurd, EAbsurd ->
      ()
  | EFail, EFail ->
      ()
  | ENextCase, ENextCase ->
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_expression")

and free_accu_expression = fun (datacon_fvars, valfun_fvars, var_fvars) -> function
  | EVar (_var0) ->
      let var_fvars = Var.AtomSet.add _var0 var_fvars in
      (datacon_fvars, valfun_fvars, var_fvars)
  | EBool (_x0) ->
      (datacon_fvars, valfun_fvars, var_fvars)
  | ETagTuple (_tag1, _lexps0) ->
      let (datacon_fvars) = free_accu_tag (datacon_fvars) _tag1 in 
      let (datacon_fvars, valfun_fvars, var_fvars) = List.fold_left free_accu_lexp (datacon_fvars, valfun_fvars, var_fvars) _lexps0 in 
      (datacon_fvars, valfun_fvars, var_fvars)
  | EFresh (_ofr0) ->
      let ofr = open_ofr _ofr0 in
      let (var_bvars, var_ifvars, datacon_fvars, valfun_fvars) = bound_free_accu_ofr (Var.AtomSet.empty, Var.AtomSet.empty, datacon_fvars, valfun_fvars) ofr in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (datacon_fvars, valfun_fvars, var_fvars)
  | ECase (_lexp1, _branchs0) ->
      let (datacon_fvars, valfun_fvars, var_fvars) = free_accu_lexp (datacon_fvars, valfun_fvars, var_fvars) _lexp1 in 
      let (datacon_fvars, valfun_fvars, var_fvars) = List.fold_left free_accu_branch (datacon_fvars, valfun_fvars, var_fvars) _branchs0 in 
      (datacon_fvars, valfun_fvars, var_fvars)
  | ECall (_callee1, _lexp0) ->
      let (valfun_fvars) = free_accu_callee (valfun_fvars) _callee1 in 
      let (datacon_fvars, valfun_fvars, var_fvars) = free_accu_lexp (datacon_fvars, valfun_fvars, var_fvars) _lexp0 in 
      (datacon_fvars, valfun_fvars, var_fvars)
  | EMulti (_lexps0) ->
      let (datacon_fvars, valfun_fvars, var_fvars) = List.fold_left free_accu_lexp (datacon_fvars, valfun_fvars, var_fvars) _lexps0 in 
      (datacon_fvars, valfun_fvars, var_fvars)
  | ELetMulti (_lexp1, _olm0) ->
      let (datacon_fvars, valfun_fvars, var_fvars) = free_accu_lexp (datacon_fvars, valfun_fvars, var_fvars) _lexp1 in 
      let olm = open_olm _olm0 in
      let (var_bvars, var_ifvars, datacon_fvars, valfun_fvars) = bound_free_accu_olm (Var.AtomSet.empty, Var.AtomSet.empty, datacon_fvars, valfun_fvars) olm in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (datacon_fvars, valfun_fvars, var_fvars)
  | EAssert (_assertion2, _contrainte1, _lexp0) ->
      let () = free_accu_assertion () _assertion2 in 
      let (var_fvars) = free_accu_contrainte (var_fvars) _contrainte1 in 
      let (datacon_fvars, valfun_fvars, var_fvars) = free_accu_lexp (datacon_fvars, valfun_fvars, var_fvars) _lexp0 in 
      (datacon_fvars, valfun_fvars, var_fvars)
  | EAbsurd ->
      (datacon_fvars, valfun_fvars, var_fvars)
  | EFail ->
      (datacon_fvars, valfun_fvars, var_fvars)
  | ENextCase ->
      (datacon_fvars, valfun_fvars, var_fvars)

and import_callee : valfun Identifier.Map.t -> Raw.callee -> callee = fun (valfun_env) -> function
  | Raw.CUser (_valfun0) ->
      CUser (Valfun.find _valfun0 valfun_env)
  | Raw.CPrim (_primitive0) ->
      CPrim ((import_primitive ()) _primitive0)

and subst_callee : Valfun.Subst.t -> callee -> callee = fun (valfun_env) -> function
  | CUser (_valfun0) ->
      CUser (Valfun.Subst.lookup _valfun0 valfun_env)
  | CPrim (_primitive0) ->
      CPrim ((subst_primitive ()) _primitive0)

and export_callee : Valfun.AtomIdMap.t -> callee -> Raw.callee = fun (valfun_m) -> function
  | CUser (_valfun0) ->
      Raw.CUser (Valfun.AtomIdMap.lookup _valfun0 valfun_m)
  | CPrim (_primitive0) ->
      Raw.CPrim ((export_primitive ()) _primitive0)

and flatten_callee : callee -> Flat.callee = function
  | CUser (_valfun0) ->
      Flat.CUser (_valfun0)
  | CPrim (_primitive0) ->
      Flat.CPrim (flatten_primitive _primitive0)

and unflatten_callee : Flat.callee -> callee = function
  | Flat.CUser (_valfun0) ->
      CUser (_valfun0)
  | Flat.CPrim (_primitive0) ->
      CPrim (unflatten_primitive _primitive0)

and free_callee : callee -> Valfun.AtomSet.t = 
  function callee -> free_accu_callee (Valfun.AtomSet.empty) callee

and equal_callee : callee -> callee -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_callee x1 x2

and aeq_callee : unit -> callee -> callee -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | CUser (_valfun0), CUser (_valfun1) ->
      if not (Valfun.Atom.equal _valfun0 _valfun1) then raise (Invalid_argument "aeq_callee");
      ()
  | CPrim (_primitive0), CPrim (_primitive1) ->
      aeq_primitive () _primitive0 _primitive1;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_callee")

and free_accu_callee = fun (valfun_fvars) -> function
  | CUser (_valfun0) ->
      let valfun_fvars = Valfun.AtomSet.add _valfun0 valfun_fvars in
      (valfun_fvars)
  | CPrim (_primitive0) ->
      let () = free_accu_primitive () _primitive0 in 
      (valfun_fvars)

and import_primitive : unit -> Raw.primitive -> primitive = fun () -> function
  | Raw.PrimBoolAnd ->
      PrimBoolAnd
  | Raw.PrimBoolOr ->
      PrimBoolOr
  | Raw.PrimBoolNot ->
      PrimBoolNot
  | Raw.PrimAtomEquality ->
      PrimAtomEquality
  | Raw.PrimGenericSupport ->
      PrimGenericSupport
  | Raw.PrimGenericOuter ->
      PrimGenericOuter
  | Raw.PrimGenericInner ->
      PrimGenericInner
  | Raw.PrimGenericBound ->
      PrimGenericBound
  | Raw.PrimSetEmpty ->
      PrimSetEmpty
  | Raw.PrimSetMember ->
      PrimSetMember
  | Raw.PrimSetAdd ->
      PrimSetAdd
  | Raw.PrimSetUnion ->
      PrimSetUnion
  | Raw.PrimSetInter ->
      PrimSetInter
  | Raw.PrimSetMinus ->
      PrimSetMinus
  | Raw.PrimSetIsEmpty ->
      PrimSetIsEmpty
  | Raw.PrimSetChoose ->
      PrimSetChoose
  | Raw.PrimSingletonRename ->
      PrimSingletonRename

and subst_primitive : unit -> primitive -> primitive = fun () -> function
  | PrimBoolAnd ->
      PrimBoolAnd
  | PrimBoolOr ->
      PrimBoolOr
  | PrimBoolNot ->
      PrimBoolNot
  | PrimAtomEquality ->
      PrimAtomEquality
  | PrimGenericSupport ->
      PrimGenericSupport
  | PrimGenericOuter ->
      PrimGenericOuter
  | PrimGenericInner ->
      PrimGenericInner
  | PrimGenericBound ->
      PrimGenericBound
  | PrimSetEmpty ->
      PrimSetEmpty
  | PrimSetMember ->
      PrimSetMember
  | PrimSetAdd ->
      PrimSetAdd
  | PrimSetUnion ->
      PrimSetUnion
  | PrimSetInter ->
      PrimSetInter
  | PrimSetMinus ->
      PrimSetMinus
  | PrimSetIsEmpty ->
      PrimSetIsEmpty
  | PrimSetChoose ->
      PrimSetChoose
  | PrimSingletonRename ->
      PrimSingletonRename

and export_primitive : unit -> primitive -> Raw.primitive = fun () -> function
  | PrimBoolAnd ->
      Raw.PrimBoolAnd
  | PrimBoolOr ->
      Raw.PrimBoolOr
  | PrimBoolNot ->
      Raw.PrimBoolNot
  | PrimAtomEquality ->
      Raw.PrimAtomEquality
  | PrimGenericSupport ->
      Raw.PrimGenericSupport
  | PrimGenericOuter ->
      Raw.PrimGenericOuter
  | PrimGenericInner ->
      Raw.PrimGenericInner
  | PrimGenericBound ->
      Raw.PrimGenericBound
  | PrimSetEmpty ->
      Raw.PrimSetEmpty
  | PrimSetMember ->
      Raw.PrimSetMember
  | PrimSetAdd ->
      Raw.PrimSetAdd
  | PrimSetUnion ->
      Raw.PrimSetUnion
  | PrimSetInter ->
      Raw.PrimSetInter
  | PrimSetMinus ->
      Raw.PrimSetMinus
  | PrimSetIsEmpty ->
      Raw.PrimSetIsEmpty
  | PrimSetChoose ->
      Raw.PrimSetChoose
  | PrimSingletonRename ->
      Raw.PrimSingletonRename

and flatten_primitive : primitive -> Flat.primitive = function
  | PrimBoolAnd ->
      Flat.PrimBoolAnd
  | PrimBoolOr ->
      Flat.PrimBoolOr
  | PrimBoolNot ->
      Flat.PrimBoolNot
  | PrimAtomEquality ->
      Flat.PrimAtomEquality
  | PrimGenericSupport ->
      Flat.PrimGenericSupport
  | PrimGenericOuter ->
      Flat.PrimGenericOuter
  | PrimGenericInner ->
      Flat.PrimGenericInner
  | PrimGenericBound ->
      Flat.PrimGenericBound
  | PrimSetEmpty ->
      Flat.PrimSetEmpty
  | PrimSetMember ->
      Flat.PrimSetMember
  | PrimSetAdd ->
      Flat.PrimSetAdd
  | PrimSetUnion ->
      Flat.PrimSetUnion
  | PrimSetInter ->
      Flat.PrimSetInter
  | PrimSetMinus ->
      Flat.PrimSetMinus
  | PrimSetIsEmpty ->
      Flat.PrimSetIsEmpty
  | PrimSetChoose ->
      Flat.PrimSetChoose
  | PrimSingletonRename ->
      Flat.PrimSingletonRename

and unflatten_primitive : Flat.primitive -> primitive = function
  | Flat.PrimBoolAnd ->
      PrimBoolAnd
  | Flat.PrimBoolOr ->
      PrimBoolOr
  | Flat.PrimBoolNot ->
      PrimBoolNot
  | Flat.PrimAtomEquality ->
      PrimAtomEquality
  | Flat.PrimGenericSupport ->
      PrimGenericSupport
  | Flat.PrimGenericOuter ->
      PrimGenericOuter
  | Flat.PrimGenericInner ->
      PrimGenericInner
  | Flat.PrimGenericBound ->
      PrimGenericBound
  | Flat.PrimSetEmpty ->
      PrimSetEmpty
  | Flat.PrimSetMember ->
      PrimSetMember
  | Flat.PrimSetAdd ->
      PrimSetAdd
  | Flat.PrimSetUnion ->
      PrimSetUnion
  | Flat.PrimSetInter ->
      PrimSetInter
  | Flat.PrimSetMinus ->
      PrimSetMinus
  | Flat.PrimSetIsEmpty ->
      PrimSetIsEmpty
  | Flat.PrimSetChoose ->
      PrimSetChoose
  | Flat.PrimSingletonRename ->
      PrimSingletonRename

and free_primitive : primitive -> unit = 
  function primitive -> free_accu_primitive () primitive

and equal_primitive : primitive -> primitive -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_primitive x1 x2

and aeq_primitive : unit -> primitive -> primitive -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | PrimBoolAnd, PrimBoolAnd ->
      ()
  | PrimBoolOr, PrimBoolOr ->
      ()
  | PrimBoolNot, PrimBoolNot ->
      ()
  | PrimAtomEquality, PrimAtomEquality ->
      ()
  | PrimGenericSupport, PrimGenericSupport ->
      ()
  | PrimGenericOuter, PrimGenericOuter ->
      ()
  | PrimGenericInner, PrimGenericInner ->
      ()
  | PrimGenericBound, PrimGenericBound ->
      ()
  | PrimSetEmpty, PrimSetEmpty ->
      ()
  | PrimSetMember, PrimSetMember ->
      ()
  | PrimSetAdd, PrimSetAdd ->
      ()
  | PrimSetUnion, PrimSetUnion ->
      ()
  | PrimSetInter, PrimSetInter ->
      ()
  | PrimSetMinus, PrimSetMinus ->
      ()
  | PrimSetIsEmpty, PrimSetIsEmpty ->
      ()
  | PrimSetChoose, PrimSetChoose ->
      ()
  | PrimSingletonRename, PrimSingletonRename ->
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_primitive")

and free_accu_primitive = fun () -> function
  | PrimBoolAnd ->
      ()
  | PrimBoolOr ->
      ()
  | PrimBoolNot ->
      ()
  | PrimAtomEquality ->
      ()
  | PrimGenericSupport ->
      ()
  | PrimGenericOuter ->
      ()
  | PrimGenericInner ->
      ()
  | PrimGenericBound ->
      ()
  | PrimSetEmpty ->
      ()
  | PrimSetMember ->
      ()
  | PrimSetAdd ->
      ()
  | PrimSetUnion ->
      ()
  | PrimSetInter ->
      ()
  | PrimSetMinus ->
      ()
  | PrimSetIsEmpty ->
      ()
  | PrimSetChoose ->
      ()
  | PrimSingletonRename ->
      ()

and subst_mono : Var.Subst.t -> mono -> mono = fun (var_ienv) -> function
  (_var0) ->
    (Var.Subst.lookup _var0 var_ienv)

and bound_mono : mono -> Var.AtomSet.t = 
  function mono -> bound_accu_mono (Var.AtomSet.empty) mono

and bound_free_mono : mono -> Var.AtomSet.t = 
  function mono -> bound_free_accu_mono (Var.AtomSet.empty) mono

and equal_mono : mono -> mono -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_mono x1 x2

and import_mono = fun (var_ienv) -> function
  (_var0) ->
    (Var.find _var0 var_ienv)

and bvi_accu_mono = fun (var_bvars) -> function
  (_var0) ->
      let var_bvars = Identifier.Map.add _var0 () var_bvars in
      (var_bvars)

and bvi_mono = 
  function mono -> bvi_accu_mono (Identifier.Map.empty) mono

and bound_accu_mono = fun (var_bvars) -> function
  (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars)

and export_mono : Var.AtomIdMap.t -> mono -> Raw.mono = fun (var_im) -> function
  (_var0) ->
    (Var.AtomIdMap.lookup _var0 var_im)

and flatten_mono : mono -> Flat.mono = function
  (_var0) ->
    (_var0)

and unflatten_mono : Flat.mono -> mono = function
  (_var0) ->
    (_var0)

and bound_free_accu_mono = fun (var_bvars) -> function
  (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars)

and aeq_mono : unit -> mono -> mono -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var0), (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_mono");
      ()

and freshen2_mono : Var.Subst.t * Var.Subst.t -> mono -> mono -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_var0), (_var1) ->
      let var_env1, var_env2 = Var.Subst.freshen2 _var0 var_env1 _var1 var_env2 in
      (var_env1, var_env2)

and import_branch : datacon Identifier.Map.t * valfun Identifier.Map.t * var Identifier.Map.t -> Raw.branch -> branch = fun (datacon_env, valfun_env, var_env) -> function
  (_obr0) ->
      let (var_bvars) = bvi_obr _obr0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _obr0 = import_obr (datacon_env, valfun_env, var_ienv) _obr0 in
    (create_obr _obr0)

and subst_branch : Datacon.Subst.t * Valfun.Subst.t * Var.Subst.t -> branch -> branch = fun (datacon_env, valfun_env, var_env) -> function
  (_obr0) ->
    (apply_obr (datacon_env, valfun_env, var_env) _obr0)

and export_branch : Datacon.AtomIdMap.t * Valfun.AtomIdMap.t * Var.AtomIdMap.t -> branch -> Raw.branch = fun (datacon_m, valfun_m, var_m) -> function
  (_obr0) ->
      let obr = open_obr _obr0 in
      let (var_bvars) = bound_obr obr in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
    (export_obr (datacon_m, valfun_m, var_im) obr)

and flatten_branch : branch -> Flat.branch = function
  (_obr0) ->
      let obr = open_obr _obr0 in
    (flatten_obr obr)

and unflatten_branch : Flat.branch -> branch = function
  (_obr0) ->
      let obr = unflatten_obr _obr0 in
    (create_obr obr)

and free_branch : branch -> Datacon.AtomSet.t * Valfun.AtomSet.t * Var.AtomSet.t = 
  function branch -> free_accu_branch (Datacon.AtomSet.empty, Valfun.AtomSet.empty, Var.AtomSet.empty) branch

and equal_branch : branch -> branch -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_branch x1 x2

and aeq_branch : unit -> branch -> branch -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_obr0), (_obr1) ->
      let _obr0, _obr1 = open2i_obr _obr0 _obr1 in
      aeq_obr () _obr0 _obr1;
      ()

and free_accu_branch = fun (datacon_fvars, valfun_fvars, var_fvars) -> function
  (_obr0) ->
      let obr = open_obr _obr0 in
      let (var_bvars, var_ifvars, datacon_fvars, valfun_fvars) = bound_free_accu_obr (Var.AtomSet.empty, Var.AtomSet.empty, datacon_fvars, valfun_fvars) obr in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (datacon_fvars, valfun_fvars, var_fvars)

and subst_def : Var.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t -> def -> def = fun (var_oenv, datacon_ienv, datatype_ienv, valfun_ienv) -> function
  | DefValFun (_valfun1, _fundef0) ->
      DefValFun (Valfun.Subst.lookup _valfun1 valfun_ienv, (subst_fundef (datacon_ienv, valfun_ienv, var_oenv)) _fundef0)
  | DefDataType (_datatype1, _datatypedef0) ->
      DefDataType (Datatype.Subst.lookup _datatype1 datatype_ienv, (subst_datatypedef (var_oenv, datacon_ienv, datatype_ienv)) _datatypedef0)

and bound_def : def -> Datacon.AtomSet.t * Datatype.AtomSet.t * Valfun.AtomSet.t = 
  function def -> bound_accu_def (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Valfun.AtomSet.empty) def

and bound_free_def : def -> Datacon.AtomSet.t * Datatype.AtomSet.t * Valfun.AtomSet.t * Datacon.AtomSet.t * Datatype.AtomSet.t * Valfun.AtomSet.t * Var.AtomSet.t = 
  function def -> bound_free_accu_def (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Valfun.AtomSet.empty, Datacon.AtomSet.empty, Datatype.AtomSet.empty, Valfun.AtomSet.empty, Var.AtomSet.empty) def

and equal_def : def -> def -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_def x1 x2

and import_def = fun (var_oenv, datacon_ienv, datatype_ienv, valfun_ienv) -> function
  | Raw.DefValFun (_valfun1, _fundef0) ->
      DefValFun (Valfun.find _valfun1 valfun_ienv, (import_fundef (datacon_ienv, valfun_ienv, var_oenv)) _fundef0)
  | Raw.DefDataType (_datatype1, _datatypedef0) ->
      DefDataType (Datatype.find _datatype1 datatype_ienv, (import_datatypedef (var_oenv, datacon_ienv, datatype_ienv)) _datatypedef0)

and bvi_accu_def = fun (datacon_bvars, datatype_bvars, valfun_bvars) -> function
  | Raw.DefValFun (_valfun1, _fundef0) ->
      let valfun_bvars = Identifier.Map.add _valfun1 () valfun_bvars in
      (datacon_bvars, datatype_bvars, valfun_bvars)
  | Raw.DefDataType (_datatype1, _datatypedef0) ->
      let datatype_bvars = Identifier.Map.add _datatype1 () datatype_bvars in
      let (datacon_bvars) = bvi_accu_datatypedef (datacon_bvars) _datatypedef0 in
      (datacon_bvars, datatype_bvars, valfun_bvars)

and bvi_def = 
  function def -> bvi_accu_def (Identifier.Map.empty, Identifier.Map.empty, Identifier.Map.empty) def

and bound_accu_def = fun (datacon_bvars, datatype_bvars, valfun_bvars) -> function
  | DefValFun (_valfun1, _fundef0) ->
      let valfun_bvars = Valfun.AtomSet.add _valfun1 valfun_bvars in
      (datacon_bvars, datatype_bvars, valfun_bvars)
  | DefDataType (_datatype1, _datatypedef0) ->
      let datatype_bvars = Datatype.AtomSet.add _datatype1 datatype_bvars in
      let (datacon_bvars) = bound_accu_datatypedef (datacon_bvars) _datatypedef0 in
      (datacon_bvars, datatype_bvars, valfun_bvars)

and export_def : Var.AtomIdMap.t * Datacon.AtomIdMap.t * Datatype.AtomIdMap.t * Valfun.AtomIdMap.t -> def -> Raw.def = fun (var_om, datacon_im, datatype_im, valfun_im) -> function
  | DefValFun (_valfun1, _fundef0) ->
      Raw.DefValFun (Valfun.AtomIdMap.lookup _valfun1 valfun_im, (export_fundef (datacon_im, valfun_im, var_om)) _fundef0)
  | DefDataType (_datatype1, _datatypedef0) ->
      Raw.DefDataType (Datatype.AtomIdMap.lookup _datatype1 datatype_im, (export_datatypedef (var_om, datacon_im, datatype_im)) _datatypedef0)

and flatten_def : def -> Flat.def = function
  | DefValFun (_valfun1, _fundef0) ->
      Flat.DefValFun (_valfun1, flatten_fundef _fundef0)
  | DefDataType (_datatype1, _datatypedef0) ->
      Flat.DefDataType (_datatype1, flatten_datatypedef _datatypedef0)

and unflatten_def : Flat.def -> def = function
  | Flat.DefValFun (_valfun1, _fundef0) ->
      DefValFun (_valfun1, unflatten_fundef _fundef0)
  | Flat.DefDataType (_datatype1, _datatypedef0) ->
      DefDataType (_datatype1, unflatten_datatypedef _datatypedef0)

and bound_free_accu_def = fun (datacon_bvars, datatype_bvars, valfun_bvars, datacon_ifvars, datatype_ifvars, valfun_ifvars, var_ofvars) -> function
  | DefValFun (_valfun1, _fundef0) ->
      let valfun_bvars = Valfun.AtomSet.add _valfun1 valfun_bvars in
      let (datacon_ifvars, valfun_ifvars, var_ofvars) = free_accu_fundef (datacon_ifvars, valfun_ifvars, var_ofvars) _fundef0 in
      (datacon_bvars, datatype_bvars, valfun_bvars, datacon_ifvars, datatype_ifvars, valfun_ifvars, var_ofvars)
  | DefDataType (_datatype1, _datatypedef0) ->
      let datatype_bvars = Datatype.AtomSet.add _datatype1 datatype_bvars in
      let (datacon_bvars, datatype_ifvars, var_ofvars) = bound_free_accu_datatypedef (datacon_bvars, datatype_ifvars, var_ofvars) _datatypedef0 in
      (datacon_bvars, datatype_bvars, valfun_bvars, datacon_ifvars, datatype_ifvars, valfun_ifvars, var_ofvars)

and aeq_def : unit -> def -> def -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | DefValFun (_valfun1, _fundef0), DefValFun (_valfun3, _fundef2) ->
      if not (Valfun.Atom.equal _valfun1 _valfun3) then raise (Invalid_argument "aeq_def");
      aeq_fundef () _fundef0 _fundef2;
      ()
  | DefDataType (_datatype1, _datatypedef0), DefDataType (_datatype3, _datatypedef2) ->
      if not (Datatype.Atom.equal _datatype1 _datatype3) then raise (Invalid_argument "aeq_def");
      aeq_datatypedef () _datatypedef0 _datatypedef2;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_def")

and freshen2_def : Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t -> def -> def -> Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t = fun (datacon_env1, datatype_env1, valfun_env1, datacon_env2, datatype_env2, valfun_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | DefValFun (_valfun1, _fundef0), DefValFun (_valfun3, _fundef2) ->
      let valfun_env1, valfun_env2 = Valfun.Subst.freshen2 _valfun1 valfun_env1 _valfun3 valfun_env2 in
      (datacon_env1, datatype_env1, valfun_env1, datacon_env2, datatype_env2, valfun_env2)
  | DefDataType (_datatype1, _datatypedef0), DefDataType (_datatype3, _datatypedef2) ->
      let datatype_env1, datatype_env2 = Datatype.Subst.freshen2 _datatype1 datatype_env1 _datatype3 datatype_env2 in
      let (datacon_env1, datatype_env1, valfun_env1, datacon_env2, datatype_env2, valfun_env2) = freshen2_datatypedef (datacon_env1, datatype_env1, valfun_env1, datacon_env2, datatype_env2, valfun_env2) _datatypedef0 _datatypedef2 in
      (datacon_env1, datatype_env1, valfun_env1, datacon_env2, datatype_env2, valfun_env2)
  | _, _ ->
      raise (Invalid_argument "freshen2_def")

and import_fundef : datacon Identifier.Map.t * valfun Identifier.Map.t * var Identifier.Map.t -> Raw.fundef -> fundef = fun (datacon_env, valfun_env, var_env) -> function
  (_ofu0) ->
      let (var_bvars) = bvi_ofu _ofu0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _ofu0 = import_ofu (datacon_env, valfun_env, var_ienv) _ofu0 in
    (create_ofu _ofu0)

and subst_fundef : Datacon.Subst.t * Valfun.Subst.t * Var.Subst.t -> fundef -> fundef = fun (datacon_env, valfun_env, var_env) -> function
  (_ofu0) ->
    (apply_ofu (datacon_env, valfun_env, var_env) _ofu0)

and export_fundef : Datacon.AtomIdMap.t * Valfun.AtomIdMap.t * Var.AtomIdMap.t -> fundef -> Raw.fundef = fun (datacon_m, valfun_m, var_m) -> function
  (_ofu0) ->
      let ofu = open_ofu _ofu0 in
      let (var_bvars) = bound_ofu ofu in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
    (export_ofu (datacon_m, valfun_m, var_im) ofu)

and flatten_fundef : fundef -> Flat.fundef = function
  (_ofu0) ->
      let ofu = open_ofu _ofu0 in
    (flatten_ofu ofu)

and unflatten_fundef : Flat.fundef -> fundef = function
  (_ofu0) ->
      let ofu = unflatten_ofu _ofu0 in
    (create_ofu ofu)

and free_fundef : fundef -> Datacon.AtomSet.t * Valfun.AtomSet.t * Var.AtomSet.t = 
  function fundef -> free_accu_fundef (Datacon.AtomSet.empty, Valfun.AtomSet.empty, Var.AtomSet.empty) fundef

and equal_fundef : fundef -> fundef -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_fundef x1 x2

and aeq_fundef : unit -> fundef -> fundef -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_ofu0), (_ofu1) ->
      let _ofu0, _ofu1 = open2i_ofu _ofu0 _ofu1 in
      aeq_ofu () _ofu0 _ofu1;
      ()

and free_accu_fundef = fun (datacon_fvars, valfun_fvars, var_fvars) -> function
  (_ofu0) ->
      let ofu = open_ofu _ofu0 in
      let (var_bvars, var_ifvars, datacon_fvars, valfun_fvars) = bound_free_accu_ofu (Var.AtomSet.empty, Var.AtomSet.empty, datacon_fvars, valfun_fvars) ofu in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (datacon_fvars, valfun_fvars, var_fvars)

and import_specification : var Identifier.Map.t -> Raw.specification -> specification = fun (var_env) -> function
  (_osp0) ->
      let (var_bvars) = bvi_osp _osp0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _osp0 = import_osp (var_ienv) _osp0 in
    (create_osp _osp0)

and subst_specification : Var.Subst.t -> specification -> specification = fun (var_env) -> function
  (_osp0) ->
    (apply_osp (var_env) _osp0)

and export_specification : Var.AtomIdMap.t -> specification -> Raw.specification = fun (var_m) -> function
  (_osp0) ->
      let osp = open_osp _osp0 in
      let (var_bvars) = bound_osp osp in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
    (export_osp (var_im) osp)

and flatten_specification : specification -> Flat.specification = function
  (_osp0) ->
      let osp = open_osp _osp0 in
    (flatten_osp osp)

and unflatten_specification : Flat.specification -> specification = function
  (_osp0) ->
      let osp = unflatten_osp _osp0 in
    (create_osp osp)

and free_specification : specification -> Var.AtomSet.t = 
  function specification -> free_accu_specification (Var.AtomSet.empty) specification

and equal_specification : specification -> specification -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_specification x1 x2

and aeq_specification : unit -> specification -> specification -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_osp0), (_osp1) ->
      let _osp0, _osp1 = open2i_osp _osp0 _osp1 in
      aeq_osp () _osp0 _osp1;
      ()

and free_accu_specification = fun (var_fvars) -> function
  (_osp0) ->
      let osp = open_osp _osp0 in
      let (var_bvars, var_ifvars) = bound_free_accu_osp (Var.AtomSet.empty, Var.AtomSet.empty) osp in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)

and import_postcondition : var Identifier.Map.t -> Raw.postcondition -> postcondition = fun (var_env) -> function
  (_opc0) ->
      let (var_bvars) = bvi_opc _opc0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _opc0 = import_opc (var_ienv) _opc0 in
    (create_opc _opc0)

and subst_postcondition : Var.Subst.t -> postcondition -> postcondition = fun (var_env) -> function
  (_opc0) ->
    (apply_opc (var_env) _opc0)

and export_postcondition : Var.AtomIdMap.t -> postcondition -> Raw.postcondition = fun (var_m) -> function
  (_opc0) ->
      let opc = open_opc _opc0 in
      let (var_bvars) = bound_opc opc in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
    (export_opc (var_im) opc)

and flatten_postcondition : postcondition -> Flat.postcondition = function
  (_opc0) ->
      let opc = open_opc _opc0 in
    (flatten_opc opc)

and unflatten_postcondition : Flat.postcondition -> postcondition = function
  (_opc0) ->
      let opc = unflatten_opc _opc0 in
    (create_opc opc)

and free_postcondition : postcondition -> Var.AtomSet.t = 
  function postcondition -> free_accu_postcondition (Var.AtomSet.empty) postcondition

and equal_postcondition : postcondition -> postcondition -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_postcondition x1 x2

and aeq_postcondition : unit -> postcondition -> postcondition -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_opc0), (_opc1) ->
      let _opc0, _opc1 = open2i_opc _opc0 _opc1 in
      aeq_opc () _opc0 _opc1;
      ()

and free_accu_postcondition = fun (var_fvars) -> function
  (_opc0) ->
      let opc = open_opc _opc0 in
      let (var_bvars, var_ifvars) = bound_free_accu_opc (Var.AtomSet.empty, Var.AtomSet.empty) opc in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (var_fvars)

and subst_datatypedef : Var.Subst.t * Datacon.Subst.t * Datatype.Subst.t -> datatypedef -> datatypedef = fun (var_oenv, datacon_ienv, datatype_ienv) -> function
  (_kind1, _datacondefs0) ->
    ((subst_kind ()) _kind1, List.map (subst_datacondef (var_oenv, datacon_ienv, datatype_ienv)) _datacondefs0)

and bound_datatypedef : datatypedef -> Datacon.AtomSet.t = 
  function datatypedef -> bound_accu_datatypedef (Datacon.AtomSet.empty) datatypedef

and bound_free_datatypedef : datatypedef -> Datacon.AtomSet.t * Datatype.AtomSet.t * Var.AtomSet.t = 
  function datatypedef -> bound_free_accu_datatypedef (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Var.AtomSet.empty) datatypedef

and equal_datatypedef : datatypedef -> datatypedef -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_datatypedef x1 x2

and import_datatypedef = fun (var_oenv, datacon_ienv, datatype_ienv) -> function
  (_kind1, _datacondefs0) ->
    ((import_kind ()) _kind1, List.map (import_datacondef (var_oenv, datacon_ienv, datatype_ienv)) _datacondefs0)

and bvi_accu_datatypedef = fun (datacon_bvars) -> function
  (_kind1, _datacondefs0) ->
      let (datacon_bvars) = List.fold_left bvi_accu_datacondef (datacon_bvars) _datacondefs0 in
      (datacon_bvars)

and bvi_datatypedef = 
  function datatypedef -> bvi_accu_datatypedef (Identifier.Map.empty) datatypedef

and bound_accu_datatypedef = fun (datacon_bvars) -> function
  (_kind1, _datacondefs0) ->
      let (datacon_bvars) = List.fold_left bound_accu_datacondef (datacon_bvars) _datacondefs0 in
      (datacon_bvars)

and export_datatypedef : Var.AtomIdMap.t * Datacon.AtomIdMap.t * Datatype.AtomIdMap.t -> datatypedef -> Raw.datatypedef = fun (var_om, datacon_im, datatype_im) -> function
  (_kind1, _datacondefs0) ->
    ((export_kind ()) _kind1, List.map (export_datacondef (var_om, datacon_im, datatype_im)) _datacondefs0)

and flatten_datatypedef : datatypedef -> Flat.datatypedef = function
  (_kind1, _datacondefs0) ->
    (flatten_kind _kind1, List.map flatten_datacondef _datacondefs0)

and unflatten_datatypedef : Flat.datatypedef -> datatypedef = function
  (_kind1, _datacondefs0) ->
    (unflatten_kind _kind1, List.map unflatten_datacondef _datacondefs0)

and bound_free_accu_datatypedef = fun (datacon_bvars, datatype_ifvars, var_ofvars) -> function
  (_kind1, _datacondefs0) ->
      let () = free_accu_kind () _kind1 in
      let (datacon_bvars, datatype_ifvars, var_ofvars) = List.fold_left bound_free_accu_datacondef (datacon_bvars, datatype_ifvars, var_ofvars) _datacondefs0 in
      (datacon_bvars, datatype_ifvars, var_ofvars)

and aeq_datatypedef : unit -> datatypedef -> datatypedef -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_kind1, _datacondefs0), (_kind3, _datacondefs2) ->
      aeq_kind () _kind1 _kind3;
      List.fold_left2 aeq_datacondef () _datacondefs0 _datacondefs2;
      ()

and freshen2_datatypedef : Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t -> datatypedef -> datatypedef -> Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t = fun (datacon_env1, datatype_env1, valfun_env1, datacon_env2, datatype_env2, valfun_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_kind1, _datacondefs0), (_kind3, _datacondefs2) ->
      let (datacon_env1, datatype_env1, valfun_env1, datacon_env2, datatype_env2, valfun_env2) = List.fold_left2 freshen2_datacondef (datacon_env1, datatype_env1, valfun_env1, datacon_env2, datatype_env2, valfun_env2) _datacondefs0 _datacondefs2 in
      (datacon_env1, datatype_env1, valfun_env1, datacon_env2, datatype_env2, valfun_env2)

and import_kind : unit -> Raw.kind -> kind = fun () -> function
  | Raw.KExpression ->
      KExpression
  | Raw.KPattern ->
      KPattern

and subst_kind : unit -> kind -> kind = fun () -> function
  | KExpression ->
      KExpression
  | KPattern ->
      KPattern

and export_kind : unit -> kind -> Raw.kind = fun () -> function
  | KExpression ->
      Raw.KExpression
  | KPattern ->
      Raw.KPattern

and flatten_kind : kind -> Flat.kind = function
  | KExpression ->
      Flat.KExpression
  | KPattern ->
      Flat.KPattern

and unflatten_kind : Flat.kind -> kind = function
  | Flat.KExpression ->
      KExpression
  | Flat.KPattern ->
      KPattern

and free_kind : kind -> unit = 
  function kind -> free_accu_kind () kind

and equal_kind : kind -> kind -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_kind x1 x2

and aeq_kind : unit -> kind -> kind -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | KExpression, KExpression ->
      ()
  | KPattern, KPattern ->
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_kind")

and free_accu_kind = fun () -> function
  | KExpression ->
      ()
  | KPattern ->
      ()

and subst_datacondef : Var.Subst.t * Datacon.Subst.t * Datatype.Subst.t -> datacondef -> datacondef = fun (var_oenv, datacon_ienv, datatype_ienv) -> function
  (_datacon1, _datacondetails0) ->
    (Datacon.Subst.lookup _datacon1 datacon_ienv, (subst_datacondetails (datatype_ienv, var_oenv)) _datacondetails0)

and bound_datacondef : datacondef -> Datacon.AtomSet.t = 
  function datacondef -> bound_accu_datacondef (Datacon.AtomSet.empty) datacondef

and bound_free_datacondef : datacondef -> Datacon.AtomSet.t * Datatype.AtomSet.t * Var.AtomSet.t = 
  function datacondef -> bound_free_accu_datacondef (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Var.AtomSet.empty) datacondef

and equal_datacondef : datacondef -> datacondef -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_datacondef x1 x2

and import_datacondef = fun (var_oenv, datacon_ienv, datatype_ienv) -> function
  (_datacon1, _datacondetails0) ->
    (Datacon.find _datacon1 datacon_ienv, (import_datacondetails (datatype_ienv, var_oenv)) _datacondetails0)

and bvi_accu_datacondef = fun (datacon_bvars) -> function
  (_datacon1, _datacondetails0) ->
      let datacon_bvars = Identifier.Map.add _datacon1 () datacon_bvars in
      (datacon_bvars)

and bvi_datacondef = 
  function datacondef -> bvi_accu_datacondef (Identifier.Map.empty) datacondef

and bound_accu_datacondef = fun (datacon_bvars) -> function
  (_datacon1, _datacondetails0) ->
      let datacon_bvars = Datacon.AtomSet.add _datacon1 datacon_bvars in
      (datacon_bvars)

and export_datacondef : Var.AtomIdMap.t * Datacon.AtomIdMap.t * Datatype.AtomIdMap.t -> datacondef -> Raw.datacondef = fun (var_om, datacon_im, datatype_im) -> function
  (_datacon1, _datacondetails0) ->
    (Datacon.AtomIdMap.lookup _datacon1 datacon_im, (export_datacondetails (datatype_im, var_om)) _datacondetails0)

and flatten_datacondef : datacondef -> Flat.datacondef = function
  (_datacon1, _datacondetails0) ->
    (_datacon1, flatten_datacondetails _datacondetails0)

and unflatten_datacondef : Flat.datacondef -> datacondef = function
  (_datacon1, _datacondetails0) ->
    (_datacon1, unflatten_datacondetails _datacondetails0)

and bound_free_accu_datacondef = fun (datacon_bvars, datatype_ifvars, var_ofvars) -> function
  (_datacon1, _datacondetails0) ->
      let datacon_bvars = Datacon.AtomSet.add _datacon1 datacon_bvars in
      let (datatype_ifvars, var_ofvars) = free_accu_datacondetails (datatype_ifvars, var_ofvars) _datacondetails0 in
      (datacon_bvars, datatype_ifvars, var_ofvars)

and aeq_datacondef : unit -> datacondef -> datacondef -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_datacon1, _datacondetails0), (_datacon3, _datacondetails2) ->
      if not (Datacon.Atom.equal _datacon1 _datacon3) then raise (Invalid_argument "aeq_datacondef");
      aeq_datacondetails () _datacondetails0 _datacondetails2;
      ()

and freshen2_datacondef : Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t -> datacondef -> datacondef -> Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t = fun (datacon_env1, datatype_env1, valfun_env1, datacon_env2, datatype_env2, valfun_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_datacon1, _datacondetails0), (_datacon3, _datacondetails2) ->
      let datacon_env1, datacon_env2 = Datacon.Subst.freshen2 _datacon1 datacon_env1 _datacon3 datacon_env2 in
      (datacon_env1, datatype_env1, valfun_env1, datacon_env2, datatype_env2, valfun_env2)

and import_datacondetails : datatype Identifier.Map.t * var Identifier.Map.t -> Raw.datacondetails -> datacondetails = fun (datatype_env, var_env) -> function
  (_dcd0) ->
      let (var_bvars) = bvi_dcd _dcd0 in 
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _dcd0 = import_dcd (datatype_env, var_ienv) _dcd0 in
    (create_dcd _dcd0)

and subst_datacondetails : Datatype.Subst.t * Var.Subst.t -> datacondetails -> datacondetails = fun (datatype_env, var_env) -> function
  (_dcd0) ->
    (apply_dcd (datatype_env, var_env) _dcd0)

and export_datacondetails : Datatype.AtomIdMap.t * Var.AtomIdMap.t -> datacondetails -> Raw.datacondetails = fun (datatype_m, var_m) -> function
  (_dcd0) ->
      let dcd = open_dcd _dcd0 in
      let (var_bvars) = bound_dcd dcd in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
    (export_dcd (datatype_m, var_im) dcd)

and flatten_datacondetails : datacondetails -> Flat.datacondetails = function
  (_dcd0) ->
      let dcd = open_dcd _dcd0 in
    (flatten_dcd dcd)

and unflatten_datacondetails : Flat.datacondetails -> datacondetails = function
  (_dcd0) ->
      let dcd = unflatten_dcd _dcd0 in
    (create_dcd dcd)

and free_datacondetails : datacondetails -> Datatype.AtomSet.t * Var.AtomSet.t = 
  function datacondetails -> free_accu_datacondetails (Datatype.AtomSet.empty, Var.AtomSet.empty) datacondetails

and equal_datacondetails : datacondetails -> datacondetails -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_datacondetails x1 x2

and aeq_datacondetails : unit -> datacondetails -> datacondetails -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_dcd0), (_dcd1) ->
      let _dcd0, _dcd1 = open2i_dcd _dcd0 _dcd1 in
      aeq_dcd () _dcd0 _dcd1;
      ()

and free_accu_datacondetails = fun (datatype_fvars, var_fvars) -> function
  (_dcd0) ->
      let dcd = open_dcd _dcd0 in
      let (var_bvars, var_ifvars, datatype_fvars) = bound_free_accu_dcd (Var.AtomSet.empty, Var.AtomSet.empty, datatype_fvars) dcd in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (datatype_fvars, var_fvars)

and subst_defs : Var.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t -> defs -> defs = fun (var_oenv, datacon_ienv, datatype_ienv, valfun_ienv) -> function
  (_defs0) ->
    (List.map (subst_def (var_oenv, datacon_ienv, datatype_ienv, valfun_ienv)) _defs0)

and bound_defs : defs -> Datacon.AtomSet.t * Datatype.AtomSet.t * Valfun.AtomSet.t = 
  function defs -> bound_accu_defs (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Valfun.AtomSet.empty) defs

and bound_free_defs : defs -> Datacon.AtomSet.t * Datatype.AtomSet.t * Valfun.AtomSet.t * Datacon.AtomSet.t * Datatype.AtomSet.t * Valfun.AtomSet.t * Var.AtomSet.t = 
  function defs -> bound_free_accu_defs (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Valfun.AtomSet.empty, Datacon.AtomSet.empty, Datatype.AtomSet.empty, Valfun.AtomSet.empty, Var.AtomSet.empty) defs

and equal_defs : defs -> defs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_defs x1 x2

and import_defs = fun (var_oenv, datacon_ienv, datatype_ienv, valfun_ienv) -> function
  (_defs0) ->
    (List.map (import_def (var_oenv, datacon_ienv, datatype_ienv, valfun_ienv)) _defs0)

and bvi_accu_defs = fun (datacon_bvars, datatype_bvars, valfun_bvars) -> function
  (_defs0) ->
      let (datacon_bvars, datatype_bvars, valfun_bvars) = List.fold_left bvi_accu_def (datacon_bvars, datatype_bvars, valfun_bvars) _defs0 in
      (datacon_bvars, datatype_bvars, valfun_bvars)

and bvi_defs = 
  function defs -> bvi_accu_defs (Identifier.Map.empty, Identifier.Map.empty, Identifier.Map.empty) defs

and bound_accu_defs = fun (datacon_bvars, datatype_bvars, valfun_bvars) -> function
  (_defs0) ->
      let (datacon_bvars, datatype_bvars, valfun_bvars) = List.fold_left bound_accu_def (datacon_bvars, datatype_bvars, valfun_bvars) _defs0 in
      (datacon_bvars, datatype_bvars, valfun_bvars)

and export_defs : Var.AtomIdMap.t * Datacon.AtomIdMap.t * Datatype.AtomIdMap.t * Valfun.AtomIdMap.t -> defs -> Raw.defs = fun (var_om, datacon_im, datatype_im, valfun_im) -> function
  (_defs0) ->
    (List.map (export_def (var_om, datacon_im, datatype_im, valfun_im)) _defs0)

and flatten_defs : defs -> Flat.defs = function
  (_defs0) ->
    (List.map flatten_def _defs0)

and unflatten_defs : Flat.defs -> defs = function
  (_defs0) ->
    (List.map unflatten_def _defs0)

and bound_free_accu_defs = fun (datacon_bvars, datatype_bvars, valfun_bvars, datacon_ifvars, datatype_ifvars, valfun_ifvars, var_ofvars) -> function
  (_defs0) ->
      let (datacon_bvars, datatype_bvars, valfun_bvars, datacon_ifvars, datatype_ifvars, valfun_ifvars, var_ofvars) = List.fold_left bound_free_accu_def (datacon_bvars, datatype_bvars, valfun_bvars, datacon_ifvars, datatype_ifvars, valfun_ifvars, var_ofvars) _defs0 in
      (datacon_bvars, datatype_bvars, valfun_bvars, datacon_ifvars, datatype_ifvars, valfun_ifvars, var_ofvars)

and aeq_defs : unit -> defs -> defs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_defs0), (_defs1) ->
      List.fold_left2 aeq_def () _defs0 _defs1;
      ()

and freshen2_defs : Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t -> defs -> defs -> Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t * Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t = fun (datacon_env1, datatype_env1, valfun_env1, datacon_env2, datatype_env2, valfun_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_defs0), (_defs1) ->
      let (datacon_env1, datatype_env1, valfun_env1, datacon_env2, datatype_env2, valfun_env2) = List.fold_left2 freshen2_def (datacon_env1, datatype_env1, valfun_env1, datacon_env2, datatype_env2, valfun_env2) _defs0 _defs1 in
      (datacon_env1, datatype_env1, valfun_env1, datacon_env2, datatype_env2, valfun_env2)

and create_defs : defs -> opaque_defs = 
  function body -> {
    defs_delayed = (Datacon.Subst.id, Datatype.Subst.id, Valfun.Subst.id, Var.Subst.id);
    defs = body
  }

and open_defs : opaque_defs -> defs = function abstraction ->
  let (datacon_delayed, datatype_delayed, valfun_delayed, var_delayed) = abstraction.defs_delayed in
  let body = abstraction.defs in
  let (datacon_bvars, datatype_bvars, valfun_bvars) = bound_defs body in
  let datacon_env = Datacon.Subst.freshen datacon_bvars datacon_delayed in
  let datatype_env = Datatype.Subst.freshen datatype_bvars datatype_delayed in
  let valfun_env = Valfun.Subst.freshen valfun_bvars valfun_delayed in
  let body = subst_defs (var_delayed, datacon_env, datatype_env, valfun_env) body in
  if not (Datacon.Subst.is_id datacon_delayed && Datatype.Subst.is_id datatype_delayed && Valfun.Subst.is_id valfun_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.defs_delayed <- (Datacon.Subst.id, Datatype.Subst.id, Valfun.Subst.id, Var.Subst.id);
    abstraction.defs <- body
  end;
  body

and open2_defs : opaque_defs -> opaque_defs -> defs * defs = fun x1 x2 -> 
  change_invalid_to_open2 open2i_defs x1 x2

and open2i_defs : opaque_defs -> opaque_defs -> defs * defs = fun abstraction1 abstraction2 ->
  let (datacon_delayed1, datatype_delayed1, valfun_delayed1, var_delayed1) = abstraction1.defs_delayed in
  let body1 = abstraction1.defs in
  let (datacon_delayed2, datatype_delayed2, valfun_delayed2, var_delayed2) = abstraction2.defs_delayed in
  let body2 = abstraction2.defs in
  let (datacon_env1, datatype_env1, valfun_env1, datacon_env2, datatype_env2, valfun_env2) = freshen2_defs (Datacon.Subst.id, Datatype.Subst.id, Valfun.Subst.id, Datacon.Subst.id, Datatype.Subst.id, Valfun.Subst.id) body1 body2 in
  let datacon_env1 = Datacon.Subst.union datacon_delayed1 datacon_env1 in
  let datatype_env1 = Datatype.Subst.union datatype_delayed1 datatype_env1 in
  let valfun_env1 = Valfun.Subst.union valfun_delayed1 valfun_env1 in
  let datacon_env2 = Datacon.Subst.union datacon_delayed2 datacon_env2 in
  let datatype_env2 = Datatype.Subst.union datatype_delayed2 datatype_env2 in
  let valfun_env2 = Valfun.Subst.union valfun_delayed2 valfun_env2 in
  let body1 = subst_defs (var_delayed1, datacon_env1, datatype_env1, valfun_env1) body1 in
  let body2 = subst_defs (var_delayed2, datacon_env2, datatype_env2, valfun_env2) body2 in
  if not (Datacon.Subst.is_id datacon_delayed1 && Datatype.Subst.is_id datatype_delayed1 && Valfun.Subst.is_id valfun_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.defs_delayed <- (Datacon.Subst.id, Datatype.Subst.id, Valfun.Subst.id, Var.Subst.id);
    abstraction1.defs <- body1
  end;
  if not (Datacon.Subst.is_id datacon_delayed2 && Datatype.Subst.is_id datatype_delayed2 && Valfun.Subst.is_id valfun_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.defs_delayed <- (Datacon.Subst.id, Datatype.Subst.id, Valfun.Subst.id, Var.Subst.id);
    abstraction2.defs <- body2
  end;
  body1, body2

and apply_defs = 
  fun (datacon_env, datatype_env, valfun_env, var_env) abstraction ->
    let (datacon_delayed, datatype_delayed, valfun_delayed, var_delayed) = abstraction.defs_delayed in {
      abstraction with defs_delayed = (Datacon.Subst.compose datacon_env datacon_delayed, Datatype.Subst.compose datatype_env datatype_delayed, Valfun.Subst.compose valfun_env valfun_delayed, Var.Subst.compose var_env var_delayed)
    }

and import_program : datacon Identifier.Map.t * datatype Identifier.Map.t * valfun Identifier.Map.t * var Identifier.Map.t -> Raw.program -> program = fun (datacon_env, datatype_env, valfun_env, var_env) -> function
  (_defs0) ->
      let (datacon_bvars, datatype_bvars, valfun_bvars) = bvi_defs _defs0 in 
      let datacon_ienv = Datacon.Atom.mfreshb datacon_bvars datacon_env in
      let datatype_ienv = Datatype.Atom.mfreshb datatype_bvars datatype_env in
      let valfun_ienv = Valfun.Atom.mfreshb valfun_bvars valfun_env in
      let _defs0 = import_defs (var_env, datacon_ienv, datatype_ienv, valfun_ienv) _defs0 in
    (create_defs _defs0)

and subst_program : Datacon.Subst.t * Datatype.Subst.t * Valfun.Subst.t * Var.Subst.t -> program -> program = fun (datacon_env, datatype_env, valfun_env, var_env) -> function
  (_defs0) ->
    (apply_defs (datacon_env, datatype_env, valfun_env, var_env) _defs0)

and export_program : Datacon.AtomIdMap.t * Datatype.AtomIdMap.t * Valfun.AtomIdMap.t * Var.AtomIdMap.t -> program -> Raw.program = fun (datacon_m, datatype_m, valfun_m, var_m) -> function
  (_defs0) ->
      let defs = open_defs _defs0 in
      let (datacon_bvars, datatype_bvars, valfun_bvars) = bound_defs defs in
      let datacon_im = Datacon.AtomIdMap.add_set datacon_bvars datacon_m in
      let datatype_im = Datatype.AtomIdMap.add_set datatype_bvars datatype_m in
      let valfun_im = Valfun.AtomIdMap.add_set valfun_bvars valfun_m in
    (export_defs (var_m, datacon_im, datatype_im, valfun_im) defs)

and flatten_program : program -> Flat.program = function
  (_defs0) ->
      let defs = open_defs _defs0 in
    (flatten_defs defs)

and unflatten_program : Flat.program -> program = function
  (_defs0) ->
      let defs = unflatten_defs _defs0 in
    (create_defs defs)

and free_program : program -> Datacon.AtomSet.t * Datatype.AtomSet.t * Valfun.AtomSet.t * Var.AtomSet.t = 
  function program -> free_accu_program (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Valfun.AtomSet.empty, Var.AtomSet.empty) program

and equal_program : program -> program -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_program x1 x2

and aeq_program : unit -> program -> program -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_defs0), (_defs1) ->
      let _defs0, _defs1 = open2i_defs _defs0 _defs1 in
      aeq_defs () _defs0 _defs1;
      ()

and free_accu_program = fun (datacon_fvars, datatype_fvars, valfun_fvars, var_fvars) -> function
  (_defs0) ->
      let defs = open_defs _defs0 in
      let (datacon_bvars, datatype_bvars, valfun_bvars, datacon_ifvars, datatype_ifvars, valfun_ifvars, var_fvars) = bound_free_accu_defs (Datacon.AtomSet.empty, Datatype.AtomSet.empty, Valfun.AtomSet.empty, Datacon.AtomSet.empty, Datatype.AtomSet.empty, Valfun.AtomSet.empty, var_fvars) defs in
      let datacon_fvars = Datacon.AtomSet.union datacon_fvars (Datacon.AtomSet.diff datacon_ifvars datacon_bvars) in
      let datatype_fvars = Datatype.AtomSet.union datatype_fvars (Datatype.AtomSet.diff datatype_ifvars datatype_bvars) in
      let valfun_fvars = Valfun.AtomSet.union valfun_fvars (Valfun.AtomSet.diff valfun_ifvars valfun_bvars) in
      (datacon_fvars, datatype_fvars, valfun_fvars, var_fvars)

and subst_ofr : Datacon.Subst.t * Valfun.Subst.t * Var.Subst.t -> ofr -> ofr = fun (datacon_oenv, valfun_oenv, var_ienv) -> function
  (_monos1, _lexp0) ->
    (List.map (subst_mono (var_ienv)) _monos1, (subst_lexp (datacon_oenv, valfun_oenv, var_ienv)) _lexp0)

and bound_ofr : ofr -> Var.AtomSet.t = 
  function ofr -> bound_accu_ofr (Var.AtomSet.empty) ofr

and bound_free_ofr : ofr -> Var.AtomSet.t * Var.AtomSet.t * Datacon.AtomSet.t * Valfun.AtomSet.t = 
  function ofr -> bound_free_accu_ofr (Var.AtomSet.empty, Var.AtomSet.empty, Datacon.AtomSet.empty, Valfun.AtomSet.empty) ofr

and equal_ofr : ofr -> ofr -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_ofr x1 x2

and import_ofr = fun (datacon_oenv, valfun_oenv, var_ienv) -> function
  (_monos1, _lexp0) ->
    (List.map (import_mono (var_ienv)) _monos1, (import_lexp (datacon_oenv, valfun_oenv, var_ienv)) _lexp0)

and bvi_accu_ofr = fun (var_bvars) -> function
  (_monos1, _lexp0) ->
      let (var_bvars) = List.fold_left bvi_accu_mono (var_bvars) _monos1 in
      (var_bvars)

and bvi_ofr = 
  function ofr -> bvi_accu_ofr (Identifier.Map.empty) ofr

and bound_accu_ofr = fun (var_bvars) -> function
  (_monos1, _lexp0) ->
      let (var_bvars) = List.fold_left bound_accu_mono (var_bvars) _monos1 in
      (var_bvars)

and export_ofr : Datacon.AtomIdMap.t * Valfun.AtomIdMap.t * Var.AtomIdMap.t -> ofr -> Raw.ofr = fun (datacon_om, valfun_om, var_im) -> function
  (_monos1, _lexp0) ->
    (List.map (export_mono (var_im)) _monos1, (export_lexp (datacon_om, valfun_om, var_im)) _lexp0)

and flatten_ofr : ofr -> Flat.ofr = function
  (_monos1, _lexp0) ->
    (List.map flatten_mono _monos1, flatten_lexp _lexp0)

and unflatten_ofr : Flat.ofr -> ofr = function
  (_monos1, _lexp0) ->
    (List.map unflatten_mono _monos1, unflatten_lexp _lexp0)

and bound_free_accu_ofr = fun (var_bvars, var_ifvars, datacon_ofvars, valfun_ofvars) -> function
  (_monos1, _lexp0) ->
      let (var_bvars) = List.fold_left bound_free_accu_mono (var_bvars) _monos1 in
      let (datacon_ofvars, valfun_ofvars, var_ifvars) = free_accu_lexp (datacon_ofvars, valfun_ofvars, var_ifvars) _lexp0 in
      (var_bvars, var_ifvars, datacon_ofvars, valfun_ofvars)

and aeq_ofr : unit -> ofr -> ofr -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_monos1, _lexp0), (_monos3, _lexp2) ->
      List.fold_left2 aeq_mono () _monos1 _monos3;
      aeq_lexp () _lexp0 _lexp2;
      ()

and freshen2_ofr : Var.Subst.t * Var.Subst.t -> ofr -> ofr -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_monos1, _lexp0), (_monos3, _lexp2) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_mono (var_env1, var_env2) _monos1 _monos3 in
      (var_env1, var_env2)

and create_ofr : ofr -> opaque_ofr = 
  function body -> {
    ofr_delayed = (Datacon.Subst.id, Valfun.Subst.id, Var.Subst.id);
    ofr = body
  }

and open_ofr : opaque_ofr -> ofr = function abstraction ->
  let (datacon_delayed, valfun_delayed, var_delayed) = abstraction.ofr_delayed in
  let body = abstraction.ofr in
  let (var_bvars) = bound_ofr body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_ofr (datacon_delayed, valfun_delayed, var_env) body in
  if not (Datacon.Subst.is_id datacon_delayed && Valfun.Subst.is_id valfun_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.ofr_delayed <- (Datacon.Subst.id, Valfun.Subst.id, Var.Subst.id);
    abstraction.ofr <- body
  end;
  body

and open2_ofr : opaque_ofr -> opaque_ofr -> ofr * ofr = fun x1 x2 -> 
  change_invalid_to_open2 open2i_ofr x1 x2

and open2i_ofr : opaque_ofr -> opaque_ofr -> ofr * ofr = fun abstraction1 abstraction2 ->
  let (datacon_delayed1, valfun_delayed1, var_delayed1) = abstraction1.ofr_delayed in
  let body1 = abstraction1.ofr in
  let (datacon_delayed2, valfun_delayed2, var_delayed2) = abstraction2.ofr_delayed in
  let body2 = abstraction2.ofr in
  let (var_env1, var_env2) = freshen2_ofr (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_ofr (datacon_delayed1, valfun_delayed1, var_env1) body1 in
  let body2 = subst_ofr (datacon_delayed2, valfun_delayed2, var_env2) body2 in
  if not (Datacon.Subst.is_id datacon_delayed1 && Valfun.Subst.is_id valfun_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.ofr_delayed <- (Datacon.Subst.id, Valfun.Subst.id, Var.Subst.id);
    abstraction1.ofr <- body1
  end;
  if not (Datacon.Subst.is_id datacon_delayed2 && Valfun.Subst.is_id valfun_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.ofr_delayed <- (Datacon.Subst.id, Valfun.Subst.id, Var.Subst.id);
    abstraction2.ofr <- body2
  end;
  body1, body2

and apply_ofr = 
  fun (datacon_env, valfun_env, var_env) abstraction ->
    let (datacon_delayed, valfun_delayed, var_delayed) = abstraction.ofr_delayed in {
      abstraction with ofr_delayed = (Datacon.Subst.compose datacon_env datacon_delayed, Valfun.Subst.compose valfun_env valfun_delayed, Var.Subst.compose var_env var_delayed)
    }

and subst_olm : Datacon.Subst.t * Valfun.Subst.t * Var.Subst.t -> olm -> olm = fun (datacon_oenv, valfun_oenv, var_ienv) -> function
  (_monos2, _contrainte1, _lexp0) ->
    (List.map (subst_mono (var_ienv)) _monos2, (subst_contrainte (var_ienv)) _contrainte1, (subst_lexp (datacon_oenv, valfun_oenv, var_ienv)) _lexp0)

and bound_olm : olm -> Var.AtomSet.t = 
  function olm -> bound_accu_olm (Var.AtomSet.empty) olm

and bound_free_olm : olm -> Var.AtomSet.t * Var.AtomSet.t * Datacon.AtomSet.t * Valfun.AtomSet.t = 
  function olm -> bound_free_accu_olm (Var.AtomSet.empty, Var.AtomSet.empty, Datacon.AtomSet.empty, Valfun.AtomSet.empty) olm

and equal_olm : olm -> olm -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_olm x1 x2

and import_olm = fun (datacon_oenv, valfun_oenv, var_ienv) -> function
  (_monos2, _contrainte1, _lexp0) ->
    (List.map (import_mono (var_ienv)) _monos2, (import_contrainte (var_ienv)) _contrainte1, (import_lexp (datacon_oenv, valfun_oenv, var_ienv)) _lexp0)

and bvi_accu_olm = fun (var_bvars) -> function
  (_monos2, _contrainte1, _lexp0) ->
      let (var_bvars) = List.fold_left bvi_accu_mono (var_bvars) _monos2 in
      (var_bvars)

and bvi_olm = 
  function olm -> bvi_accu_olm (Identifier.Map.empty) olm

and bound_accu_olm = fun (var_bvars) -> function
  (_monos2, _contrainte1, _lexp0) ->
      let (var_bvars) = List.fold_left bound_accu_mono (var_bvars) _monos2 in
      (var_bvars)

and export_olm : Datacon.AtomIdMap.t * Valfun.AtomIdMap.t * Var.AtomIdMap.t -> olm -> Raw.olm = fun (datacon_om, valfun_om, var_im) -> function
  (_monos2, _contrainte1, _lexp0) ->
    (List.map (export_mono (var_im)) _monos2, (export_contrainte (var_im)) _contrainte1, (export_lexp (datacon_om, valfun_om, var_im)) _lexp0)

and flatten_olm : olm -> Flat.olm = function
  (_monos2, _contrainte1, _lexp0) ->
    (List.map flatten_mono _monos2, flatten_contrainte _contrainte1, flatten_lexp _lexp0)

and unflatten_olm : Flat.olm -> olm = function
  (_monos2, _contrainte1, _lexp0) ->
    (List.map unflatten_mono _monos2, unflatten_contrainte _contrainte1, unflatten_lexp _lexp0)

and bound_free_accu_olm = fun (var_bvars, var_ifvars, datacon_ofvars, valfun_ofvars) -> function
  (_monos2, _contrainte1, _lexp0) ->
      let (var_bvars) = List.fold_left bound_free_accu_mono (var_bvars) _monos2 in
      let (var_ifvars) = free_accu_contrainte (var_ifvars) _contrainte1 in
      let (datacon_ofvars, valfun_ofvars, var_ifvars) = free_accu_lexp (datacon_ofvars, valfun_ofvars, var_ifvars) _lexp0 in
      (var_bvars, var_ifvars, datacon_ofvars, valfun_ofvars)

and aeq_olm : unit -> olm -> olm -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_monos2, _contrainte1, _lexp0), (_monos5, _contrainte4, _lexp3) ->
      List.fold_left2 aeq_mono () _monos2 _monos5;
      aeq_contrainte () _contrainte1 _contrainte4;
      aeq_lexp () _lexp0 _lexp3;
      ()

and freshen2_olm : Var.Subst.t * Var.Subst.t -> olm -> olm -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_monos2, _contrainte1, _lexp0), (_monos5, _contrainte4, _lexp3) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_mono (var_env1, var_env2) _monos2 _monos5 in
      (var_env1, var_env2)

and create_olm : olm -> opaque_olm = 
  function body -> {
    olm_delayed = (Datacon.Subst.id, Valfun.Subst.id, Var.Subst.id);
    olm = body
  }

and open_olm : opaque_olm -> olm = function abstraction ->
  let (datacon_delayed, valfun_delayed, var_delayed) = abstraction.olm_delayed in
  let body = abstraction.olm in
  let (var_bvars) = bound_olm body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_olm (datacon_delayed, valfun_delayed, var_env) body in
  if not (Datacon.Subst.is_id datacon_delayed && Valfun.Subst.is_id valfun_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.olm_delayed <- (Datacon.Subst.id, Valfun.Subst.id, Var.Subst.id);
    abstraction.olm <- body
  end;
  body

and open2_olm : opaque_olm -> opaque_olm -> olm * olm = fun x1 x2 -> 
  change_invalid_to_open2 open2i_olm x1 x2

and open2i_olm : opaque_olm -> opaque_olm -> olm * olm = fun abstraction1 abstraction2 ->
  let (datacon_delayed1, valfun_delayed1, var_delayed1) = abstraction1.olm_delayed in
  let body1 = abstraction1.olm in
  let (datacon_delayed2, valfun_delayed2, var_delayed2) = abstraction2.olm_delayed in
  let body2 = abstraction2.olm in
  let (var_env1, var_env2) = freshen2_olm (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_olm (datacon_delayed1, valfun_delayed1, var_env1) body1 in
  let body2 = subst_olm (datacon_delayed2, valfun_delayed2, var_env2) body2 in
  if not (Datacon.Subst.is_id datacon_delayed1 && Valfun.Subst.is_id valfun_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.olm_delayed <- (Datacon.Subst.id, Valfun.Subst.id, Var.Subst.id);
    abstraction1.olm <- body1
  end;
  if not (Datacon.Subst.is_id datacon_delayed2 && Valfun.Subst.is_id valfun_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.olm_delayed <- (Datacon.Subst.id, Valfun.Subst.id, Var.Subst.id);
    abstraction2.olm <- body2
  end;
  body1, body2

and apply_olm = 
  fun (datacon_env, valfun_env, var_env) abstraction ->
    let (datacon_delayed, valfun_delayed, var_delayed) = abstraction.olm_delayed in {
      abstraction with olm_delayed = (Datacon.Subst.compose datacon_env datacon_delayed, Valfun.Subst.compose valfun_env valfun_delayed, Var.Subst.compose var_env var_delayed)
    }

and subst_obr : Datacon.Subst.t * Valfun.Subst.t * Var.Subst.t -> obr -> obr = fun (datacon_oenv, valfun_oenv, var_ienv) -> function
  (_patterns1, _lexp0) ->
    (List.map (subst_pattern (datacon_oenv, var_ienv)) _patterns1, (subst_lexp (datacon_oenv, valfun_oenv, var_ienv)) _lexp0)

and bound_obr : obr -> Var.AtomSet.t = 
  function obr -> bound_accu_obr (Var.AtomSet.empty) obr

and bound_free_obr : obr -> Var.AtomSet.t * Var.AtomSet.t * Datacon.AtomSet.t * Valfun.AtomSet.t = 
  function obr -> bound_free_accu_obr (Var.AtomSet.empty, Var.AtomSet.empty, Datacon.AtomSet.empty, Valfun.AtomSet.empty) obr

and equal_obr : obr -> obr -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_obr x1 x2

and import_obr = fun (datacon_oenv, valfun_oenv, var_ienv) -> function
  (_patterns1, _lexp0) ->
    (List.map (import_pattern (datacon_oenv, var_ienv)) _patterns1, (import_lexp (datacon_oenv, valfun_oenv, var_ienv)) _lexp0)

and bvi_accu_obr = fun (var_bvars) -> function
  (_patterns1, _lexp0) ->
      let (var_bvars) = List.fold_left bvi_accu_pattern (var_bvars) _patterns1 in
      (var_bvars)

and bvi_obr = 
  function obr -> bvi_accu_obr (Identifier.Map.empty) obr

and bound_accu_obr = fun (var_bvars) -> function
  (_patterns1, _lexp0) ->
      let (var_bvars) = List.fold_left bound_accu_pattern (var_bvars) _patterns1 in
      (var_bvars)

and export_obr : Datacon.AtomIdMap.t * Valfun.AtomIdMap.t * Var.AtomIdMap.t -> obr -> Raw.obr = fun (datacon_om, valfun_om, var_im) -> function
  (_patterns1, _lexp0) ->
    (List.map (export_pattern (datacon_om, var_im)) _patterns1, (export_lexp (datacon_om, valfun_om, var_im)) _lexp0)

and flatten_obr : obr -> Flat.obr = function
  (_patterns1, _lexp0) ->
    (List.map flatten_pattern _patterns1, flatten_lexp _lexp0)

and unflatten_obr : Flat.obr -> obr = function
  (_patterns1, _lexp0) ->
    (List.map unflatten_pattern _patterns1, unflatten_lexp _lexp0)

and bound_free_accu_obr = fun (var_bvars, var_ifvars, datacon_ofvars, valfun_ofvars) -> function
  (_patterns1, _lexp0) ->
      let (var_bvars, datacon_ofvars) = List.fold_left bound_free_accu_pattern (var_bvars, datacon_ofvars) _patterns1 in
      let (datacon_ofvars, valfun_ofvars, var_ifvars) = free_accu_lexp (datacon_ofvars, valfun_ofvars, var_ifvars) _lexp0 in
      (var_bvars, var_ifvars, datacon_ofvars, valfun_ofvars)

and aeq_obr : unit -> obr -> obr -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_patterns1, _lexp0), (_patterns3, _lexp2) ->
      List.fold_left2 aeq_pattern () _patterns1 _patterns3;
      aeq_lexp () _lexp0 _lexp2;
      ()

and freshen2_obr : Var.Subst.t * Var.Subst.t -> obr -> obr -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_patterns1, _lexp0), (_patterns3, _lexp2) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_pattern (var_env1, var_env2) _patterns1 _patterns3 in
      (var_env1, var_env2)

and create_obr : obr -> opaque_obr = 
  function body -> {
    obr_delayed = (Datacon.Subst.id, Valfun.Subst.id, Var.Subst.id);
    obr = body
  }

and open_obr : opaque_obr -> obr = function abstraction ->
  let (datacon_delayed, valfun_delayed, var_delayed) = abstraction.obr_delayed in
  let body = abstraction.obr in
  let (var_bvars) = bound_obr body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_obr (datacon_delayed, valfun_delayed, var_env) body in
  if not (Datacon.Subst.is_id datacon_delayed && Valfun.Subst.is_id valfun_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.obr_delayed <- (Datacon.Subst.id, Valfun.Subst.id, Var.Subst.id);
    abstraction.obr <- body
  end;
  body

and open2_obr : opaque_obr -> opaque_obr -> obr * obr = fun x1 x2 -> 
  change_invalid_to_open2 open2i_obr x1 x2

and open2i_obr : opaque_obr -> opaque_obr -> obr * obr = fun abstraction1 abstraction2 ->
  let (datacon_delayed1, valfun_delayed1, var_delayed1) = abstraction1.obr_delayed in
  let body1 = abstraction1.obr in
  let (datacon_delayed2, valfun_delayed2, var_delayed2) = abstraction2.obr_delayed in
  let body2 = abstraction2.obr in
  let (var_env1, var_env2) = freshen2_obr (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_obr (datacon_delayed1, valfun_delayed1, var_env1) body1 in
  let body2 = subst_obr (datacon_delayed2, valfun_delayed2, var_env2) body2 in
  if not (Datacon.Subst.is_id datacon_delayed1 && Valfun.Subst.is_id valfun_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.obr_delayed <- (Datacon.Subst.id, Valfun.Subst.id, Var.Subst.id);
    abstraction1.obr <- body1
  end;
  if not (Datacon.Subst.is_id datacon_delayed2 && Valfun.Subst.is_id valfun_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.obr_delayed <- (Datacon.Subst.id, Valfun.Subst.id, Var.Subst.id);
    abstraction2.obr <- body2
  end;
  body1, body2

and apply_obr = 
  fun (datacon_env, valfun_env, var_env) abstraction ->
    let (datacon_delayed, valfun_delayed, var_delayed) = abstraction.obr_delayed in {
      abstraction with obr_delayed = (Datacon.Subst.compose datacon_env datacon_delayed, Valfun.Subst.compose valfun_env valfun_delayed, Var.Subst.compose var_env var_delayed)
    }

and subst_ofu : Datacon.Subst.t * Valfun.Subst.t * Var.Subst.t -> ofu -> ofu = fun (datacon_oenv, valfun_oenv, var_ienv) -> function
  (_monos3, _contrainte2, _postcondition1, _lexp0) ->
    (List.map (subst_mono (var_ienv)) _monos3, (subst_contrainte (var_ienv)) _contrainte2, (subst_postcondition (var_ienv)) _postcondition1, (subst_lexp (datacon_oenv, valfun_oenv, var_ienv)) _lexp0)

and bound_ofu : ofu -> Var.AtomSet.t = 
  function ofu -> bound_accu_ofu (Var.AtomSet.empty) ofu

and bound_free_ofu : ofu -> Var.AtomSet.t * Var.AtomSet.t * Datacon.AtomSet.t * Valfun.AtomSet.t = 
  function ofu -> bound_free_accu_ofu (Var.AtomSet.empty, Var.AtomSet.empty, Datacon.AtomSet.empty, Valfun.AtomSet.empty) ofu

and equal_ofu : ofu -> ofu -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_ofu x1 x2

and import_ofu = fun (datacon_oenv, valfun_oenv, var_ienv) -> function
  (_monos3, _contrainte2, _postcondition1, _lexp0) ->
    (List.map (import_mono (var_ienv)) _monos3, (import_contrainte (var_ienv)) _contrainte2, (import_postcondition (var_ienv)) _postcondition1, (import_lexp (datacon_oenv, valfun_oenv, var_ienv)) _lexp0)

and bvi_accu_ofu = fun (var_bvars) -> function
  (_monos3, _contrainte2, _postcondition1, _lexp0) ->
      let (var_bvars) = List.fold_left bvi_accu_mono (var_bvars) _monos3 in
      (var_bvars)

and bvi_ofu = 
  function ofu -> bvi_accu_ofu (Identifier.Map.empty) ofu

and bound_accu_ofu = fun (var_bvars) -> function
  (_monos3, _contrainte2, _postcondition1, _lexp0) ->
      let (var_bvars) = List.fold_left bound_accu_mono (var_bvars) _monos3 in
      (var_bvars)

and export_ofu : Datacon.AtomIdMap.t * Valfun.AtomIdMap.t * Var.AtomIdMap.t -> ofu -> Raw.ofu = fun (datacon_om, valfun_om, var_im) -> function
  (_monos3, _contrainte2, _postcondition1, _lexp0) ->
    (List.map (export_mono (var_im)) _monos3, (export_contrainte (var_im)) _contrainte2, (export_postcondition (var_im)) _postcondition1, (export_lexp (datacon_om, valfun_om, var_im)) _lexp0)

and flatten_ofu : ofu -> Flat.ofu = function
  (_monos3, _contrainte2, _postcondition1, _lexp0) ->
    (List.map flatten_mono _monos3, flatten_contrainte _contrainte2, flatten_postcondition _postcondition1, flatten_lexp _lexp0)

and unflatten_ofu : Flat.ofu -> ofu = function
  (_monos3, _contrainte2, _postcondition1, _lexp0) ->
    (List.map unflatten_mono _monos3, unflatten_contrainte _contrainte2, unflatten_postcondition _postcondition1, unflatten_lexp _lexp0)

and bound_free_accu_ofu = fun (var_bvars, var_ifvars, datacon_ofvars, valfun_ofvars) -> function
  (_monos3, _contrainte2, _postcondition1, _lexp0) ->
      let (var_bvars) = List.fold_left bound_free_accu_mono (var_bvars) _monos3 in
      let (var_ifvars) = free_accu_contrainte (var_ifvars) _contrainte2 in
      let (var_ifvars) = free_accu_postcondition (var_ifvars) _postcondition1 in
      let (datacon_ofvars, valfun_ofvars, var_ifvars) = free_accu_lexp (datacon_ofvars, valfun_ofvars, var_ifvars) _lexp0 in
      (var_bvars, var_ifvars, datacon_ofvars, valfun_ofvars)

and aeq_ofu : unit -> ofu -> ofu -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_monos3, _contrainte2, _postcondition1, _lexp0), (_monos7, _contrainte6, _postcondition5, _lexp4) ->
      List.fold_left2 aeq_mono () _monos3 _monos7;
      aeq_contrainte () _contrainte2 _contrainte6;
      aeq_postcondition () _postcondition1 _postcondition5;
      aeq_lexp () _lexp0 _lexp4;
      ()

and freshen2_ofu : Var.Subst.t * Var.Subst.t -> ofu -> ofu -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_monos3, _contrainte2, _postcondition1, _lexp0), (_monos7, _contrainte6, _postcondition5, _lexp4) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_mono (var_env1, var_env2) _monos3 _monos7 in
      (var_env1, var_env2)

and create_ofu : ofu -> opaque_ofu = 
  function body -> {
    ofu_delayed = (Datacon.Subst.id, Valfun.Subst.id, Var.Subst.id);
    ofu = body
  }

and open_ofu : opaque_ofu -> ofu = function abstraction ->
  let (datacon_delayed, valfun_delayed, var_delayed) = abstraction.ofu_delayed in
  let body = abstraction.ofu in
  let (var_bvars) = bound_ofu body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_ofu (datacon_delayed, valfun_delayed, var_env) body in
  if not (Datacon.Subst.is_id datacon_delayed && Valfun.Subst.is_id valfun_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.ofu_delayed <- (Datacon.Subst.id, Valfun.Subst.id, Var.Subst.id);
    abstraction.ofu <- body
  end;
  body

and open2_ofu : opaque_ofu -> opaque_ofu -> ofu * ofu = fun x1 x2 -> 
  change_invalid_to_open2 open2i_ofu x1 x2

and open2i_ofu : opaque_ofu -> opaque_ofu -> ofu * ofu = fun abstraction1 abstraction2 ->
  let (datacon_delayed1, valfun_delayed1, var_delayed1) = abstraction1.ofu_delayed in
  let body1 = abstraction1.ofu in
  let (datacon_delayed2, valfun_delayed2, var_delayed2) = abstraction2.ofu_delayed in
  let body2 = abstraction2.ofu in
  let (var_env1, var_env2) = freshen2_ofu (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_ofu (datacon_delayed1, valfun_delayed1, var_env1) body1 in
  let body2 = subst_ofu (datacon_delayed2, valfun_delayed2, var_env2) body2 in
  if not (Datacon.Subst.is_id datacon_delayed1 && Valfun.Subst.is_id valfun_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.ofu_delayed <- (Datacon.Subst.id, Valfun.Subst.id, Var.Subst.id);
    abstraction1.ofu <- body1
  end;
  if not (Datacon.Subst.is_id datacon_delayed2 && Valfun.Subst.is_id valfun_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.ofu_delayed <- (Datacon.Subst.id, Valfun.Subst.id, Var.Subst.id);
    abstraction2.ofu <- body2
  end;
  body1, body2

and apply_ofu = 
  fun (datacon_env, valfun_env, var_env) abstraction ->
    let (datacon_delayed, valfun_delayed, var_delayed) = abstraction.ofu_delayed in {
      abstraction with ofu_delayed = (Datacon.Subst.compose datacon_env datacon_delayed, Valfun.Subst.compose valfun_env valfun_delayed, Var.Subst.compose var_env var_delayed)
    }

and subst_osp : Var.Subst.t -> osp -> osp = fun (var_ienv) -> function
  (_monos2, _contrainte1, _postcondition0) ->
    (List.map (subst_mono (var_ienv)) _monos2, (subst_contrainte (var_ienv)) _contrainte1, (subst_postcondition (var_ienv)) _postcondition0)

and bound_osp : osp -> Var.AtomSet.t = 
  function osp -> bound_accu_osp (Var.AtomSet.empty) osp

and bound_free_osp : osp -> Var.AtomSet.t * Var.AtomSet.t = 
  function osp -> bound_free_accu_osp (Var.AtomSet.empty, Var.AtomSet.empty) osp

and equal_osp : osp -> osp -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_osp x1 x2

and import_osp = fun (var_ienv) -> function
  (_monos2, _contrainte1, _postcondition0) ->
    (List.map (import_mono (var_ienv)) _monos2, (import_contrainte (var_ienv)) _contrainte1, (import_postcondition (var_ienv)) _postcondition0)

and bvi_accu_osp = fun (var_bvars) -> function
  (_monos2, _contrainte1, _postcondition0) ->
      let (var_bvars) = List.fold_left bvi_accu_mono (var_bvars) _monos2 in
      (var_bvars)

and bvi_osp = 
  function osp -> bvi_accu_osp (Identifier.Map.empty) osp

and bound_accu_osp = fun (var_bvars) -> function
  (_monos2, _contrainte1, _postcondition0) ->
      let (var_bvars) = List.fold_left bound_accu_mono (var_bvars) _monos2 in
      (var_bvars)

and export_osp : Var.AtomIdMap.t -> osp -> Raw.osp = fun (var_im) -> function
  (_monos2, _contrainte1, _postcondition0) ->
    (List.map (export_mono (var_im)) _monos2, (export_contrainte (var_im)) _contrainte1, (export_postcondition (var_im)) _postcondition0)

and flatten_osp : osp -> Flat.osp = function
  (_monos2, _contrainte1, _postcondition0) ->
    (List.map flatten_mono _monos2, flatten_contrainte _contrainte1, flatten_postcondition _postcondition0)

and unflatten_osp : Flat.osp -> osp = function
  (_monos2, _contrainte1, _postcondition0) ->
    (List.map unflatten_mono _monos2, unflatten_contrainte _contrainte1, unflatten_postcondition _postcondition0)

and bound_free_accu_osp = fun (var_bvars, var_ifvars) -> function
  (_monos2, _contrainte1, _postcondition0) ->
      let (var_bvars) = List.fold_left bound_free_accu_mono (var_bvars) _monos2 in
      let (var_ifvars) = free_accu_contrainte (var_ifvars) _contrainte1 in
      let (var_ifvars) = free_accu_postcondition (var_ifvars) _postcondition0 in
      (var_bvars, var_ifvars)

and aeq_osp : unit -> osp -> osp -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_monos2, _contrainte1, _postcondition0), (_monos5, _contrainte4, _postcondition3) ->
      List.fold_left2 aeq_mono () _monos2 _monos5;
      aeq_contrainte () _contrainte1 _contrainte4;
      aeq_postcondition () _postcondition0 _postcondition3;
      ()

and freshen2_osp : Var.Subst.t * Var.Subst.t -> osp -> osp -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_monos2, _contrainte1, _postcondition0), (_monos5, _contrainte4, _postcondition3) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_mono (var_env1, var_env2) _monos2 _monos5 in
      (var_env1, var_env2)

and create_osp : osp -> opaque_osp = 
  function body -> {
    osp_delayed = (Var.Subst.id);
    osp = body
  }

and open_osp : opaque_osp -> osp = function abstraction ->
  let (var_delayed) = abstraction.osp_delayed in
  let body = abstraction.osp in
  let (var_bvars) = bound_osp body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_osp (var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.osp_delayed <- (Var.Subst.id);
    abstraction.osp <- body
  end;
  body

and open2_osp : opaque_osp -> opaque_osp -> osp * osp = fun x1 x2 -> 
  change_invalid_to_open2 open2i_osp x1 x2

and open2i_osp : opaque_osp -> opaque_osp -> osp * osp = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.osp_delayed in
  let body1 = abstraction1.osp in
  let (var_delayed2) = abstraction2.osp_delayed in
  let body2 = abstraction2.osp in
  let (var_env1, var_env2) = freshen2_osp (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_osp (var_env1) body1 in
  let body2 = subst_osp (var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.osp_delayed <- (Var.Subst.id);
    abstraction1.osp <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.osp_delayed <- (Var.Subst.id);
    abstraction2.osp <- body2
  end;
  body1, body2

and apply_osp = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.osp_delayed in {
      abstraction with osp_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_opc : Var.Subst.t -> opc -> opc = fun (var_ienv) -> function
  (_monos1, _contrainte0) ->
    (List.map (subst_mono (var_ienv)) _monos1, (subst_contrainte (var_ienv)) _contrainte0)

and bound_opc : opc -> Var.AtomSet.t = 
  function opc -> bound_accu_opc (Var.AtomSet.empty) opc

and bound_free_opc : opc -> Var.AtomSet.t * Var.AtomSet.t = 
  function opc -> bound_free_accu_opc (Var.AtomSet.empty, Var.AtomSet.empty) opc

and equal_opc : opc -> opc -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_opc x1 x2

and import_opc = fun (var_ienv) -> function
  (_monos1, _contrainte0) ->
    (List.map (import_mono (var_ienv)) _monos1, (import_contrainte (var_ienv)) _contrainte0)

and bvi_accu_opc = fun (var_bvars) -> function
  (_monos1, _contrainte0) ->
      let (var_bvars) = List.fold_left bvi_accu_mono (var_bvars) _monos1 in
      (var_bvars)

and bvi_opc = 
  function opc -> bvi_accu_opc (Identifier.Map.empty) opc

and bound_accu_opc = fun (var_bvars) -> function
  (_monos1, _contrainte0) ->
      let (var_bvars) = List.fold_left bound_accu_mono (var_bvars) _monos1 in
      (var_bvars)

and export_opc : Var.AtomIdMap.t -> opc -> Raw.opc = fun (var_im) -> function
  (_monos1, _contrainte0) ->
    (List.map (export_mono (var_im)) _monos1, (export_contrainte (var_im)) _contrainte0)

and flatten_opc : opc -> Flat.opc = function
  (_monos1, _contrainte0) ->
    (List.map flatten_mono _monos1, flatten_contrainte _contrainte0)

and unflatten_opc : Flat.opc -> opc = function
  (_monos1, _contrainte0) ->
    (List.map unflatten_mono _monos1, unflatten_contrainte _contrainte0)

and bound_free_accu_opc = fun (var_bvars, var_ifvars) -> function
  (_monos1, _contrainte0) ->
      let (var_bvars) = List.fold_left bound_free_accu_mono (var_bvars) _monos1 in
      let (var_ifvars) = free_accu_contrainte (var_ifvars) _contrainte0 in
      (var_bvars, var_ifvars)

and aeq_opc : unit -> opc -> opc -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_monos1, _contrainte0), (_monos3, _contrainte2) ->
      List.fold_left2 aeq_mono () _monos1 _monos3;
      aeq_contrainte () _contrainte0 _contrainte2;
      ()

and freshen2_opc : Var.Subst.t * Var.Subst.t -> opc -> opc -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_monos1, _contrainte0), (_monos3, _contrainte2) ->
      let (var_env1, var_env2) = List.fold_left2 freshen2_mono (var_env1, var_env2) _monos1 _monos3 in
      (var_env1, var_env2)

and create_opc : opc -> opaque_opc = 
  function body -> {
    opc_delayed = (Var.Subst.id);
    opc = body
  }

and open_opc : opaque_opc -> opc = function abstraction ->
  let (var_delayed) = abstraction.opc_delayed in
  let body = abstraction.opc in
  let (var_bvars) = bound_opc body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_opc (var_env) body in
  if not (Var.Subst.is_id var_delayed) then begin
    abstraction.opc_delayed <- (Var.Subst.id);
    abstraction.opc <- body
  end;
  body

and open2_opc : opaque_opc -> opaque_opc -> opc * opc = fun x1 x2 -> 
  change_invalid_to_open2 open2i_opc x1 x2

and open2i_opc : opaque_opc -> opaque_opc -> opc * opc = fun abstraction1 abstraction2 ->
  let (var_delayed1) = abstraction1.opc_delayed in
  let body1 = abstraction1.opc in
  let (var_delayed2) = abstraction2.opc_delayed in
  let body2 = abstraction2.opc in
  let (var_env1, var_env2) = freshen2_opc (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_opc (var_env1) body1 in
  let body2 = subst_opc (var_env2) body2 in
  if not (Var.Subst.is_id var_delayed1) then begin
    abstraction1.opc_delayed <- (Var.Subst.id);
    abstraction1.opc <- body1
  end;
  if not (Var.Subst.is_id var_delayed2) then begin
    abstraction2.opc_delayed <- (Var.Subst.id);
    abstraction2.opc <- body2
  end;
  body1, body2

and apply_opc = 
  fun (var_env) abstraction ->
    let (var_delayed) = abstraction.opc_delayed in {
      abstraction with opc_delayed = (Var.Subst.compose var_env var_delayed)
    }

and subst_dcd : Datatype.Subst.t * Var.Subst.t -> dcd -> dcd = fun (datatype_oenv, var_ienv) -> function
  (_llayout1, _contrainte0) ->
    ((subst_llayout (datatype_oenv, var_ienv)) _llayout1, (subst_contrainte (var_ienv)) _contrainte0)

and bound_dcd : dcd -> Var.AtomSet.t = 
  function dcd -> bound_accu_dcd (Var.AtomSet.empty) dcd

and bound_free_dcd : dcd -> Var.AtomSet.t * Var.AtomSet.t * Datatype.AtomSet.t = 
  function dcd -> bound_free_accu_dcd (Var.AtomSet.empty, Var.AtomSet.empty, Datatype.AtomSet.empty) dcd

and equal_dcd : dcd -> dcd -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_dcd x1 x2

and import_dcd = fun (datatype_oenv, var_ienv) -> function
  (_llayout1, _contrainte0) ->
    ((import_llayout (datatype_oenv, var_ienv)) _llayout1, (import_contrainte (var_ienv)) _contrainte0)

and bvi_accu_dcd = fun (var_bvars) -> function
  (_llayout1, _contrainte0) ->
      let (var_bvars) = bvi_accu_llayout (var_bvars) _llayout1 in
      (var_bvars)

and bvi_dcd = 
  function dcd -> bvi_accu_dcd (Identifier.Map.empty) dcd

and bound_accu_dcd = fun (var_bvars) -> function
  (_llayout1, _contrainte0) ->
      let (var_bvars) = bound_accu_llayout (var_bvars) _llayout1 in
      (var_bvars)

and export_dcd : Datatype.AtomIdMap.t * Var.AtomIdMap.t -> dcd -> Raw.dcd = fun (datatype_om, var_im) -> function
  (_llayout1, _contrainte0) ->
    ((export_llayout (datatype_om, var_im)) _llayout1, (export_contrainte (var_im)) _contrainte0)

and flatten_dcd : dcd -> Flat.dcd = function
  (_llayout1, _contrainte0) ->
    (flatten_llayout _llayout1, flatten_contrainte _contrainte0)

and unflatten_dcd : Flat.dcd -> dcd = function
  (_llayout1, _contrainte0) ->
    (unflatten_llayout _llayout1, unflatten_contrainte _contrainte0)

and bound_free_accu_dcd = fun (var_bvars, var_ifvars, datatype_ofvars) -> function
  (_llayout1, _contrainte0) ->
      let (var_bvars, datatype_ofvars) = bound_free_accu_llayout (var_bvars, datatype_ofvars) _llayout1 in
      let (var_ifvars) = free_accu_contrainte (var_ifvars) _contrainte0 in
      (var_bvars, var_ifvars, datatype_ofvars)

and aeq_dcd : unit -> dcd -> dcd -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_llayout1, _contrainte0), (_llayout3, _contrainte2) ->
      aeq_llayout () _llayout1 _llayout3;
      aeq_contrainte () _contrainte0 _contrainte2;
      ()

and freshen2_dcd : Var.Subst.t * Var.Subst.t -> dcd -> dcd -> Var.Subst.t * Var.Subst.t = fun (var_env1, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_llayout1, _contrainte0), (_llayout3, _contrainte2) ->
      let (var_env1, var_env2) = freshen2_llayout (var_env1, var_env2) _llayout1 _llayout3 in
      (var_env1, var_env2)

and create_dcd : dcd -> opaque_dcd = 
  function body -> {
    dcd_delayed = (Datatype.Subst.id, Var.Subst.id);
    dcd = body
  }

and open_dcd : opaque_dcd -> dcd = function abstraction ->
  let (datatype_delayed, var_delayed) = abstraction.dcd_delayed in
  let body = abstraction.dcd in
  let (var_bvars) = bound_dcd body in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_dcd (datatype_delayed, var_env) body in
  if not (Datatype.Subst.is_id datatype_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.dcd_delayed <- (Datatype.Subst.id, Var.Subst.id);
    abstraction.dcd <- body
  end;
  body

and open2_dcd : opaque_dcd -> opaque_dcd -> dcd * dcd = fun x1 x2 -> 
  change_invalid_to_open2 open2i_dcd x1 x2

and open2i_dcd : opaque_dcd -> opaque_dcd -> dcd * dcd = fun abstraction1 abstraction2 ->
  let (datatype_delayed1, var_delayed1) = abstraction1.dcd_delayed in
  let body1 = abstraction1.dcd in
  let (datatype_delayed2, var_delayed2) = abstraction2.dcd_delayed in
  let body2 = abstraction2.dcd in
  let (var_env1, var_env2) = freshen2_dcd (Var.Subst.id, Var.Subst.id) body1 body2 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_dcd (datatype_delayed1, var_env1) body1 in
  let body2 = subst_dcd (datatype_delayed2, var_env2) body2 in
  if not (Datatype.Subst.is_id datatype_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.dcd_delayed <- (Datatype.Subst.id, Var.Subst.id);
    abstraction1.dcd <- body1
  end;
  if not (Datatype.Subst.is_id datatype_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.dcd_delayed <- (Datatype.Subst.id, Var.Subst.id);
    abstraction2.dcd <- body2
  end;
  body1, body2

and apply_dcd = 
  fun (datatype_env, var_env) abstraction ->
    let (datatype_delayed, var_delayed) = abstraction.dcd_delayed in {
      abstraction with dcd_delayed = (Datatype.Subst.compose datatype_env datatype_delayed, Var.Subst.compose var_env var_delayed)
    }

class map = object(self)

  method typ : typ -> typ = function
  | TData (_datatype0) ->
      self#tdata (_datatype0)
  | TAtom ->
      self#tatom
  | TAtomSet ->
      self#tatomset
  | TBool ->
      self#tbool

  method tdata : datatype -> typ = 
  function (_datatype0) -> 
      TData (_datatype0)

  method tatom : typ = TAtom

  method tatomset : typ = TAtomSet

  method tbool : typ = TBool

  method layout : layout -> layout = function
  | LComponent (_monos1, _typ0) ->
      self#lcomponent (_monos1, _typ0)
  | LInner (_llayout0) ->
      self#linner (_llayout0)
  | LOuter (_llayout0) ->
      self#louter (_llayout0)
  | LAbstraction (_llayout0) ->
      self#labstraction (_llayout0)
  | LTuple (_llayouts0) ->
      self#ltuple (_llayouts0)

  method lcomponent : mono option * typ -> layout = 
  function (_monos1, _typ0) -> 
      LComponent (option_map (self#mono) _monos1, (self#typ) _typ0)

  method linner : llayout -> layout = 
  function (_llayout0) -> 
      LInner ((self#llayout) _llayout0)

  method louter : llayout -> layout = 
  function (_llayout0) -> 
      LOuter ((self#llayout) _llayout0)

  method labstraction : llayout -> layout = 
  function (_llayout0) -> 
      LAbstraction ((self#llayout) _llayout0)

  method ltuple : llayout list -> layout = 
  function (_llayouts0) -> 
      LTuple (List.map (self#llayout) _llayouts0)

  method llayout : llayout -> llayout = function
  (_layouts0) ->
    (Annotation.map (self#layout) _layouts0)

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

  method set_entity : set_entity -> set_entity = function
  (_set_function1, _var0) ->
    ((self#set_function) _set_function1, _var0)

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

  method set_expression : set_expression -> set_expression = function
  | SEEmpty ->
      self#seempty
  | SEApp (_set_entity0) ->
      self#seapp (_set_entity0)
  | SEAssocOp (_setsetset_operator1, _set_expressions0) ->
      self#seassocop (_setsetset_operator1, _set_expressions0)
  | SEConditional (_contrainte2, _set_expression1, _set_expression0) ->
      self#seconditional (_contrainte2, _set_expression1, _set_expression0)

  method seempty : set_expression = SEEmpty

  method seapp : set_entity -> set_expression = 
  function (_set_entity0) -> 
      SEApp ((self#set_entity) _set_entity0)

  method seassocop : setsetset_operator * set_expression list -> set_expression = 
  function (_setsetset_operator1, _set_expressions0) -> 
      SEAssocOp ((self#setsetset_operator) _setsetset_operator1, List.map (self#set_expression) _set_expressions0)

  method seconditional : contrainte * set_expression * set_expression -> set_expression = 
  function (_contrainte2, _set_expression1, _set_expression0) -> 
      SEConditional ((self#contrainte) _contrainte2, (self#set_expression) _set_expression1, (self#set_expression) _set_expression0)

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

  method contrainte : contrainte -> contrainte = function
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

  method ftrue : contrainte = FTrue

  method ffalse : contrainte = FFalse

  method fboolvar : var -> contrainte = 
  function (_var0) -> 
      FBoolVar (_var0)

  method fnot : contrainte -> contrainte = 
  function (_contrainte0) -> 
      FNot ((self#contrainte) _contrainte0)

  method fboolassocop : boolboolbool_operator * contrainte list -> contrainte = 
  function (_boolboolbool_operator1, _contraintes0) -> 
      FBoolAssocOp ((self#boolboolbool_operator) _boolboolbool_operator1, List.map (self#contrainte) _contraintes0)

  method fsetbinop : set_expression * setsetbool_operator * set_expression -> contrainte = 
  function (_set_expression2, _setsetbool_operator1, _set_expression0) -> 
      FSetBinOp ((self#set_expression) _set_expression2, (self#setsetbool_operator) _setsetbool_operator1, (self#set_expression) _set_expression0)

  method tag : tag -> tag = function
  (_datacon0) ->
    (_datacon0)

  method raw_pattern : raw_pattern -> raw_pattern = function
  | PWildcard ->
      self#pwildcard
  | PVar (_var0) ->
      self#pvar (_var0)
  | PBool (_x0) ->
      self#pbool (_x0)
  | PTagTuple (_tag1, _patterns0) ->
      self#ptagtuple (_tag1, _patterns0)

  method pwildcard : raw_pattern = PWildcard

  method pvar : var -> raw_pattern = 
  function (_var0) -> 
      PVar (_var0)

  method pbool : ( bool ) -> raw_pattern = 
  function (_x0) -> 
      PBool (_x0)

  method ptagtuple : tag * pattern list -> raw_pattern = 
  function (_tag1, _patterns0) -> 
      PTagTuple ((self#tag) _tag1, List.map (self#pattern) _patterns0)

  method pattern : pattern -> pattern = function
  (_raw_patterns0) ->
    (Annotation.map (self#raw_pattern) _raw_patterns0)

  method lexp : lexp -> lexp = function
  (_expressions0) ->
    (Annotation.map (self#expression) _expressions0)

  method assertion : assertion -> assertion = function
  | StaticAssertion ->
      self#staticassertion
  | DynamicAssertion ->
      self#dynamicassertion

  method staticassertion : assertion = StaticAssertion

  method dynamicassertion : assertion = DynamicAssertion

  method expression : expression -> expression = function
  | EVar (_var0) ->
      self#evar (_var0)
  | EBool (_x0) ->
      self#ebool (_x0)
  | ETagTuple (_tag1, _lexps0) ->
      self#etagtuple (_tag1, _lexps0)
  | EFresh (_ofr0) ->
      self#efresh (_ofr0)
  | ECase (_lexp1, _branchs0) ->
      self#ecase (_lexp1, _branchs0)
  | ECall (_callee1, _lexp0) ->
      self#ecall (_callee1, _lexp0)
  | EMulti (_lexps0) ->
      self#emulti (_lexps0)
  | ELetMulti (_lexp1, _olm0) ->
      self#eletmulti (_lexp1, _olm0)
  | EAssert (_assertion2, _contrainte1, _lexp0) ->
      self#eassert (_assertion2, _contrainte1, _lexp0)
  | EAbsurd ->
      self#eabsurd
  | EFail ->
      self#efail
  | ENextCase ->
      self#enextcase

  method evar : var -> expression = 
  function (_var0) -> 
      EVar (_var0)

  method ebool : ( bool ) -> expression = 
  function (_x0) -> 
      EBool (_x0)

  method etagtuple : tag * lexp list -> expression = 
  function (_tag1, _lexps0) -> 
      ETagTuple ((self#tag) _tag1, List.map (self#lexp) _lexps0)

  method efresh : opaque_ofr -> expression = 
  function (_ofr0) -> 
      EFresh (create_ofr (self#ofr (open_ofr _ofr0)))

  method ecase : lexp * branch list -> expression = 
  function (_lexp1, _branchs0) -> 
      ECase ((self#lexp) _lexp1, List.map (self#branch) _branchs0)

  method ecall : callee * lexp -> expression = 
  function (_callee1, _lexp0) -> 
      ECall ((self#callee) _callee1, (self#lexp) _lexp0)

  method emulti : lexp list -> expression = 
  function (_lexps0) -> 
      EMulti (List.map (self#lexp) _lexps0)

  method eletmulti : lexp * opaque_olm -> expression = 
  function (_lexp1, _olm0) -> 
      ELetMulti ((self#lexp) _lexp1, create_olm (self#olm (open_olm _olm0)))

  method eassert : assertion * contrainte * lexp -> expression = 
  function (_assertion2, _contrainte1, _lexp0) -> 
      EAssert ((self#assertion) _assertion2, (self#contrainte) _contrainte1, (self#lexp) _lexp0)

  method eabsurd : expression = EAbsurd

  method efail : expression = EFail

  method enextcase : expression = ENextCase

  method callee : callee -> callee = function
  | CUser (_valfun0) ->
      self#cuser (_valfun0)
  | CPrim (_primitive0) ->
      self#cprim (_primitive0)

  method cuser : valfun -> callee = 
  function (_valfun0) -> 
      CUser (_valfun0)

  method cprim : primitive -> callee = 
  function (_primitive0) -> 
      CPrim ((self#primitive) _primitive0)

  method primitive : primitive -> primitive = function
  | PrimBoolAnd ->
      self#primbooland
  | PrimBoolOr ->
      self#primboolor
  | PrimBoolNot ->
      self#primboolnot
  | PrimAtomEquality ->
      self#primatomequality
  | PrimGenericSupport ->
      self#primgenericsupport
  | PrimGenericOuter ->
      self#primgenericouter
  | PrimGenericInner ->
      self#primgenericinner
  | PrimGenericBound ->
      self#primgenericbound
  | PrimSetEmpty ->
      self#primsetempty
  | PrimSetMember ->
      self#primsetmember
  | PrimSetAdd ->
      self#primsetadd
  | PrimSetUnion ->
      self#primsetunion
  | PrimSetInter ->
      self#primsetinter
  | PrimSetMinus ->
      self#primsetminus
  | PrimSetIsEmpty ->
      self#primsetisempty
  | PrimSetChoose ->
      self#primsetchoose
  | PrimSingletonRename ->
      self#primsingletonrename

  method primbooland : primitive = PrimBoolAnd

  method primboolor : primitive = PrimBoolOr

  method primboolnot : primitive = PrimBoolNot

  method primatomequality : primitive = PrimAtomEquality

  method primgenericsupport : primitive = PrimGenericSupport

  method primgenericouter : primitive = PrimGenericOuter

  method primgenericinner : primitive = PrimGenericInner

  method primgenericbound : primitive = PrimGenericBound

  method primsetempty : primitive = PrimSetEmpty

  method primsetmember : primitive = PrimSetMember

  method primsetadd : primitive = PrimSetAdd

  method primsetunion : primitive = PrimSetUnion

  method primsetinter : primitive = PrimSetInter

  method primsetminus : primitive = PrimSetMinus

  method primsetisempty : primitive = PrimSetIsEmpty

  method primsetchoose : primitive = PrimSetChoose

  method primsingletonrename : primitive = PrimSingletonRename

  method mono : mono -> mono = function
  (_var0) ->
    (_var0)

  method branch : branch -> branch = function
  (_obr0) ->
    (create_obr (self#obr (open_obr _obr0)))

  method def : def -> def = function
  | DefValFun (_valfun1, _fundef0) ->
      self#defvalfun (_valfun1, _fundef0)
  | DefDataType (_datatype1, _datatypedef0) ->
      self#defdatatype (_datatype1, _datatypedef0)

  method defvalfun : valfun * fundef -> def = 
  function (_valfun1, _fundef0) -> 
      DefValFun (_valfun1, (self#fundef) _fundef0)

  method defdatatype : datatype * datatypedef -> def = 
  function (_datatype1, _datatypedef0) -> 
      DefDataType (_datatype1, (self#datatypedef) _datatypedef0)

  method fundef : fundef -> fundef = function
  (_ofu0) ->
    (create_ofu (self#ofu (open_ofu _ofu0)))

  method specification : specification -> specification = function
  (_osp0) ->
    (create_osp (self#osp (open_osp _osp0)))

  method postcondition : postcondition -> postcondition = function
  (_opc0) ->
    (create_opc (self#opc (open_opc _opc0)))

  method datatypedef : datatypedef -> datatypedef = function
  (_kind1, _datacondefs0) ->
    ((self#kind) _kind1, List.map (self#datacondef) _datacondefs0)

  method kind : kind -> kind = function
  | KExpression ->
      self#kexpression
  | KPattern ->
      self#kpattern

  method kexpression : kind = KExpression

  method kpattern : kind = KPattern

  method datacondef : datacondef -> datacondef = function
  (_datacon1, _datacondetails0) ->
    (_datacon1, (self#datacondetails) _datacondetails0)

  method datacondetails : datacondetails -> datacondetails = function
  (_dcd0) ->
    (create_dcd (self#dcd (open_dcd _dcd0)))

  method defs : defs -> defs = function
  (_defs0) ->
    (List.map (self#def) _defs0)

  method program : program -> program = function
  (_defs0) ->
    (create_defs (self#defs (open_defs _defs0)))

  method ofr : ofr -> ofr = function
  (_monos1, _lexp0) ->
    (List.map (self#mono) _monos1, (self#lexp) _lexp0)

  method olm : olm -> olm = function
  (_monos2, _contrainte1, _lexp0) ->
    (List.map (self#mono) _monos2, (self#contrainte) _contrainte1, (self#lexp) _lexp0)

  method obr : obr -> obr = function
  (_patterns1, _lexp0) ->
    (List.map (self#pattern) _patterns1, (self#lexp) _lexp0)

  method ofu : ofu -> ofu = function
  (_monos3, _contrainte2, _postcondition1, _lexp0) ->
    (List.map (self#mono) _monos3, (self#contrainte) _contrainte2, (self#postcondition) _postcondition1, (self#lexp) _lexp0)

  method osp : osp -> osp = function
  (_monos2, _contrainte1, _postcondition0) ->
    (List.map (self#mono) _monos2, (self#contrainte) _contrainte1, (self#postcondition) _postcondition0)

  method opc : opc -> opc = function
  (_monos1, _contrainte0) ->
    (List.map (self#mono) _monos1, (self#contrainte) _contrainte0)

  method dcd : dcd -> dcd = function
  (_llayout1, _contrainte0) ->
    ((self#llayout) _llayout1, (self#contrainte) _contrainte0)

end

class [ 'accumulator ] fold = object(self)

  method typ : 'accumulator -> typ -> 'accumulator = fun accu -> function
  | TData (_datatype0) ->
      self#tdata accu (_datatype0)
  | TAtom ->
      self#tatom accu
  | TAtomSet ->
      self#tatomset accu
  | TBool ->
      self#tbool accu

  method tdata : 'accumulator -> datatype -> 'accumulator = fun accu -> 
  function (_datatype0) -> 
      accu

  method tatom : 'accumulator -> 'accumulator = fun accu ->       accu

  method tatomset : 'accumulator -> 'accumulator = fun accu ->       accu

  method tbool : 'accumulator -> 'accumulator = fun accu ->       accu

  method layout : 'accumulator -> layout -> 'accumulator = fun accu -> function
  | LComponent (_monos1, _typ0) ->
      self#lcomponent accu (_monos1, _typ0)
  | LInner (_llayout0) ->
      self#linner accu (_llayout0)
  | LOuter (_llayout0) ->
      self#louter accu (_llayout0)
  | LAbstraction (_llayout0) ->
      self#labstraction accu (_llayout0)
  | LTuple (_llayouts0) ->
      self#ltuple accu (_llayouts0)

  method lcomponent : 'accumulator -> mono option * typ -> 'accumulator = fun accu -> 
  function (_monos1, _typ0) -> 
      let accu = option_fold (self#mono) accu _monos1 in
      let accu = (self#typ) accu _typ0 in
      accu

  method linner : 'accumulator -> llayout -> 'accumulator = fun accu -> 
  function (_llayout0) -> 
      let accu = (self#llayout) accu _llayout0 in
      accu

  method louter : 'accumulator -> llayout -> 'accumulator = fun accu -> 
  function (_llayout0) -> 
      let accu = (self#llayout) accu _llayout0 in
      accu

  method labstraction : 'accumulator -> llayout -> 'accumulator = fun accu -> 
  function (_llayout0) -> 
      let accu = (self#llayout) accu _llayout0 in
      accu

  method ltuple : 'accumulator -> llayout list -> 'accumulator = fun accu -> 
  function (_llayouts0) -> 
      let accu = List.fold_left (self#llayout) accu _llayouts0 in
      accu

  method llayout : 'accumulator -> llayout -> 'accumulator = fun accu -> function
  (_layouts0) ->
      let accu = Annotation.fold (self#layout) accu _layouts0 in
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

  method set_entity : 'accumulator -> set_entity -> 'accumulator = fun accu -> function
  (_set_function1, _var0) ->
      let accu = (self#set_function) accu _set_function1 in
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

  method set_expression : 'accumulator -> set_expression -> 'accumulator = fun accu -> function
  | SEEmpty ->
      self#seempty accu
  | SEApp (_set_entity0) ->
      self#seapp accu (_set_entity0)
  | SEAssocOp (_setsetset_operator1, _set_expressions0) ->
      self#seassocop accu (_setsetset_operator1, _set_expressions0)
  | SEConditional (_contrainte2, _set_expression1, _set_expression0) ->
      self#seconditional accu (_contrainte2, _set_expression1, _set_expression0)

  method seempty : 'accumulator -> 'accumulator = fun accu ->       accu

  method seapp : 'accumulator -> set_entity -> 'accumulator = fun accu -> 
  function (_set_entity0) -> 
      let accu = (self#set_entity) accu _set_entity0 in
      accu

  method seassocop : 'accumulator -> setsetset_operator * set_expression list -> 'accumulator = fun accu -> 
  function (_setsetset_operator1, _set_expressions0) -> 
      let accu = (self#setsetset_operator) accu _setsetset_operator1 in
      let accu = List.fold_left (self#set_expression) accu _set_expressions0 in
      accu

  method seconditional : 'accumulator -> contrainte * set_expression * set_expression -> 'accumulator = fun accu -> 
  function (_contrainte2, _set_expression1, _set_expression0) -> 
      let accu = (self#contrainte) accu _contrainte2 in
      let accu = (self#set_expression) accu _set_expression1 in
      let accu = (self#set_expression) accu _set_expression0 in
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

  method contrainte : 'accumulator -> contrainte -> 'accumulator = fun accu -> function
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

  method tag : 'accumulator -> tag -> 'accumulator = fun accu -> function
  (_datacon0) ->
      accu

  method raw_pattern : 'accumulator -> raw_pattern -> 'accumulator = fun accu -> function
  | PWildcard ->
      self#pwildcard accu
  | PVar (_var0) ->
      self#pvar accu (_var0)
  | PBool (_x0) ->
      self#pbool accu (_x0)
  | PTagTuple (_tag1, _patterns0) ->
      self#ptagtuple accu (_tag1, _patterns0)

  method pwildcard : 'accumulator -> 'accumulator = fun accu ->       accu

  method pvar : 'accumulator -> var -> 'accumulator = fun accu -> 
  function (_var0) -> 
      accu

  method pbool : 'accumulator -> ( bool ) -> 'accumulator = fun accu -> 
  function (_x0) -> 
      accu

  method ptagtuple : 'accumulator -> tag * pattern list -> 'accumulator = fun accu -> 
  function (_tag1, _patterns0) -> 
      let accu = (self#tag) accu _tag1 in
      let accu = List.fold_left (self#pattern) accu _patterns0 in
      accu

  method pattern : 'accumulator -> pattern -> 'accumulator = fun accu -> function
  (_raw_patterns0) ->
      let accu = Annotation.fold (self#raw_pattern) accu _raw_patterns0 in
      accu

  method lexp : 'accumulator -> lexp -> 'accumulator = fun accu -> function
  (_expressions0) ->
      let accu = Annotation.fold (self#expression) accu _expressions0 in
      accu

  method assertion : 'accumulator -> assertion -> 'accumulator = fun accu -> function
  | StaticAssertion ->
      self#staticassertion accu
  | DynamicAssertion ->
      self#dynamicassertion accu

  method staticassertion : 'accumulator -> 'accumulator = fun accu ->       accu

  method dynamicassertion : 'accumulator -> 'accumulator = fun accu ->       accu

  method expression : 'accumulator -> expression -> 'accumulator = fun accu -> function
  | EVar (_var0) ->
      self#evar accu (_var0)
  | EBool (_x0) ->
      self#ebool accu (_x0)
  | ETagTuple (_tag1, _lexps0) ->
      self#etagtuple accu (_tag1, _lexps0)
  | EFresh (_ofr0) ->
      self#efresh accu (_ofr0)
  | ECase (_lexp1, _branchs0) ->
      self#ecase accu (_lexp1, _branchs0)
  | ECall (_callee1, _lexp0) ->
      self#ecall accu (_callee1, _lexp0)
  | EMulti (_lexps0) ->
      self#emulti accu (_lexps0)
  | ELetMulti (_lexp1, _olm0) ->
      self#eletmulti accu (_lexp1, _olm0)
  | EAssert (_assertion2, _contrainte1, _lexp0) ->
      self#eassert accu (_assertion2, _contrainte1, _lexp0)
  | EAbsurd ->
      self#eabsurd accu
  | EFail ->
      self#efail accu
  | ENextCase ->
      self#enextcase accu

  method evar : 'accumulator -> var -> 'accumulator = fun accu -> 
  function (_var0) -> 
      accu

  method ebool : 'accumulator -> ( bool ) -> 'accumulator = fun accu -> 
  function (_x0) -> 
      accu

  method etagtuple : 'accumulator -> tag * lexp list -> 'accumulator = fun accu -> 
  function (_tag1, _lexps0) -> 
      let accu = (self#tag) accu _tag1 in
      let accu = List.fold_left (self#lexp) accu _lexps0 in
      accu

  method efresh : 'accumulator -> opaque_ofr -> 'accumulator = fun accu -> 
  function (_ofr0) -> 
      let accu = self#ofr accu (open_ofr _ofr0) in
      accu

  method ecase : 'accumulator -> lexp * branch list -> 'accumulator = fun accu -> 
  function (_lexp1, _branchs0) -> 
      let accu = (self#lexp) accu _lexp1 in
      let accu = List.fold_left (self#branch) accu _branchs0 in
      accu

  method ecall : 'accumulator -> callee * lexp -> 'accumulator = fun accu -> 
  function (_callee1, _lexp0) -> 
      let accu = (self#callee) accu _callee1 in
      let accu = (self#lexp) accu _lexp0 in
      accu

  method emulti : 'accumulator -> lexp list -> 'accumulator = fun accu -> 
  function (_lexps0) -> 
      let accu = List.fold_left (self#lexp) accu _lexps0 in
      accu

  method eletmulti : 'accumulator -> lexp * opaque_olm -> 'accumulator = fun accu -> 
  function (_lexp1, _olm0) -> 
      let accu = (self#lexp) accu _lexp1 in
      let accu = self#olm accu (open_olm _olm0) in
      accu

  method eassert : 'accumulator -> assertion * contrainte * lexp -> 'accumulator = fun accu -> 
  function (_assertion2, _contrainte1, _lexp0) -> 
      let accu = (self#assertion) accu _assertion2 in
      let accu = (self#contrainte) accu _contrainte1 in
      let accu = (self#lexp) accu _lexp0 in
      accu

  method eabsurd : 'accumulator -> 'accumulator = fun accu ->       accu

  method efail : 'accumulator -> 'accumulator = fun accu ->       accu

  method enextcase : 'accumulator -> 'accumulator = fun accu ->       accu

  method callee : 'accumulator -> callee -> 'accumulator = fun accu -> function
  | CUser (_valfun0) ->
      self#cuser accu (_valfun0)
  | CPrim (_primitive0) ->
      self#cprim accu (_primitive0)

  method cuser : 'accumulator -> valfun -> 'accumulator = fun accu -> 
  function (_valfun0) -> 
      accu

  method cprim : 'accumulator -> primitive -> 'accumulator = fun accu -> 
  function (_primitive0) -> 
      let accu = (self#primitive) accu _primitive0 in
      accu

  method primitive : 'accumulator -> primitive -> 'accumulator = fun accu -> function
  | PrimBoolAnd ->
      self#primbooland accu
  | PrimBoolOr ->
      self#primboolor accu
  | PrimBoolNot ->
      self#primboolnot accu
  | PrimAtomEquality ->
      self#primatomequality accu
  | PrimGenericSupport ->
      self#primgenericsupport accu
  | PrimGenericOuter ->
      self#primgenericouter accu
  | PrimGenericInner ->
      self#primgenericinner accu
  | PrimGenericBound ->
      self#primgenericbound accu
  | PrimSetEmpty ->
      self#primsetempty accu
  | PrimSetMember ->
      self#primsetmember accu
  | PrimSetAdd ->
      self#primsetadd accu
  | PrimSetUnion ->
      self#primsetunion accu
  | PrimSetInter ->
      self#primsetinter accu
  | PrimSetMinus ->
      self#primsetminus accu
  | PrimSetIsEmpty ->
      self#primsetisempty accu
  | PrimSetChoose ->
      self#primsetchoose accu
  | PrimSingletonRename ->
      self#primsingletonrename accu

  method primbooland : 'accumulator -> 'accumulator = fun accu ->       accu

  method primboolor : 'accumulator -> 'accumulator = fun accu ->       accu

  method primboolnot : 'accumulator -> 'accumulator = fun accu ->       accu

  method primatomequality : 'accumulator -> 'accumulator = fun accu ->       accu

  method primgenericsupport : 'accumulator -> 'accumulator = fun accu ->       accu

  method primgenericouter : 'accumulator -> 'accumulator = fun accu ->       accu

  method primgenericinner : 'accumulator -> 'accumulator = fun accu ->       accu

  method primgenericbound : 'accumulator -> 'accumulator = fun accu ->       accu

  method primsetempty : 'accumulator -> 'accumulator = fun accu ->       accu

  method primsetmember : 'accumulator -> 'accumulator = fun accu ->       accu

  method primsetadd : 'accumulator -> 'accumulator = fun accu ->       accu

  method primsetunion : 'accumulator -> 'accumulator = fun accu ->       accu

  method primsetinter : 'accumulator -> 'accumulator = fun accu ->       accu

  method primsetminus : 'accumulator -> 'accumulator = fun accu ->       accu

  method primsetisempty : 'accumulator -> 'accumulator = fun accu ->       accu

  method primsetchoose : 'accumulator -> 'accumulator = fun accu ->       accu

  method primsingletonrename : 'accumulator -> 'accumulator = fun accu ->       accu

  method mono : 'accumulator -> mono -> 'accumulator = fun accu -> function
  (_var0) ->
      accu

  method branch : 'accumulator -> branch -> 'accumulator = fun accu -> function
  (_obr0) ->
      let accu = self#obr accu (open_obr _obr0) in
      accu

  method def : 'accumulator -> def -> 'accumulator = fun accu -> function
  | DefValFun (_valfun1, _fundef0) ->
      self#defvalfun accu (_valfun1, _fundef0)
  | DefDataType (_datatype1, _datatypedef0) ->
      self#defdatatype accu (_datatype1, _datatypedef0)

  method defvalfun : 'accumulator -> valfun * fundef -> 'accumulator = fun accu -> 
  function (_valfun1, _fundef0) -> 
      let accu = (self#fundef) accu _fundef0 in
      accu

  method defdatatype : 'accumulator -> datatype * datatypedef -> 'accumulator = fun accu -> 
  function (_datatype1, _datatypedef0) -> 
      let accu = (self#datatypedef) accu _datatypedef0 in
      accu

  method fundef : 'accumulator -> fundef -> 'accumulator = fun accu -> function
  (_ofu0) ->
      let accu = self#ofu accu (open_ofu _ofu0) in
      accu

  method specification : 'accumulator -> specification -> 'accumulator = fun accu -> function
  (_osp0) ->
      let accu = self#osp accu (open_osp _osp0) in
      accu

  method postcondition : 'accumulator -> postcondition -> 'accumulator = fun accu -> function
  (_opc0) ->
      let accu = self#opc accu (open_opc _opc0) in
      accu

  method datatypedef : 'accumulator -> datatypedef -> 'accumulator = fun accu -> function
  (_kind1, _datacondefs0) ->
      let accu = (self#kind) accu _kind1 in
      let accu = List.fold_left (self#datacondef) accu _datacondefs0 in
      accu

  method kind : 'accumulator -> kind -> 'accumulator = fun accu -> function
  | KExpression ->
      self#kexpression accu
  | KPattern ->
      self#kpattern accu

  method kexpression : 'accumulator -> 'accumulator = fun accu ->       accu

  method kpattern : 'accumulator -> 'accumulator = fun accu ->       accu

  method datacondef : 'accumulator -> datacondef -> 'accumulator = fun accu -> function
  (_datacon1, _datacondetails0) ->
      let accu = (self#datacondetails) accu _datacondetails0 in
      accu

  method datacondetails : 'accumulator -> datacondetails -> 'accumulator = fun accu -> function
  (_dcd0) ->
      let accu = self#dcd accu (open_dcd _dcd0) in
      accu

  method defs : 'accumulator -> defs -> 'accumulator = fun accu -> function
  (_defs0) ->
      let accu = List.fold_left (self#def) accu _defs0 in
      accu

  method program : 'accumulator -> program -> 'accumulator = fun accu -> function
  (_defs0) ->
      let accu = self#defs accu (open_defs _defs0) in
      accu

  method ofr : 'accumulator -> ofr -> 'accumulator = fun accu -> function
  (_monos1, _lexp0) ->
      let accu = List.fold_left (self#mono) accu _monos1 in
      let accu = (self#lexp) accu _lexp0 in
      accu

  method olm : 'accumulator -> olm -> 'accumulator = fun accu -> function
  (_monos2, _contrainte1, _lexp0) ->
      let accu = List.fold_left (self#mono) accu _monos2 in
      let accu = (self#contrainte) accu _contrainte1 in
      let accu = (self#lexp) accu _lexp0 in
      accu

  method obr : 'accumulator -> obr -> 'accumulator = fun accu -> function
  (_patterns1, _lexp0) ->
      let accu = List.fold_left (self#pattern) accu _patterns1 in
      let accu = (self#lexp) accu _lexp0 in
      accu

  method ofu : 'accumulator -> ofu -> 'accumulator = fun accu -> function
  (_monos3, _contrainte2, _postcondition1, _lexp0) ->
      let accu = List.fold_left (self#mono) accu _monos3 in
      let accu = (self#contrainte) accu _contrainte2 in
      let accu = (self#postcondition) accu _postcondition1 in
      let accu = (self#lexp) accu _lexp0 in
      accu

  method osp : 'accumulator -> osp -> 'accumulator = fun accu -> function
  (_monos2, _contrainte1, _postcondition0) ->
      let accu = List.fold_left (self#mono) accu _monos2 in
      let accu = (self#contrainte) accu _contrainte1 in
      let accu = (self#postcondition) accu _postcondition0 in
      accu

  method opc : 'accumulator -> opc -> 'accumulator = fun accu -> function
  (_monos1, _contrainte0) ->
      let accu = List.fold_left (self#mono) accu _monos1 in
      let accu = (self#contrainte) accu _contrainte0 in
      accu

  method dcd : 'accumulator -> dcd -> 'accumulator = fun accu -> function
  (_llayout1, _contrainte0) ->
      let accu = (self#llayout) accu _llayout1 in
      let accu = (self#contrainte) accu _contrainte0 in
      accu

end
