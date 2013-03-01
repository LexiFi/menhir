(* This file was generated from ./essai.mla. Do not edit! *)

module Identifier = AlphaLib.Atom.String

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

module Tycon = AlphaLib.Atom.Make(Identifier)

module Var = AlphaLib.Atom.Make(Identifier)

module Raw = struct

type var =
  Identifier.t

 and tycon =
  Identifier.t

 and pattern = 
  | PVar of var
  | PPair of pattern * pattern

 and ty = 
  | TyCon of tycon

 and expr = 
  | EVar of var

 and patexpr = 
  pattern * expr

 and megapattern = 
  | P of pattern
  | Q of tycon

 and megaexpr = 
  | E of expr
  | T of ty

 and megapatexpr = 
  megapattern * megaexpr

 and abs = 
  megapatexpr

end

module Flat = struct

type var =
  Var.Atom.t

 and tycon =
  Tycon.Atom.t

 and pattern = 
  | PVar of var
  | PPair of pattern * pattern

 and ty = 
  | TyCon of tycon

 and expr = 
  | EVar of var

 and patexpr = 
  pattern * expr

 and megapattern = 
  | P of pattern
  | Q of tycon

 and megaexpr = 
  | E of expr
  | T of ty

 and megapatexpr = 
  megapattern * megaexpr

 and abs = 
  megapatexpr

end

type var =
  Var.Atom.t

 and tycon =
  Tycon.Atom.t

 and pattern = 
  | PVar of var
  | PPair of pattern * pattern

 and ty = 
  | TyCon of tycon

 and expr = 
  | EVar of var

 and patexpr = 
  pattern * expr

 and megapattern = 
  | P of pattern
  | Q of tycon

 and megaexpr = 
  | E of expr
  | T of ty

 and megapatexpr = 
  megapattern * megaexpr

 and opaque_megapatexpr = {
    mutable megapatexpr_delayed: Tycon.Subst.t * Var.Subst.t;
    mutable megapatexpr: megapatexpr
  }

 and abs = 
  opaque_megapatexpr

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

and subst_pattern : Var.Subst.t -> pattern -> pattern = fun (var_ienv) -> function
  | PVar (_var0) ->
      PVar (Var.Subst.lookup _var0 var_ienv)
  | PPair (_pattern1, _pattern0) ->
      PPair ((subst_pattern (var_ienv)) _pattern1, (subst_pattern (var_ienv)) _pattern0)

and bound_pattern : pattern -> Var.AtomSet.t = 
  function pattern -> bound_accu_pattern (Var.AtomSet.empty) pattern

and bound_free_pattern : pattern -> Var.AtomSet.t = 
  function pattern -> bound_free_accu_pattern (Var.AtomSet.empty) pattern

and equal_pattern : pattern -> pattern -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_pattern x1 x2

and import_pattern = fun (var_ienv) -> function
  | Raw.PVar (_var0) ->
      PVar (Var.find _var0 var_ienv)
  | Raw.PPair (_pattern1, _pattern0) ->
      PPair ((import_pattern (var_ienv)) _pattern1, (import_pattern (var_ienv)) _pattern0)

and bvi_accu_pattern = fun (var_bvars) -> function
  | Raw.PVar (_var0) ->
      let var_bvars = Identifier.Map.add _var0 () var_bvars in
      (var_bvars)
  | Raw.PPair (_pattern1, _pattern0) ->
      let (var_bvars) = bvi_accu_pattern (var_bvars) _pattern1 in
      let (var_bvars) = bvi_accu_pattern (var_bvars) _pattern0 in
      (var_bvars)

and bvi_pattern = 
  function pattern -> bvi_accu_pattern (Identifier.Map.empty) pattern

and bound_accu_pattern = fun (var_bvars) -> function
  | PVar (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars)
  | PPair (_pattern1, _pattern0) ->
      let (var_bvars) = bound_accu_pattern (var_bvars) _pattern1 in
      let (var_bvars) = bound_accu_pattern (var_bvars) _pattern0 in
      (var_bvars)

and export_pattern : Var.AtomIdMap.t -> pattern -> Raw.pattern = fun (var_im) -> function
  | PVar (_var0) ->
      Raw.PVar (Var.AtomIdMap.lookup _var0 var_im)
  | PPair (_pattern1, _pattern0) ->
      Raw.PPair ((export_pattern (var_im)) _pattern1, (export_pattern (var_im)) _pattern0)

and flatten_pattern : pattern -> Flat.pattern = function
  | PVar (_var0) ->
      Flat.PVar (_var0)
  | PPair (_pattern1, _pattern0) ->
      Flat.PPair (flatten_pattern _pattern1, flatten_pattern _pattern0)

and unflatten_pattern : Flat.pattern -> pattern = function
  | Flat.PVar (_var0) ->
      PVar (_var0)
  | Flat.PPair (_pattern1, _pattern0) ->
      PPair (unflatten_pattern _pattern1, unflatten_pattern _pattern0)

and bound_free_accu_pattern = fun (var_bvars) -> function
  | PVar (_var0) ->
      let var_bvars = Var.AtomSet.add _var0 var_bvars in
      (var_bvars)
  | PPair (_pattern1, _pattern0) ->
      let (var_bvars) = bound_free_accu_pattern (var_bvars) _pattern1 in
      let (var_bvars) = bound_free_accu_pattern (var_bvars) _pattern0 in
      (var_bvars)

and aeq_pattern : unit -> pattern -> pattern -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | PVar (_var0), PVar (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_pattern");
      ()
  | PPair (_pattern1, _pattern0), PPair (_pattern3, _pattern2) ->
      aeq_pattern () _pattern1 _pattern3;
      aeq_pattern () _pattern0 _pattern2;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_pattern")

and freshen2_pattern : Tycon.Subst.t * Var.Subst.t * Tycon.Subst.t * Var.Subst.t -> pattern -> pattern -> Tycon.Subst.t * Var.Subst.t * Tycon.Subst.t * Var.Subst.t = fun (tycon_env1, var_env1, tycon_env2, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | PVar (_var0), PVar (_var1) ->
      let var_env1, var_env2 = Var.Subst.freshen2 _var0 var_env1 _var1 var_env2 in
      (tycon_env1, var_env1, tycon_env2, var_env2)
  | PPair (_pattern1, _pattern0), PPair (_pattern3, _pattern2) ->
      let (tycon_env1, var_env1, tycon_env2, var_env2) = freshen2_pattern (tycon_env1, var_env1, tycon_env2, var_env2) _pattern1 _pattern3 in
      let (tycon_env1, var_env1, tycon_env2, var_env2) = freshen2_pattern (tycon_env1, var_env1, tycon_env2, var_env2) _pattern0 _pattern2 in
      (tycon_env1, var_env1, tycon_env2, var_env2)
  | _, _ ->
      raise (Invalid_argument "freshen2_pattern")

and import_ty : tycon Identifier.Map.t -> Raw.ty -> ty = fun (tycon_env) -> function
  | Raw.TyCon (_tycon0) ->
      TyCon (Tycon.find _tycon0 tycon_env)

and subst_ty : Tycon.Subst.t -> ty -> ty = fun (tycon_env) -> function
  | TyCon (_tycon0) ->
      TyCon (Tycon.Subst.lookup _tycon0 tycon_env)

and export_ty : Tycon.AtomIdMap.t -> ty -> Raw.ty = fun (tycon_m) -> function
  | TyCon (_tycon0) ->
      Raw.TyCon (Tycon.AtomIdMap.lookup _tycon0 tycon_m)

and flatten_ty : ty -> Flat.ty = function
  | TyCon (_tycon0) ->
      Flat.TyCon (_tycon0)

and unflatten_ty : Flat.ty -> ty = function
  | Flat.TyCon (_tycon0) ->
      TyCon (_tycon0)

and free_ty : ty -> Tycon.AtomSet.t = 
  function ty -> free_accu_ty (Tycon.AtomSet.empty) ty

and equal_ty : ty -> ty -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_ty x1 x2

and aeq_ty : unit -> ty -> ty -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | TyCon (_tycon0), TyCon (_tycon1) ->
      if not (Tycon.Atom.equal _tycon0 _tycon1) then raise (Invalid_argument "aeq_ty");
      ()

and free_accu_ty = fun (tycon_fvars) -> function
  | TyCon (_tycon0) ->
      let tycon_fvars = Tycon.AtomSet.add _tycon0 tycon_fvars in
      (tycon_fvars)

and import_expr : var Identifier.Map.t -> Raw.expr -> expr = fun (var_env) -> function
  | Raw.EVar (_var0) ->
      EVar (Var.find _var0 var_env)

and subst_expr : Var.Subst.t -> expr -> expr = fun (var_env) -> function
  | EVar (_var0) ->
      EVar (Var.Subst.lookup _var0 var_env)

and export_expr : Var.AtomIdMap.t -> expr -> Raw.expr = fun (var_m) -> function
  | EVar (_var0) ->
      Raw.EVar (Var.AtomIdMap.lookup _var0 var_m)

and flatten_expr : expr -> Flat.expr = function
  | EVar (_var0) ->
      Flat.EVar (_var0)

and unflatten_expr : Flat.expr -> expr = function
  | Flat.EVar (_var0) ->
      EVar (_var0)

and free_expr : expr -> Var.AtomSet.t = 
  function expr -> free_accu_expr (Var.AtomSet.empty) expr

and equal_expr : expr -> expr -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_expr x1 x2

and aeq_expr : unit -> expr -> expr -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | EVar (_var0), EVar (_var1) ->
      if not (Var.Atom.equal _var0 _var1) then raise (Invalid_argument "aeq_expr");
      ()

and free_accu_expr = fun (var_fvars) -> function
  | EVar (_var0) ->
      let var_fvars = Var.AtomSet.add _var0 var_fvars in
      (var_fvars)

and subst_patexpr : Var.Subst.t -> patexpr -> patexpr = fun (var_ienv) -> function
  (_pattern1, _expr0) ->
    ((subst_pattern (var_ienv)) _pattern1, (subst_expr (var_ienv)) _expr0)

and bound_patexpr : patexpr -> Var.AtomSet.t = 
  function patexpr -> bound_accu_patexpr (Var.AtomSet.empty) patexpr

and bound_free_patexpr : patexpr -> Var.AtomSet.t * Var.AtomSet.t = 
  function patexpr -> bound_free_accu_patexpr (Var.AtomSet.empty, Var.AtomSet.empty) patexpr

and equal_patexpr : patexpr -> patexpr -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_patexpr x1 x2

and import_patexpr = fun (var_ienv) -> function
  (_pattern1, _expr0) ->
    ((import_pattern (var_ienv)) _pattern1, (import_expr (var_ienv)) _expr0)

and bvi_accu_patexpr = fun (var_bvars) -> function
  (_pattern1, _expr0) ->
      let (var_bvars) = bvi_accu_pattern (var_bvars) _pattern1 in
      (var_bvars)

and bvi_patexpr = 
  function patexpr -> bvi_accu_patexpr (Identifier.Map.empty) patexpr

and bound_accu_patexpr = fun (var_bvars) -> function
  (_pattern1, _expr0) ->
      let (var_bvars) = bound_accu_pattern (var_bvars) _pattern1 in
      (var_bvars)

and export_patexpr : Var.AtomIdMap.t -> patexpr -> Raw.patexpr = fun (var_im) -> function
  (_pattern1, _expr0) ->
    ((export_pattern (var_im)) _pattern1, (export_expr (var_im)) _expr0)

and flatten_patexpr : patexpr -> Flat.patexpr = function
  (_pattern1, _expr0) ->
    (flatten_pattern _pattern1, flatten_expr _expr0)

and unflatten_patexpr : Flat.patexpr -> patexpr = function
  (_pattern1, _expr0) ->
    (unflatten_pattern _pattern1, unflatten_expr _expr0)

and bound_free_accu_patexpr = fun (var_bvars, var_ifvars) -> function
  (_pattern1, _expr0) ->
      let (var_bvars) = bound_free_accu_pattern (var_bvars) _pattern1 in
      let (var_ifvars) = free_accu_expr (var_ifvars) _expr0 in
      (var_bvars, var_ifvars)

and aeq_patexpr : unit -> patexpr -> patexpr -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_pattern1, _expr0), (_pattern3, _expr2) ->
      aeq_pattern () _pattern1 _pattern3;
      aeq_expr () _expr0 _expr2;
      ()

and freshen2_patexpr : Tycon.Subst.t * Var.Subst.t * Tycon.Subst.t * Var.Subst.t -> patexpr -> patexpr -> Tycon.Subst.t * Var.Subst.t * Tycon.Subst.t * Var.Subst.t = fun (tycon_env1, var_env1, tycon_env2, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_pattern1, _expr0), (_pattern3, _expr2) ->
      let (tycon_env1, var_env1, tycon_env2, var_env2) = freshen2_pattern (tycon_env1, var_env1, tycon_env2, var_env2) _pattern1 _pattern3 in
      (tycon_env1, var_env1, tycon_env2, var_env2)

and subst_megapattern : Tycon.Subst.t * Var.Subst.t -> megapattern -> megapattern = fun (tycon_ienv, var_ienv) -> function
  | P (_pattern0) ->
      P ((subst_pattern (var_ienv)) _pattern0)
  | Q (_tycon0) ->
      Q (Tycon.Subst.lookup _tycon0 tycon_ienv)

and bound_megapattern : megapattern -> Tycon.AtomSet.t * Var.AtomSet.t = 
  function megapattern -> bound_accu_megapattern (Tycon.AtomSet.empty, Var.AtomSet.empty) megapattern

and bound_free_megapattern : megapattern -> Tycon.AtomSet.t * Var.AtomSet.t = 
  function megapattern -> bound_free_accu_megapattern (Tycon.AtomSet.empty, Var.AtomSet.empty) megapattern

and equal_megapattern : megapattern -> megapattern -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_megapattern x1 x2

and import_megapattern = fun (tycon_ienv, var_ienv) -> function
  | Raw.P (_pattern0) ->
      P ((import_pattern (var_ienv)) _pattern0)
  | Raw.Q (_tycon0) ->
      Q (Tycon.find _tycon0 tycon_ienv)

and bvi_accu_megapattern = fun (tycon_bvars, var_bvars) -> function
  | Raw.P (_pattern0) ->
      let (var_bvars) = bvi_accu_pattern (var_bvars) _pattern0 in
      (tycon_bvars, var_bvars)
  | Raw.Q (_tycon0) ->
      let tycon_bvars = Identifier.Map.add _tycon0 () tycon_bvars in
      (tycon_bvars, var_bvars)

and bvi_megapattern = 
  function megapattern -> bvi_accu_megapattern (Identifier.Map.empty, Identifier.Map.empty) megapattern

and bound_accu_megapattern = fun (tycon_bvars, var_bvars) -> function
  | P (_pattern0) ->
      let (var_bvars) = bound_accu_pattern (var_bvars) _pattern0 in
      (tycon_bvars, var_bvars)
  | Q (_tycon0) ->
      let tycon_bvars = Tycon.AtomSet.add _tycon0 tycon_bvars in
      (tycon_bvars, var_bvars)

and export_megapattern : Tycon.AtomIdMap.t * Var.AtomIdMap.t -> megapattern -> Raw.megapattern = fun (tycon_im, var_im) -> function
  | P (_pattern0) ->
      Raw.P ((export_pattern (var_im)) _pattern0)
  | Q (_tycon0) ->
      Raw.Q (Tycon.AtomIdMap.lookup _tycon0 tycon_im)

and flatten_megapattern : megapattern -> Flat.megapattern = function
  | P (_pattern0) ->
      Flat.P (flatten_pattern _pattern0)
  | Q (_tycon0) ->
      Flat.Q (_tycon0)

and unflatten_megapattern : Flat.megapattern -> megapattern = function
  | Flat.P (_pattern0) ->
      P (unflatten_pattern _pattern0)
  | Flat.Q (_tycon0) ->
      Q (_tycon0)

and bound_free_accu_megapattern = fun (tycon_bvars, var_bvars) -> function
  | P (_pattern0) ->
      let (var_bvars) = bound_free_accu_pattern (var_bvars) _pattern0 in
      (tycon_bvars, var_bvars)
  | Q (_tycon0) ->
      let tycon_bvars = Tycon.AtomSet.add _tycon0 tycon_bvars in
      (tycon_bvars, var_bvars)

and aeq_megapattern : unit -> megapattern -> megapattern -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | P (_pattern0), P (_pattern1) ->
      aeq_pattern () _pattern0 _pattern1;
      ()
  | Q (_tycon0), Q (_tycon1) ->
      if not (Tycon.Atom.equal _tycon0 _tycon1) then raise (Invalid_argument "aeq_megapattern");
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_megapattern")

and freshen2_megapattern : Tycon.Subst.t * Var.Subst.t * Tycon.Subst.t * Var.Subst.t -> megapattern -> megapattern -> Tycon.Subst.t * Var.Subst.t * Tycon.Subst.t * Var.Subst.t = fun (tycon_env1, var_env1, tycon_env2, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | P (_pattern0), P (_pattern1) ->
      let (tycon_env1, var_env1, tycon_env2, var_env2) = freshen2_pattern (tycon_env1, var_env1, tycon_env2, var_env2) _pattern0 _pattern1 in
      (tycon_env1, var_env1, tycon_env2, var_env2)
  | Q (_tycon0), Q (_tycon1) ->
      let tycon_env1, tycon_env2 = Tycon.Subst.freshen2 _tycon0 tycon_env1 _tycon1 tycon_env2 in
      (tycon_env1, var_env1, tycon_env2, var_env2)
  | _, _ ->
      raise (Invalid_argument "freshen2_megapattern")

and import_megaexpr : tycon Identifier.Map.t * var Identifier.Map.t -> Raw.megaexpr -> megaexpr = fun (tycon_env, var_env) -> function
  | Raw.E (_expr0) ->
      E ((import_expr (var_env)) _expr0)
  | Raw.T (_ty0) ->
      T ((import_ty (tycon_env)) _ty0)

and subst_megaexpr : Tycon.Subst.t * Var.Subst.t -> megaexpr -> megaexpr = fun (tycon_env, var_env) -> function
  | E (_expr0) ->
      E ((subst_expr (var_env)) _expr0)
  | T (_ty0) ->
      T ((subst_ty (tycon_env)) _ty0)

and export_megaexpr : Tycon.AtomIdMap.t * Var.AtomIdMap.t -> megaexpr -> Raw.megaexpr = fun (tycon_m, var_m) -> function
  | E (_expr0) ->
      Raw.E ((export_expr (var_m)) _expr0)
  | T (_ty0) ->
      Raw.T ((export_ty (tycon_m)) _ty0)

and flatten_megaexpr : megaexpr -> Flat.megaexpr = function
  | E (_expr0) ->
      Flat.E (flatten_expr _expr0)
  | T (_ty0) ->
      Flat.T (flatten_ty _ty0)

and unflatten_megaexpr : Flat.megaexpr -> megaexpr = function
  | Flat.E (_expr0) ->
      E (unflatten_expr _expr0)
  | Flat.T (_ty0) ->
      T (unflatten_ty _ty0)

and free_megaexpr : megaexpr -> Tycon.AtomSet.t * Var.AtomSet.t = 
  function megaexpr -> free_accu_megaexpr (Tycon.AtomSet.empty, Var.AtomSet.empty) megaexpr

and equal_megaexpr : megaexpr -> megaexpr -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_megaexpr x1 x2

and aeq_megaexpr : unit -> megaexpr -> megaexpr -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | E (_expr0), E (_expr1) ->
      aeq_expr () _expr0 _expr1;
      ()
  | T (_ty0), T (_ty1) ->
      aeq_ty () _ty0 _ty1;
      ()
  | _, _ ->
      raise (Invalid_argument "aeq_megaexpr")

and free_accu_megaexpr = fun (tycon_fvars, var_fvars) -> function
  | E (_expr0) ->
      let (var_fvars) = free_accu_expr (var_fvars) _expr0 in 
      (tycon_fvars, var_fvars)
  | T (_ty0) ->
      let (tycon_fvars) = free_accu_ty (tycon_fvars) _ty0 in 
      (tycon_fvars, var_fvars)

and subst_megapatexpr : Tycon.Subst.t * Var.Subst.t -> megapatexpr -> megapatexpr = fun (tycon_ienv, var_ienv) -> function
  (_megapattern1, _megaexpr0) ->
    ((subst_megapattern (tycon_ienv, var_ienv)) _megapattern1, (subst_megaexpr (tycon_ienv, var_ienv)) _megaexpr0)

and bound_megapatexpr : megapatexpr -> Tycon.AtomSet.t * Var.AtomSet.t = 
  function megapatexpr -> bound_accu_megapatexpr (Tycon.AtomSet.empty, Var.AtomSet.empty) megapatexpr

and bound_free_megapatexpr : megapatexpr -> Tycon.AtomSet.t * Var.AtomSet.t * Tycon.AtomSet.t * Var.AtomSet.t = 
  function megapatexpr -> bound_free_accu_megapatexpr (Tycon.AtomSet.empty, Var.AtomSet.empty, Tycon.AtomSet.empty, Var.AtomSet.empty) megapatexpr

and equal_megapatexpr : megapatexpr -> megapatexpr -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_megapatexpr x1 x2

and import_megapatexpr = fun (tycon_ienv, var_ienv) -> function
  (_megapattern1, _megaexpr0) ->
    ((import_megapattern (tycon_ienv, var_ienv)) _megapattern1, (import_megaexpr (tycon_ienv, var_ienv)) _megaexpr0)

and bvi_accu_megapatexpr = fun (tycon_bvars, var_bvars) -> function
  (_megapattern1, _megaexpr0) ->
      let (tycon_bvars, var_bvars) = bvi_accu_megapattern (tycon_bvars, var_bvars) _megapattern1 in
      (tycon_bvars, var_bvars)

and bvi_megapatexpr = 
  function megapatexpr -> bvi_accu_megapatexpr (Identifier.Map.empty, Identifier.Map.empty) megapatexpr

and bound_accu_megapatexpr = fun (tycon_bvars, var_bvars) -> function
  (_megapattern1, _megaexpr0) ->
      let (tycon_bvars, var_bvars) = bound_accu_megapattern (tycon_bvars, var_bvars) _megapattern1 in
      (tycon_bvars, var_bvars)

and export_megapatexpr : Tycon.AtomIdMap.t * Var.AtomIdMap.t -> megapatexpr -> Raw.megapatexpr = fun (tycon_im, var_im) -> function
  (_megapattern1, _megaexpr0) ->
    ((export_megapattern (tycon_im, var_im)) _megapattern1, (export_megaexpr (tycon_im, var_im)) _megaexpr0)

and flatten_megapatexpr : megapatexpr -> Flat.megapatexpr = function
  (_megapattern1, _megaexpr0) ->
    (flatten_megapattern _megapattern1, flatten_megaexpr _megaexpr0)

and unflatten_megapatexpr : Flat.megapatexpr -> megapatexpr = function
  (_megapattern1, _megaexpr0) ->
    (unflatten_megapattern _megapattern1, unflatten_megaexpr _megaexpr0)

and bound_free_accu_megapatexpr = fun (tycon_bvars, var_bvars, tycon_ifvars, var_ifvars) -> function
  (_megapattern1, _megaexpr0) ->
      let (tycon_bvars, var_bvars) = bound_free_accu_megapattern (tycon_bvars, var_bvars) _megapattern1 in
      let (tycon_ifvars, var_ifvars) = free_accu_megaexpr (tycon_ifvars, var_ifvars) _megaexpr0 in
      (tycon_bvars, var_bvars, tycon_ifvars, var_ifvars)

and aeq_megapatexpr : unit -> megapatexpr -> megapatexpr -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_megapattern1, _megaexpr0), (_megapattern3, _megaexpr2) ->
      aeq_megapattern () _megapattern1 _megapattern3;
      aeq_megaexpr () _megaexpr0 _megaexpr2;
      ()

and freshen2_megapatexpr : Tycon.Subst.t * Var.Subst.t * Tycon.Subst.t * Var.Subst.t -> megapatexpr -> megapatexpr -> Tycon.Subst.t * Var.Subst.t * Tycon.Subst.t * Var.Subst.t = fun (tycon_env1, var_env1, tycon_env2, var_env2) -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_megapattern1, _megaexpr0), (_megapattern3, _megaexpr2) ->
      let (tycon_env1, var_env1, tycon_env2, var_env2) = freshen2_megapattern (tycon_env1, var_env1, tycon_env2, var_env2) _megapattern1 _megapattern3 in
      (tycon_env1, var_env1, tycon_env2, var_env2)

and create_megapatexpr : megapatexpr -> opaque_megapatexpr = 
  function body -> {
    megapatexpr_delayed = (Tycon.Subst.id, Var.Subst.id);
    megapatexpr = body
  }

and open_megapatexpr : opaque_megapatexpr -> megapatexpr = function abstraction ->
  let (tycon_delayed, var_delayed) = abstraction.megapatexpr_delayed in
  let body = abstraction.megapatexpr in
  let (tycon_bvars, var_bvars) = bound_megapatexpr body in
  let tycon_env = Tycon.Subst.freshen tycon_bvars tycon_delayed in
  let var_env = Var.Subst.freshen var_bvars var_delayed in
  let body = subst_megapatexpr (tycon_env, var_env) body in
  if not (Tycon.Subst.is_id tycon_delayed && Var.Subst.is_id var_delayed) then begin
    abstraction.megapatexpr_delayed <- (Tycon.Subst.id, Var.Subst.id);
    abstraction.megapatexpr <- body
  end;
  body

and open2_megapatexpr : opaque_megapatexpr -> opaque_megapatexpr -> megapatexpr * megapatexpr = fun x1 x2 -> 
  change_invalid_to_open2 open2i_megapatexpr x1 x2

and open2i_megapatexpr : opaque_megapatexpr -> opaque_megapatexpr -> megapatexpr * megapatexpr = fun abstraction1 abstraction2 ->
  let (tycon_delayed1, var_delayed1) = abstraction1.megapatexpr_delayed in
  let body1 = abstraction1.megapatexpr in
  let (tycon_delayed2, var_delayed2) = abstraction2.megapatexpr_delayed in
  let body2 = abstraction2.megapatexpr in
  let (tycon_env1, var_env1, tycon_env2, var_env2) = freshen2_megapatexpr (Tycon.Subst.id, Var.Subst.id, Tycon.Subst.id, Var.Subst.id) body1 body2 in
  let tycon_env1 = Tycon.Subst.union tycon_delayed1 tycon_env1 in
  let var_env1 = Var.Subst.union var_delayed1 var_env1 in
  let tycon_env2 = Tycon.Subst.union tycon_delayed2 tycon_env2 in
  let var_env2 = Var.Subst.union var_delayed2 var_env2 in
  let body1 = subst_megapatexpr (tycon_env1, var_env1) body1 in
  let body2 = subst_megapatexpr (tycon_env2, var_env2) body2 in
  if not (Tycon.Subst.is_id tycon_delayed1 && Var.Subst.is_id var_delayed1) then begin
    abstraction1.megapatexpr_delayed <- (Tycon.Subst.id, Var.Subst.id);
    abstraction1.megapatexpr <- body1
  end;
  if not (Tycon.Subst.is_id tycon_delayed2 && Var.Subst.is_id var_delayed2) then begin
    abstraction2.megapatexpr_delayed <- (Tycon.Subst.id, Var.Subst.id);
    abstraction2.megapatexpr <- body2
  end;
  body1, body2

and apply_megapatexpr = 
  fun (tycon_env, var_env) abstraction ->
    let (tycon_delayed, var_delayed) = abstraction.megapatexpr_delayed in {
      abstraction with megapatexpr_delayed = (Tycon.Subst.compose tycon_env tycon_delayed, Var.Subst.compose var_env var_delayed)
    }

and import_abs : tycon Identifier.Map.t * var Identifier.Map.t -> Raw.abs -> abs = fun (tycon_env, var_env) -> function
  (_megapatexpr0) ->
      let (tycon_bvars, var_bvars) = bvi_megapatexpr _megapatexpr0 in 
      let tycon_ienv = Tycon.Atom.mfreshb tycon_bvars tycon_env in
      let var_ienv = Var.Atom.mfreshb var_bvars var_env in
      let _megapatexpr0 = import_megapatexpr (tycon_ienv, var_ienv) _megapatexpr0 in
    (create_megapatexpr _megapatexpr0)

and subst_abs : Tycon.Subst.t * Var.Subst.t -> abs -> abs = fun (tycon_env, var_env) -> function
  (_megapatexpr0) ->
    (apply_megapatexpr (tycon_env, var_env) _megapatexpr0)

and export_abs : Tycon.AtomIdMap.t * Var.AtomIdMap.t -> abs -> Raw.abs = fun (tycon_m, var_m) -> function
  (_megapatexpr0) ->
      let megapatexpr = open_megapatexpr _megapatexpr0 in
      let (tycon_bvars, var_bvars) = bound_megapatexpr megapatexpr in
      let tycon_im = Tycon.AtomIdMap.add_set tycon_bvars tycon_m in
      let var_im = Var.AtomIdMap.add_set var_bvars var_m in
    (export_megapatexpr (tycon_im, var_im) megapatexpr)

and flatten_abs : abs -> Flat.abs = function
  (_megapatexpr0) ->
      let megapatexpr = open_megapatexpr _megapatexpr0 in
    (flatten_megapatexpr megapatexpr)

and unflatten_abs : Flat.abs -> abs = function
  (_megapatexpr0) ->
      let megapatexpr = unflatten_megapatexpr _megapatexpr0 in
    (create_megapatexpr megapatexpr)

and free_abs : abs -> Tycon.AtomSet.t * Var.AtomSet.t = 
  function abs -> free_accu_abs (Tycon.AtomSet.empty, Var.AtomSet.empty) abs

and equal_abs : abs -> abs -> bool = fun x1 x2 -> 
  change_invalid_to_bool aeq_abs x1 x2

and aeq_abs : unit -> abs -> abs -> unit = fun () -> 
  fun unlikely__arg1 unlikely__arg2 ->
  match unlikely__arg1, unlikely__arg2 with
  | (_megapatexpr0), (_megapatexpr1) ->
      let _megapatexpr0, _megapatexpr1 = open2i_megapatexpr _megapatexpr0 _megapatexpr1 in
      aeq_megapatexpr () _megapatexpr0 _megapatexpr1;
      ()

and free_accu_abs = fun (tycon_fvars, var_fvars) -> function
  (_megapatexpr0) ->
      let megapatexpr = open_megapatexpr _megapatexpr0 in
      let (tycon_bvars, var_bvars, tycon_ifvars, var_ifvars) = bound_free_accu_megapatexpr (Tycon.AtomSet.empty, Var.AtomSet.empty, Tycon.AtomSet.empty, Var.AtomSet.empty) megapatexpr in
      let tycon_fvars = Tycon.AtomSet.union tycon_fvars (Tycon.AtomSet.diff tycon_ifvars tycon_bvars) in
      let var_fvars = Var.AtomSet.union var_fvars (Var.AtomSet.diff var_ifvars var_bvars) in
      (tycon_fvars, var_fvars)

class map = object(self)

  method pattern : pattern -> pattern = function
  | PVar (_var0) ->
      self#pvar (_var0)
  | PPair (_pattern1, _pattern0) ->
      self#ppair (_pattern1, _pattern0)

  method pvar : var -> pattern = 
  function (_var0) -> 
      PVar (_var0)

  method ppair : pattern * pattern -> pattern = 
  function (_pattern1, _pattern0) -> 
      PPair ((self#pattern) _pattern1, (self#pattern) _pattern0)

  method ty : ty -> ty = function
  | TyCon (_tycon0) ->
      self#tycon (_tycon0)

  method tycon : tycon -> ty = 
  function (_tycon0) -> 
      TyCon (_tycon0)

  method expr : expr -> expr = function
  | EVar (_var0) ->
      self#evar (_var0)

  method evar : var -> expr = 
  function (_var0) -> 
      EVar (_var0)

  method patexpr : patexpr -> patexpr = function
  (_pattern1, _expr0) ->
    ((self#pattern) _pattern1, (self#expr) _expr0)

  method megapattern : megapattern -> megapattern = function
  | P (_pattern0) ->
      self#p (_pattern0)
  | Q (_tycon0) ->
      self#q (_tycon0)

  method p : pattern -> megapattern = 
  function (_pattern0) -> 
      P ((self#pattern) _pattern0)

  method q : tycon -> megapattern = 
  function (_tycon0) -> 
      Q (_tycon0)

  method megaexpr : megaexpr -> megaexpr = function
  | E (_expr0) ->
      self#e (_expr0)
  | T (_ty0) ->
      self#t (_ty0)

  method e : expr -> megaexpr = 
  function (_expr0) -> 
      E ((self#expr) _expr0)

  method t : ty -> megaexpr = 
  function (_ty0) -> 
      T ((self#ty) _ty0)

  method megapatexpr : megapatexpr -> megapatexpr = function
  (_megapattern1, _megaexpr0) ->
    ((self#megapattern) _megapattern1, (self#megaexpr) _megaexpr0)

  method abs : abs -> abs = function
  (_megapatexpr0) ->
    (create_megapatexpr (self#megapatexpr (open_megapatexpr _megapatexpr0)))

end

class [ 'accumulator ] fold = object(self)

  method pattern : 'accumulator -> pattern -> 'accumulator = fun accu -> function
  | PVar (_var0) ->
      self#pvar accu (_var0)
  | PPair (_pattern1, _pattern0) ->
      self#ppair accu (_pattern1, _pattern0)

  method pvar : 'accumulator -> var -> 'accumulator = fun accu -> 
  function (_var0) -> 
      accu

  method ppair : 'accumulator -> pattern * pattern -> 'accumulator = fun accu -> 
  function (_pattern1, _pattern0) -> 
      let accu = (self#pattern) accu _pattern1 in
      let accu = (self#pattern) accu _pattern0 in
      accu

  method ty : 'accumulator -> ty -> 'accumulator = fun accu -> function
  | TyCon (_tycon0) ->
      self#tycon accu (_tycon0)

  method tycon : 'accumulator -> tycon -> 'accumulator = fun accu -> 
  function (_tycon0) -> 
      accu

  method expr : 'accumulator -> expr -> 'accumulator = fun accu -> function
  | EVar (_var0) ->
      self#evar accu (_var0)

  method evar : 'accumulator -> var -> 'accumulator = fun accu -> 
  function (_var0) -> 
      accu

  method patexpr : 'accumulator -> patexpr -> 'accumulator = fun accu -> function
  (_pattern1, _expr0) ->
      let accu = (self#pattern) accu _pattern1 in
      let accu = (self#expr) accu _expr0 in
      accu

  method megapattern : 'accumulator -> megapattern -> 'accumulator = fun accu -> function
  | P (_pattern0) ->
      self#p accu (_pattern0)
  | Q (_tycon0) ->
      self#q accu (_tycon0)

  method p : 'accumulator -> pattern -> 'accumulator = fun accu -> 
  function (_pattern0) -> 
      let accu = (self#pattern) accu _pattern0 in
      accu

  method q : 'accumulator -> tycon -> 'accumulator = fun accu -> 
  function (_tycon0) -> 
      accu

  method megaexpr : 'accumulator -> megaexpr -> 'accumulator = fun accu -> function
  | E (_expr0) ->
      self#e accu (_expr0)
  | T (_ty0) ->
      self#t accu (_ty0)

  method e : 'accumulator -> expr -> 'accumulator = fun accu -> 
  function (_expr0) -> 
      let accu = (self#expr) accu _expr0 in
      accu

  method t : 'accumulator -> ty -> 'accumulator = fun accu -> 
  function (_ty0) -> 
      let accu = (self#ty) accu _ty0 in
      accu

  method megapatexpr : 'accumulator -> megapatexpr -> 'accumulator = fun accu -> function
  (_megapattern1, _megaexpr0) ->
      let accu = (self#megapattern) accu _megapattern1 in
      let accu = (self#megaexpr) accu _megaexpr0 in
      accu

  method abs : 'accumulator -> abs -> 'accumulator = fun accu -> function
  (_megapatexpr0) ->
      let accu = self#megapatexpr accu (open_megapatexpr _megapatexpr0) in
      accu

end
