(* $Id$ *)

(** This module handles types from the user's syntax to the
    internal representation of the inference engine.
*)

open Positions
open Misc
open Sig
open MiniKindInferencer
open MiniConstraint
open MiniAlgebra
open MiniMultiEquation
open MiniTypingEnvironment
open MiniTypingExceptions
open MiniPst

(** {2 From user's syntax to internal term representation} *)
        
let rec intern' pos tenv = function
      
    | TypVar (pos, name) -> 
        begin
        match find_type_abbrev tenv name with
          | Some term -> instance_crterm pos term
          | None -> as_fun tenv name
        end
        
    | TypApp (pos, t, typs) ->
        let iargs = List.map (intern' pos tenv) typs in
          app (intern' pos tenv t) iargs 
          
    | TypGen (pos, tnames, typ) -> 
        let rqs, rtenv = fresh_rigid_vars pos tenv tnames in 
        let tenv' = add_type_variables rtenv tenv in
        gen rqs (intern' pos tenv' typ)
  
    | TypRowCons (pos, attributes, t) ->
        let typed_labels = 
          List.map (fun (l, t) -> l, intern' pos tenv t) attributes
        in
          n_rowcons typed_labels (intern' pos tenv t)

    | TypRowUniform (pos, t) ->
        uniform (intern' pos tenv t)

        
(** [intern tenv typ] converts the type expression [typ] to a type.
    The environment [tenv] maps type identifiers to types. *)
let rec intern pos tenv ty = 
  let kind_env = as_kind_env tenv in
  let kind = KindInferencer.check pos kind_env ty (mkstar kind_env) in 
    intern' pos tenv ty     

(** [intern_quantifiers pos tenv vs] returns (fqs, tenv') *)    
let intern_quantifiers pos tenv vs = 
    let (fqs, denv) = fresh_flexible_vars pos tenv vs in 
    fqs, add_type_variables denv tenv
      
(** [intern_existential_type pos tenv (vs,typ)] converts the type expression [typ] 
    to a type, existentially quantified by variable of names given by vs,
    returns (fqs, typ) *)      
let intern_existential_type pos tenv (vs, typ) =
    let (fqs, tenv') = intern_quantifiers pos tenv vs in
    fqs, intern pos tenv' typ

 
(* [intern_let_env] is used by constraint parser only *)     
let intern_let_env pos tenv rs fs = 
  let fqs, rtenv = fresh_flexible_vars pos tenv fs in
  let rqs, rtenv' = fresh_rigid_vars pos tenv rs in
    rqs, fqs, add_type_variables (rtenv @ rtenv') tenv 

    
(*------------------------------------------------------------------*)

(* DEPRECATED : useless

let rec type_of_result t = 
  match t with
    | TypApp (_, TypVar (_, "->"), [ t1; t2]) -> t2
    |  t -> t
         
let rec type_of_last_result t = 
  match t with
    | TypApp (_, TypVar (_, "->"), [ t1; t2 ]) -> type_of_last_result t2
    |  t -> t

let rec type_of_args t =
  let rec chop acu = function
    | TypApp (_, TypVar (_, "->"), [ t1; t2 ]) -> chop (t1 :: acu) t2
    | t -> acu 
  in List.rev (chop [] t)
     

let tycon_arity env t = 
  kind_arity (as_kind_env env) (typcon_kind env t)

let variables_of_typ = 
  let rec vtyp acu = function
    | TypVar (_, x) -> StringSet.add x acu
    | TypApp (_, t, ts) -> List.fold_left vtyp (vtyp acu t) ts
    | TypGen (_, tnames, t) -> assert false ??? No spec...
    | TypRowCons (_, attributes, t) -> 
        List.fold_left vtyp (vtyp acu t) (assoc_proj2 attributes)
    | TypRowUniform (_, x) -> 
        vtyp acu x
  in
    vtyp StringSet.empty 

let arrow tenv = 
  arrow (typcon_variable tenv)

let tycon tenv t =
  app (lookup_type_variable tenv t) 
    
*)

(* DEPRECATED : many changes required because of Internalization
 
type recursive_value_definition_kind =
  | Implicit of name * expression
  | Explicit of name * typ * expression
  | NotPVar
        
 [extract_type] examines an expression and looks for a sufficiently 
    explicit type annotation. If it finds one, it returns the type annotation, 
    together with the expression (deprived of its annotation). 
    Otherwise, it raises  [Not_found]. 
let rec extract_type = function
    
  | ETypeConstraint (_, e, typ) ->
      typ, e

  | ELambda (pos, PTypeConstraint (pos', p, typ1), e2) ->
      let typ2, e2 = extract_type e2 in
        TypApp (pos', TypVar (pos', "->"), [typ1; typ2]), 
        ELambda (pos, p, e2)

  | _ ->
      raise Not_found
  
 [explicit_or_implicit] examines a value definition and determines whether
    it carries an explicit type annotation. It optionally checks that the 
    left-hand side is a variable. 
let rec explicit_or_implicit p e =
  match p with
    | PTypeConstraint (pos, p, typ) ->
        explicit_or_implicit p (ETypeConstraint (pos, e, typ))
          
    | PVar (_, name) -> (
        try
          let typ, e = extract_type e in
            Explicit (name, typ, e)
        with Not_found ->
          Implicit (name, e)
      )

    | _ -> NotPVar
    
    
 [intern_scheme tenv name qs typ] produces a type scheme
    that binds [name] to [forall qs.typ].
let intern_scheme pos tenv name qs typ =
  let fqs, rtenv = fresh_flexible_vars pos tenv qs in
    Scheme (pos, [], fqs, CTrue pos, 
            StringMap.singleton name 
              ((intern pos (add_type_variables rtenv tenv) typ),
               pos))

*)