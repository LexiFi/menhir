open Sig
open Positions
open Misc
  
module Make 
  (KindInferencer: KindInferencer)
  (Constraint: Constraint)
  (TypingExceptions: TypingExceptions 
   with module MultiEquation = Constraint.MultiEquation
  ) :
  TypingEnvironment 
  with module KindInferencer = KindInferencer
  and module Constraint = Constraint
  and module TypingExceptions = TypingExceptions 
  =
struct
  
  module TypingExceptions = TypingExceptions
  module Constraint = Constraint
  module KindInferencer = KindInferencer
  open KindInferencer
  open Constraint
  open TypingExceptions
  open Constraint.MultiEquation
  open Algebra


  (** {2 Typing environment} *)

  (* Use a basic implementation. *)
  open Env

  (** [typing_info] denotes information collected during the user-defined
    type constructor analysis. *)

  (* The following information is stored for each type constructor:
     - its kind ;
     - its associated term (a type variable actually) ;
     - if it is an algebraic datatype, the list of its datatype 
     constructors.
  *)
  type algebraic_datatype = 
      (dname * crterm) list
     
  type type_info = 
      KindInferencer.t * variable * algebraic_datatype option ref
        
  let as_type_constructor ((_, v, _) as x) =
    if variable_kind v = Constant 
      then x
      else raise Not_found

  let as_type_variable (_, v, _) = 
    v

  (* The following information is stored for each datatype constructor:
     - the "external" type variables (alphas) ;
     - the "internal" type variables (betas) ;
     - its type ; 
     
     variable list * ...
  *)
  type data_constructor = crterm

  (** [environment] denotes typing information associated to identifiers. *)
  type environment = 
      {
        type_info        : type_info Env.t;
        data_constructor : data_constructor Env.t;
        type_abbrev      : crterm Env.t;
      }

  let empty_environment = 
    {
      type_info        = Env.empty;
      data_constructor = Env.empty;
      type_abbrev      = Env.empty;
    }

  let union_type_variables env1 env2 =
    { env1 with type_info = Env.concat env1.type_info env2.type_info }
      
  let add_type_variable env t (k, v) = 
    { env with type_info = Env.add env.type_info t (k, v, ref None) }

  let add_type_variables var_env env = 
    let var_env = List.map (fun (s, v) -> (s, v)) var_env in
    { env with type_info = Env.concat var_env env.type_info }

  let add_type_constructor env t x = 
    { env with type_info = Env.add env.type_info t x }

  let add_type_abbrev env t term = 
    { env with type_abbrev = Env.add env.type_abbrev t term }
    
  let add_data_constructor env t typ = 
    { env with data_constructor = Env.add env.data_constructor t typ }

  (** [lookup_typcon ?pos env t] retrieves typing information about
    the type constructor [t]. *)
  let lookup_typcon ?pos env t =
    try 
      as_type_constructor (Env.lookup env.type_info t)
    with Not_found -> raise (UnboundTypeIdentifier ((pos_or_undef pos), t))

  (** [find_type_abbrev env t] searches for a crterm associated to that name *)
  let find_type_abbrev env t =
    just_try (fun () -> Env.lookup env.type_abbrev t)
    
  (** [find_typcon env t] looks for typing information related to
      the type constructor [t] in [env]. *)
  let find_typcon env t =
    just_try (fun () -> as_type_constructor (Env.lookup env.type_info t))

  (** [lookup_type_variable env v] looks for typing information related to
      the type variable [v] in [env]. *)
  let lookup_type_variable ?pos env k = 
    try
      as_type_variable (Env.lookup env.type_info k)
    with Not_found -> raise (UnboundTypeVariable ((pos_or_undef pos), k))

  (* The kind inferencer wants a view on the environment that
     concerns only the kinds. *)
  let as_kind_env env = 
    let env = ref env in
    let read id = 
      try 
        match Env.lookup (!env).type_info id with
          | (k, _, _) -> k
      with Not_found -> raise (UnboundConstructor (undefined_position, 
                                                   id))
    in
    let update i k = 
      env := add_type_variable (!env) i (k, variable Flexible ())
    in
      (read : string -> KindInferencer.t), 
      (update : string -> KindInferencer.t -> unit)

  (* Some accessors. *)
  let typcon_kind env t = 
    proj1_3 (lookup_typcon env t)

  let typcon_variable env t = 
    proj2_3 (lookup_typcon env t)

  let as_fun tenv name = 
    TVariable (match find_typcon tenv name with
      | None -> lookup_type_variable tenv name
      | Some (_, v, _) -> v
    )

  let as_env env varlist = 
    List.fold_left 
      (fun env (n, v) -> add_type_variable env n (fresh_kind (), v))
      empty_environment varlist

  (** [is_typcon env t] check if there exists a type constructor whose
    name is [t]. *)
  let is_typcon env t = 
    (find_typcon env t) <> None
      
  (** [filter_tycon_name tenv names] checks that [names] does not contain
    a name already used by a type constructor. *)
  let filter_tycon_name tenv =
    List.filter (notf (function v -> match variable_name v with
                           None -> false 
                         | Some name -> is_typcon tenv name))

  let add_type_and_kind_variables denv tenv =
    add_type_variables 
      (List.map (fun (n, term) -> (n, (fresh_kind (), term, ref None))) denv)
      tenv
  

  (** [tycon_name_conflict tyconv_env env] checks if a type constructor is not
    overwritten by a type variable. *)
  let tycon_name_conflict pos env (fqs, (denv: (tname * variable) list)) =
    
    let _ = 
      List.iter (fun (n,v) ->
          match find_type_abbrev env n with 
          | Some n' -> raise (InvalidTypeVariableIdentifier (pos, n))
          | None -> ()
        ) denv;
        in
      
    match filtered_lookup (is_typcon env) (assoc_proj1 denv) with
      | Some n -> raise (InvalidTypeVariableIdentifier (pos, n))
      | None -> (fqs, denv)


  (** [lookup_datacon env k] looks for typing information related to
      thlookup_datacone data constructor [k] in [env]. *)
  let lookup_datacon ?pos env (k: Algebra.Ast.tname) = 
    try
      Env.lookup env.data_constructor k
    with Not_found -> raise (UnboundDataConstructor ((pos_or_undef pos), k))

  (* Deprecated : old invariant
  let rigid_args rt = 
     List.fold_left (fun acu -> 
                      function 
                          TVariable v -> 
                            if variable_kind v = Rigid then v :: acu
                            else acu
                        | _ -> acu) []
      (tycon_args rt)
  *)    
  (* Deprecated : old invariant -
  let fresh_datacon_scheme pos tenv k localvars =
    let (kvars, kt) = lookup_datacon tenv k in 
    let _ = 
      if localvars <> [] && List.length localvars <> List.length kvars then
        raise (InvalidNumberOfTypeVariable pos)
    in
    let rt = result_type (as_fun tenv) kt in
    let alphas = rigid_args rt in
    let betas = List.filter (fun v -> not (List.memq v alphas)) kvars in 
    let kind v = if List.memq v alphas then Flexible else Rigid in
    let fresh_kvars, denv =
      let mkvar ?name v = variable (kind v) ?name () in
      if localvars = [] then
        List.map mkvar kvars, []
      else 
        let fvs = List.map2 (fun name -> mkvar ~name) localvars kvars in
          (fvs, List.combine localvars (List.map (fun v -> TVariable v) fvs))
    in
    let fresh_kvars_assoc = List.combine kvars fresh_kvars in
    let fresh_kvars, denv = tycon_name_conflict pos tenv (fresh_kvars, denv)
    in
    let (alphas, betas) = 
      List.map (fun v -> List.assq v fresh_kvars_assoc) alphas,
      List.map (fun v -> List.assq v fresh_kvars_assoc) betas         
    in
      (alphas, betas, change_arterm_vars fresh_kvars_assoc kt,
       denv)
  *)   
       
  (* Deprecated : old invariant -   
    Check that all the tycon arguments are distinct rigid variables.  
  let is_regular_datacon_scheme tenv kvars kt =
    let rt = result_type (as_fun tenv) kt in
    let rigid_args = rigid_args rt in
      
      List.for_all (fun v -> List.memq v kvars) rigid_args 
      && List.length rigid_args == List.length kvars
  *)
      
  (** [get_datacon_arity pos tenv k] return the arity associated in [tenv] to 
      the constructor name [k] *)   
  let get_datacon_arity pos tenv k =      
    let kt = lookup_datacon ~pos:pos tenv k in 
    let quantifiers, rho = cut_heading_gen kt in
    let kt_args = arg_types (as_fun tenv) rho in 
    List.length kt_args
 
(* TODO_ERRORS replace failwith by exceptions *)

  (** [get_data_construction_crterm term] returns the constructor and the argument
      of the application of a dataconstructor. *)
  let get_data_construction_crterm term =         
    let rec extract_from_crterm acu = function
      | TVariable v -> v, acu
      | TTerm (App (term1, term2)) -> extract_from_crterm (term2::acu) term1
      | TTerm _ -> failwith "get_data_construction: construction should only have TTerm App" 
      | TGen _ -> failwith "get_data_construction: argument should not have heading TGen" 
     in      
    extract_from_crterm [] term

    
  (** [check_variance_of_vars variance_of_vars term] checks that all occurences in [term]
      of variables of the list [variance_of_vars] respect the given variance. *)  
  let check_variance_of_vars variance_of_vars term = 
    
    let rec check_rec local_variance = function  
    
      | TVariable v -> (
            try 
              let variance = List.assq v variance_of_vars in
              if variance <> InVariant && variance <> local_variance
                then raise (ErrorOnDataConstructorDefinition (InvalidDataConstructorBadVariance v))
            with Not_found -> ()
          )
          
      | TTerm (App _) as t ->
          let cons, args = get_data_construction_crterm t in
          let variance = variance_of_constructor cons in
          if List.length args <> List.length variance 
            then failwith "check_variance_of_vars: variance and args don't have the same length";
          let args_and_vars = List.combine args variance in
          List.iter (fun (ai,vari) -> 
                       check_rec (product_variance local_variance vari) ai
                    ) args_and_vars

      | TGen (var, subterm) -> check_rec local_variance subterm
      
      | TTerm (Var _)
         -> failwith "TTerm (Var _) should not appear in containment"
      |TTerm (RowUniform _) | TTerm (RowCons _)
         -> failwith "Row are not supported with polymorphic types" (* TODO_ROW *)
       
    in
    check_rec CoVariant term
     
  
  (** [check_regular_datacon_scheme tenv variance cons_var kt] checks that
      the construction scheme is correct. *)      
  let check_regular_datacon_scheme tenv variance cons_var kt =      
    let quantifiers, rho = cut_heading_gen kt in
    let k_args = arg_types tenv rho in  
    let k_result = result_type tenv rho in
    let rt_cons, rt_args = get_data_construction_crterm k_result in
    let rt_vars = List.map (function 
        | TVariable v -> v   
        | _ -> raise (ErrorOnDataConstructorDefinition InvalidDataConstructorResult)
      ) rt_args in
    
    (* check the constructor of the return type *) 
    if rt_cons != cons_var 
      then raise (ErrorOnDataConstructorDefinition InvalidDataConstructorBadResult);
        
    (* check the variables of the return type are all distinct *)  
    let _ = List.fold_left 
            (fun acu v -> 
              if List.memq v acu 
                then raise (ErrorOnDataConstructorDefinition InvalidDataConstructorNotDistinct);
              v::acu)
            [] rt_vars in
            
    (* check contents equality of quantifiers and vars, testing double inclusion *) 
    if not (List.for_all (fun v -> List.memq v quantifiers) rt_vars 
         && List.for_all (fun v -> List.memq v rt_vars) quantifiers ) 
      then raise (ErrorOnDataConstructorDefinition InvalidDataConstructorQuantified);
 
    (* check for variance *)    
    if List.length variance <> List.length rt_vars
      then failwith "intern kind error in check_regular_datacon_scheme";
    let variance_of_vars = List.combine rt_vars variance in
    List.iter (check_variance_of_vars variance_of_vars) k_args
    
      
          
  (** [find_algebraic_datatypes env k] looks for all the data
    constructor that are related to the data constructor [k]. *)
  let rec find_algebraic_datatypes env k = 
    let ts = filter env.type_info
      (fun (_, _, r) -> match !r with
           Some l -> List.mem k l
         | _ -> false)
    in
      match ts with
          [ (_,_, r) ] -> unSome (!r)
        | _ -> assert false

  (** [fresh_vars kind pos env vars] allocates fresh variables from a
    list of names [vars], checking name clashes with type constructors. *)
  let fresh_vars kind pos env vars =
    let vs = variable_list_from_strings ~pos:pos (fun v -> (kind, Some v)) vars in
    let (fqs, denv) = tycon_name_conflict pos env vs in
      (fqs, 
       (List.map (fun (n, v) -> (n, (fresh_kind (), v, ref None))) denv))

  (** [fresh_flexible_vars] is a specialized allocator for flexible
    variables. *)
  let fresh_flexible_vars = 
    fresh_vars Flexible

  (** [fresh_rigid_vars] is a specialized allocator for rigid
    variables. *)
  let fresh_rigid_vars = 
    fresh_vars Rigid

  let fresh_unnamed_rigid_vars pos env vars = 
    let rqs, denv = variable_list ~pos:pos Rigid vars in
      rqs, 
      List.map (fun (n, (v : variable)) -> (n, (fresh_kind (), v, ref None))) denv
      
end
