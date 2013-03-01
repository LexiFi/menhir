(* $Id$ *)

(** This module implements type inference. *)

open Positions
open Misc
open Sig
open MiniConstraint
open MiniAlgebra
open MiniMultiEquation
open MiniTypingEnvironment
open MiniTypingExceptions
open MiniEst
open MiniAst

(** {2 Inference} *)
type fragment = 
    {
      gamma       : (crterm * position) StringMap.t;
      vars        : variable list;
      ex_vars     : variable list;
      tconstraint : tconstraint;
      denv        : (tname * variable) list
    }

let empty_fragment = 
  {
    gamma       = StringMap.empty;
    vars        = [];
    ex_vars     = [];
    tconstraint = CTrue undefined_position;
    denv        = []
  }
    
let join_fragment pos f1 f2 = 
  {
    gamma = 
      (try 
        StringMap.strict_union f1.gamma f2.gamma
      with StringMap.Strict x -> raise (NonLinearPattern (pos, x)));
    vars        = f1.vars @ f2.vars;
    ex_vars     = f1.ex_vars @ f2.ex_vars;
    tconstraint = f1.tconstraint ^ f2.tconstraint;
    denv        = f1.denv @ f2.denv
  }
  
(** [infer_pat_constraint ?flat tenv p t] generates the constraints 
    related to the matching of algebraic datatype. If [flat] is set, 
    the constraint is specialized for unguarded algebraic datatype. *)
let rec infer_pat_constraint tenv p t =
  let infer_pat = infer_pat_constraint tenv in
    match p with
        
      | PZero p 
      | PVar (p, _) 
      | PWildcard p ->
          CTrue p

      | PPrimitive (pos, p) ->
          (t <?= type_of_primitive (as_fun tenv) p) pos

      | PAlias (pos, name, p) ->    
          (name <? t) pos ^ infer_pat p t 
          
      | PTypeConstraint (pos, p, (fqs,ityp)) ->
            ex ~pos:pos fqs ( (t <?= ityp) pos )
          ^ infer_pat p t
          

      | POr (pos, ps) | PAnd (pos, ps) ->
          conj (List.map (fun p -> infer_pat p t) ps)

      | PData (pos, localvars, k, ps) ->
         assert (localvars = []); (* todo *)
         match t with 
           | TVariable _ -> 
              let kt = instance_crterm pos (lookup_datacon ~pos:pos tenv k) in 
              let quantifiers, rest = cut_heading_gen kt in
              List.iter set_flexible quantifiers;
              let rt = result_type (as_fun tenv) rest in
              let ats = arg_types (as_fun tenv) rest in
              if (List.length ps <> List.length ats) then
                raise (NotEnoughPatternArgts pos);
                
               ex ~pos:pos quantifiers ( 
                  (t <?= rt) pos 
                 ^ conj (List.map2 infer_pat ps ats)
               )
          | TTerm _ ->    
              let kt = instance_crterm pos (lookup_datacon ~pos:pos tenv k) in 
              let quantifiers, rest = cut_heading_gen kt in
              List.iter set_flexible quantifiers;
              let rt = result_type (as_fun tenv) rest in
              let ats = arg_types (as_fun tenv) rest in
              if (List.length ps <> List.length ats) then
                raise (NotEnoughPatternArgts pos);
              
              CTrue pos
              
          | TGen _ -> raise (InvalidAnnotationForDataConstructor (pos,k,t)) 
          
        
        
(** [infer_pat_fragment p t] generates a fragment that represents the 
    information gained by a success when matching p. *)
and infer_pat_fragment tenv p t =
  let join pos = List.fold_left (join_fragment pos) empty_fragment in
  let rec infpat t = function

    | PZero pos -> 
        { empty_fragment with tconstraint = CFalse pos }
          
    | PPrimitive (pos, _) 
    | PWildcard pos | POr (pos, []) | PAnd (pos, []) ->
        empty_fragment
          
    | PVar (pos, name) ->
        { empty_fragment with 
            gamma = StringMap.singleton name (instance_crterm pos t, pos);
        } 
        
        
    | PAlias (pos, name, p) ->
        let fragment = infpat t p in 
          { fragment with 
              gamma = StringMap.strict_add name (t, pos) fragment.gamma 
          }

    | POr (pos, ps) ->
        assert false

    | PAnd (pos, ps) ->
        join pos (List.map (infpat t) ps)

    | PTypeConstraint (pos, p, eterm) ->
        infpat t p
        
    | PData (pos, localvars, k, ps) -> 
        assert (localvars = []); (* todo *)
        match t with 
        | TVariable _ -> 
            let kt = instance_crterm pos (lookup_datacon ~pos:pos tenv k) in 
            let quantifiers, rest = cut_heading_gen kt in
            List.iter set_flexible quantifiers;
            let rt = result_type (as_fun tenv) rest in
            let ats = arg_types (as_fun tenv) rest in
            if (List.length ps <> List.length ats) 
                then raise (NotEnoughPatternArgts pos);
            
            let fragment = join pos (List.map2 infpat ats ps) in
            { fragment with       
                vars = quantifiers @ fragment.vars;
                tconstraint = fragment.tconstraint ^ (t <?= rt) pos;
            }
        | TTerm _ ->    
            let cons_given, args_given =  
              try get_data_construction_crterm t     (* TODO_ERROR => should return specific exceptions *)
              with _ -> raise (MatchingDataConstructorWithTGen pos) (* TODO_ERROR: USE OTHER ERROR *)
              in
            let kt = instance_crterm pos (lookup_datacon ~pos:pos tenv k) in 
            let quantifiers, rest = cut_heading_gen kt in
            List.iter set_flexible quantifiers;
            let rt = result_type (as_fun tenv) rest in
            let ats = arg_types (as_fun tenv) rest in
            let cons, args = get_data_construction_crterm rt in  
            
            if not (are_equivalent cons_given cons)
              then raise (MatchingDataConstructorWithTGen pos); (* TODO_ERROR: USE OTHER ERROR *)
            if (List.length ps <> List.length ats) 
                then raise (NotEnoughPatternArgts pos);
            assert (List.length args = List.length args_given);
             
            let args_var = List.map (function TVariable v -> v | _ -> assert false) args in
            let map_args_var = List.combine args_var args_given in
            let ats_given = List.map (change_arterm_vars map_args_var) ats in
            
            let fragment = join pos (List.map2 infpat ats_given ps) in
            { fragment with       
                vars = fragment.vars;
                tconstraint = fragment.tconstraint;
            }
            
        | TGen _ -> raise (MatchingDataConstructorWithTGen pos) (* TODO_ERROR: USE OTHER ERROR *)
      
        
  in
    infpat t p

(** Constraint contexts. *)
type context =
    (crterm, variable) type_constraint -> (crterm, variable) type_constraint

let rec dvacc accu = function
  | PVar (_, name) ->
      StringSet.add name accu

  | PWildcard _ ->
      accu

  | PAlias (_, name, p) ->
      dvacc (StringSet.add name accu) p

  | PTypeConstraint (_, p, _) ->
      dvacc accu p

  | PData (_, _, dname, ps) ->
      List.fold_left dvacc accu ps

  | _ -> assert false

(** [dv p] returns the set of program variables defined (bound) by
    the pattern [p]. At the same time, it ensures that the pattern
    is well-formed. *)
let dv p =
  dvacc StringSet.empty p

  
(** [intern_data_constructor] *)
let intern_data_constructor pos variance cons_var env_info dcon_info = 
  let (tenv , acu, let_env) = env_info
  and (pos, dname, ityp) = dcon_info in
  let _ = 
    try check_regular_datacon_scheme (as_fun tenv) variance cons_var ityp     
    with ErrorOnDataConstructorDefinition case  
         -> raise (InvalidDataConstructorDefinition (pos, dname, case))
    in 
   (add_data_constructor tenv dname (instance_crterm pos ityp),
   (dname, (instance_crterm pos ityp)) :: acu, 
   StringMap.add dname (ityp, pos) let_env)

            
          
(** [infer_binding tenv b] examines a binding [b], updates the
    typing environment if it binds new types or generates 
    constraints if it binds values. 
*)
let rec infer_binding tenv b =

  match b with
   
   | TypeDec (pos, tds) ->
        List.fold_left 
          (fun (tenv, c) (pos', ikind, variance, cons_var, name, def) -> 
                      
            match def with
            
            | DAlgebraic ds ->
          
               (* Insert the type constructor into the environment. *)
               let ids_def = ref None in
               let tenv = add_type_constructor tenv name 
                            (ikind, cons_var, ids_def)
               and c = fun c' -> c (
                 CLet ([Scheme (pos, [cons_var], [], c', StringMap.empty)],
                       CTrue pos))
               in
               
               (* Algebraic datatype definition. *)
               let (tenv, ids, let_env) = 
                 List.fold_left
                   (intern_data_constructor pos variance cons_var) 
                   (tenv, [], StringMap.empty)
                   ds
               in
               ids_def := Some ids;
               
               (* Insert the data constructors in the constraint
                  context.*)

               let c = fun c' -> 
                 c (CLet ([Scheme (pos', [], [], CTrue pos', 
                                   let_env)],
                          c')) 
               in
                 (tenv, c)
                   
            | DAbbrev typ -> 
                
                (tenv, c)  
          )
          (tenv, fun c -> c)
          tds
          
          
    | BindValue (pos, vdefs) ->

        (** [infer_vdef pos tenv (pos, qs, p, e, annot)] returns the constraint
          related to a value definition. *)
        let infer_vdef pos tenv (pos, rqs, p, e, (fqs, ityp)) =
            
            let fragment = infer_pat_fragment tenv p ityp in 
            let tenv' = add_type_and_kind_variables fragment.denv tenv in
           
            Scheme (pos, rqs, fqs @ fragment.vars, 
                      fragment.tconstraint    
                    ^ infer_pat_constraint tenv' p (instance_crterm pos ityp)
                    ^ infer_expr tenv' e (instance_crterm pos ityp) ,
                    fragment.gamma);
            in
    
        let schemes = List.map (infer_vdef pos tenv) vdefs in
          tenv, (fun c -> CLet (schemes, c))

          
    | BindRecValue (pos, vdefs) ->
        (* vdef = position * tname list * pattern * expression * existential_crterm *)

        let all_rqs, all_fqs, all_cons, rec_header = List.fold_left (
            fun (all_rqs, all_fqs, all_cons, rec_header)
                (pos, rqs, pat, expr, (fqs, ityp)) ->
                
                let func_name = match pat with
                    | PVar (_, name) -> name
                    | _ -> failwith "letrec pattern should be a name"
                    in
                (* BIN let local_header = StringMap.singleton func_name (ityp, pos) in
                let cons = CLet ([ Scheme (pos, [], [], CTrue pos, local_header) ],
                                  infer_expr tenv expr (instance_crterm pos ityp) 
                                ) in *)
                let cons = infer_expr tenv expr (instance_crterm pos ityp) in
                
                rqs @ all_rqs, 
                fqs @ all_fqs,
                cons::all_cons,
                StringMap.strict_add func_name (instance_crterm pos ityp, pos) rec_header
            )
            ([], [], [], StringMap.empty)
            vdefs in
             
        let rec_header' = StringMap.map (fun (term, pos) -> 
                            (instance_crterm pos term, pos)) rec_header in
            
        let cons = CLet ([Scheme(pos, [], [], CTrue pos, rec_header)], conj all_cons) in
        let scheme = Scheme (pos, all_rqs, all_fqs, cons, rec_header') in
        tenv, (fun c -> CLet ([scheme], c))
    
       
      (* DEPRECATED : many changes required because of Internalization 
        let schemes1, rqs2, fqs2, h2, c2, c1 =
          List.fold_left 
            (fun (schemes1, rqs2, fqs2, h2, c2, c1) (pos, qs, p, e, annot) ->
                    
               (match p with
                   PVar _ -> ()
                 | _ -> raise (RecursiveDefMustBeVariable pos));
               
               let rvs, rtenv = fresh_rigid_vars pos tenv qs in
               let tenv' = add_type_variables rtenv tenv in


                 match explicit_or_implicit p e with 
                   | Implicit (name, e) ->

                       let v = variable ~pos:pos Flexible () in
                       let (t : crterm) = TVariable v in
                         
                         schemes1,
                       rvs @ rqs2,
                       v :: fqs2,
                       StringMap.add name (t, pos) h2,
                       infer_expr tenv' e t ^ c2,
                       c1

                   | Explicit (name, term, e) ->

                       intern_scheme pos tenv name qs typ :: schemes1,
                       rqs2,
                       fqs2,
                       h2,
                       c2,
                       fl rvs (infer_expr tenv' e term) 
                       ^ c1

                   | _ -> assert false

            ) ([], [], [], StringMap.empty, CTrue pos, CTrue pos) vdefs in
          
          tenv, 
        fun c -> CLet (schemes1,
                       CLet ([ Scheme (pos, rqs2, fqs2, 
                                       CLet ([ monoscheme h2 ], c2), h2) ],
                             c1 ^ c)
                      )*)
   
(* non polymorphic match: bound variables cannot be polymorphic                              
and infer_match pos tenv e t' clauses t =
  infer_expr tenv e t' ^
    conj 
    (List.map 
       (fun (pos, p, e) -> 
          let fragment = infer_pat_fragment tenv p t' 
          and pat_constraint = infer_pat_constraint tenv p t'  in
          let tenv = add_type_and_kind_variables fragment.denv tenv in
            pat_constraint 
          ^ fl ~pos fragment.ex_vars (
              CLet ([ Scheme (pos, [], fragment.vars, 
                               fragment.tconstraint, 
                               fragment.gamma) ],
                     infer_expr tenv e t) 
              )
          )
       clauses)
*)

and annot_from_crterm pos t =
  let s = MiniElaboration.shape_of_type t in
  MiniElaboration.annot_of_shape pos s 

  
and infer_match pos tenv e fqs t' clauses t =
  let match_env = StringMap.add "__match" (t', pos) StringMap.empty  in
  CLet ([ Scheme (pos, [], fqs, infer_expr tenv e (instance_crterm pos t'), match_env) ],
    conj (List.map 
       (fun (pos, p, e) -> 
          let fqs', sigma = annot_from_crterm pos (instance_crterm pos t') in
          let fragment = infer_pat_fragment tenv p sigma 
          and pat_constraint = infer_pat_constraint tenv p sigma in
          let tenv = add_type_and_kind_variables fragment.denv tenv in
          let cons = pat_constraint ^ fragment.tconstraint ^ CInstance (pos, "__match", sigma) in
          fl ~pos fragment.ex_vars (
              CLet ([ Scheme (pos, [], fragment.vars @ fqs', cons, fragment.gamma) ],
                     infer_expr tenv e t) 
            )
       ) clauses
    )
  )




(** [infer_expr tenv d e t] generates a constraint that guarantees that [e]
    has type [t]. It implements the constraint generation rules for
    expressions. It may use [d] as an equation theory to prove coercion 
    correctness. *)
and infer_expr tenv ?nb_apps expr (t : crterm) =
  (**)
  
  let nb_apps = default 0 nb_apps in
  
  match t with 
  
  | TGen (v, subterm) -> fl [v] (infer_expr tenv ~nb_apps:nb_apps expr subterm)
  
  | _ ->
  
    match expr with
    
    | EError (pos, pat) -> CFalse pos

    | EExists (pos, fqs, expr) ->
        ex ~pos:pos fqs (infer_expr tenv ~nb_apps:nb_apps expr t)

    | EForall (pos, rqs, e) ->
        fl ~pos:pos rqs (infer_expr tenv e t)
        
    | EVar (pos, name) ->
        (name <? t) pos

    | EAssertFalse pos ->
        CTrue pos

    | ELambda (pos, p, e) ->
      
        let get_cons t1 t2 =
          let fragment = infer_pat_fragment tenv p t1 in
          let tenv = add_type_and_kind_variables fragment.denv tenv in
            ex ~pos:pos fragment.vars ( 
              CLet (
                (* Bind the variables of [p] via 
                   a monomorphic [let] constraint. *)
                [ monoscheme fragment.gamma ],       
                (* Require [t1] to be a valid type for [p]. *)
                infer_pat_constraint tenv p t1 
                ^ fragment.tconstraint
                  (* Require [t2] to be a valid type for [e]. *)
                ^ infer_expr tenv e t2     
              )
            )
            in
              
        begin match t with 
        
        | TTerm (App (TTerm (App (v, t1)), t2)) when v = the_arrow (as_fun tenv) ->
            get_cons t1 t2
    
        | TVariable _ -> 
            exists ~pos:pos (fun t1 ->
               exists ~pos:pos (fun t2 ->
                     get_cons t1 t2
                  ^ (arrow (as_fun tenv) t1 t2 =?= t) pos 
               )
            )
                   
         | _ -> assert false
         
        end
          
    | EAppAnnoted (pos, e1, e2, (fqs, ityp)) ->
        
        let nb_apps' = succ nb_apps in
        ex ~pos:pos fqs (
             infer_expr ~nb_apps:nb_apps' tenv e1 (arrow (as_fun tenv) ityp t)
           ^ infer_expr ~nb_apps:0 tenv e2 (instance_crterm pos ityp)
           )
          
    | EBinding (_, b, e) ->
        snd (infer_binding tenv b) (infer_expr tenv e t)
          
    | ETypeConstraint (pos, e, (fqs,ityp)) ->
           ex ~pos:pos fqs ( (t <?= ityp) pos )
        ^ infer_expr tenv ~nb_apps:nb_apps e t
        

    | EImpredicativity (pos, expr, fqs, ityp1, ityp2) ->
        ex ~pos:pos fqs (
            infer_expr tenv ~nb_apps:nb_apps expr ityp1
          ^ ((instance_crterm pos ityp1) <?* (instance_crterm pos ityp2)) pos
          ^ (ityp2 <?= t) pos
          )        
    
    | EMatchAnnoted (pos, e, (fqs, ityp), clauses) -> 
        infer_match pos tenv e fqs ityp clauses t
    
    | EDCon (pos, (k : dname)) ->        
        (* check the number of arguments for the constructor *)
        let arity = get_datacon_arity pos tenv k in
        if arity <> nb_apps
          then raise (InvalidTypeConstructorUse (pos, k, nb_apps, arity));
        (* return the constraint *)
        (k <? t) pos
        
      
    | EPrimApp (pos, c, args) -> 
        exists_list args 
          (fun xs ->
             let xts = snd (List.split xs) in
             let ct = 
               List.fold_left (fun acu x -> arrow (as_fun tenv) x acu) t xts 
             in
               (ct =?= type_of_primitive (as_fun tenv) c) pos ^
                 CConjunction (List.map (curry (infer_expr tenv)) xs))

    | ERecordEmpty (pos) -> 
        (t =?= uniform (abs (as_fun tenv))) pos
          
    | ERecordExtend (pos, bindings, exp) ->
        exists_list bindings 
          (fun xs -> 
             exists ~pos:pos
               (fun x -> 
                  let labels = List.map extract_label_from_binding bindings 
                  and ts = assoc_proj2 xs in
                  let typed_labels = List.combine labels ts in
                    CConjunction 
                      ([
                         (t =?= record_type pos tenv typed_labels x) pos;
                         infer_expr tenv exp x
                       ] @
                         List.map (infer_label tenv) xs
                      ))
          )

    | ERecordUpdate (pos, e1, label, e2) ->

        exists3 
          (fun x x' y ->
             let r = 
               record_constructor (as_fun tenv)
                 (rowcons label (pre (as_fun tenv) x') y)
             and r' = 
               record_constructor (as_fun tenv)
                 (rowcons label x y)
             in
               CConjunction 
                 [ 
                   infer_expr tenv e1 r'; 
                   infer_expr tenv e2 x';
                   (t =?= r) pos
                 ]
          )

    | ERecordAccess (pos, e1, label) ->
        
        exists ~pos:pos (fun x -> 
             exists ~pos:pos (fun y -> 
                       let r = 
                         record_constructor (as_fun tenv) 
                           (rowcons label (pre (as_fun tenv) x) y)
                       in
                         infer_expr tenv e1 r
                         ^ (t =?= x) pos))

and extract_label_from_binding (name, _) = name

and record_type pos tenv xs x =
    let xs = List.map (fun (b, t) -> (b, pre (as_fun tenv) t)) xs in
      (* FIXME: this test should be done by kind inference. *)
      (match are_distinct (fst (List.split xs)) with
           Some x -> raise (MultipleLabels (pos, x))
         | _ -> ());
      record_constructor (as_fun tenv) (n_rowcons xs x) 

and extend_record_binding pos tenv l t =
  exists3 
    (fun x x' y -> 
       (n_arrows tenv [ rowcons l x y; x' ] (rowcons l x' y) =?= t) pos
    )
    
and infer_label tenv ((_, exp), t) =
  infer_expr tenv exp t
    
(** [infer e] determines whether the expression [e] is well-typed
    in the empty environment. *)
let infer tenv e =
  exists (infer_expr tenv e)
    
(** [bind b] generates a constraint context that describes the
    top-level binding [b]. *)
let bind env b =
  infer_binding env b  

let infer_program env = 
  List.fold_left (fun (env, acu) b ->
                    let env, f = bind env b in
                      env, (fun c -> acu (f c)))
    (env, fun c -> c)


(* FIXME: beautify the following piece of code.  *)

(* DEPRECATED BUT STILL USED BY CONSTRAINT PARSER *)
let init_env () = 
  
  try 
  
    let builtins = 
      Algebra.init_builtin_env 
        (fun ?name () -> variable Constant ?name:name () )
    in
    
    let kind_env = 
      let variance = [] in 
      List.fold_left (fun env (n, k) ->
                        add_type_constructor env n
                          (k, 
                           variable_for_typecon ~name:n variance, 
                           ref None)
                     )
        empty_environment
        MiniKindInferencer.initial_kind_env 
    in
    let init_ds variance cons_var acu ds =  
      if ds = [] then None, acu 
      else 
        let (env, acu, let_env) as r = 
          List.fold_left 
            (fun acu (d, ty) -> 
               let tenv, _, _ = acu in
               let ityp = MiniTypes.intern undefined_position tenv ty in
               intern_data_constructor undefined_position variance cons_var acu 
                 (undefined_position, d, ityp)
            ) acu ds
        in
          (Some acu, r)
    in
    
    let (init_env, acu, let_env) = 
      List.fold_left 
        (fun ((env, dvs, let_env) as acu) 
           (n, (kind, v, ds)) -> 
             let r = ref None in
             let ikind, variance = MiniKindInferencer.intern_kind (as_kind_env env) kind in
             let cons_var = variable_for_typecon ~name:n variance in
             let env = add_type_constructor env n 
               (ikind,
                cons_var,
                r) in
             let (dvs, acu) = init_ds variance cons_var (env, dvs, let_env) ds in
               r := dvs;
               acu
        )
        (kind_env, [], StringMap.empty)
        (List.rev builtins)
    in
    let vs = 
      Env.fold_left (fun vs (n, (_, v, _)) -> v :: vs)
        []
        init_env.type_info
    in
      (fun c -> 
         CLet ([ Scheme (undefined_position, vs, [], 
                         CLet ([ Scheme (undefined_position, [], [],
                                         CTrue undefined_position, 
                                         let_env) ],
                               c),
                         StringMap.empty) ], 
               CTrue undefined_position)), 
     init_env
     
   with ErrorOnDataConstructorDefinition _ 
      -> failwith "built_in data_constructors are bugged" 
     
 
      
let remove_init_context = function
    CLet ([ Scheme (pos, rqs, fqs, CLet (_, c), h) ], CTrue pos') -> 
      c
  | _ -> assert false

let generate_constraint_task = "generate-constraint"
  
let generate_constraint (b : MiniAst.program) =
  let c, env = (fun c -> c), empty_environment in
  c ((snd (infer_program env b)) (Sig.CDump Positions.undefined_position))

let register_tasks elaborate_task =
  Processing.register
    generate_constraint_task ([], ignore)
    [ [ elaborate_task ] ]
    (fun t -> generate_constraint (List.hd t)) 
    (const true)
    
 
