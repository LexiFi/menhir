(* $Id$ *)

(* TEMPORARY clarifier terminologie `multi-equation' et `variable' *)

(** This module provides a constraint solver. *)

open Positions
open Misc
open Sig

module Make
    (Constraint: Constraint)
    (Unifier: Unifier with module MultiEquation = Constraint.MultiEquation)
= struct
  
  module Unifier = Unifier
  open Constraint
  open Unifier
  open MultiEquation
  open Algebra
  open Rank
  open TypingExceptions

  type tconstraint = (crterm, variable) type_constraint

  type solving_step = 
    | Init of tconstraint
    | Solve of tconstraint
    | Solved of tconstraint
    | UnifyTerms of crterm * crterm
    | UnifyVars of variable * variable
    | Generalize of int * variable list
    | Generalized of variable list
    | ChoppedHeader of crterm StringMap.t 
    | ReturnHeader of crterm StringMap.t
    | ImpredOccurence of variable * crterm
    | ImpredSolving of variable list
    | ImpredSubstitution of (variable * crterm) list

  type environment =
    | EEmpty
    | EEnvFrame of environment * string * crterm

  let environment_as_list e = 
    let rec conv acu = function
    | EEmpty -> acu
    | EEnvFrame (env, name, v) -> 
          conv ((name, v)::acu) env
    in
      conv [] e

  (** [lookup name env] looks for a definition of [name] within
      the environment [env]. *)
  let rec lookup pos name = function
    | EEnvFrame (env, name', scheme) ->
        if name = name' then scheme
        else lookup pos name env
    | EEmpty ->
        raise (UnboundIdentifier (pos, name))

  (* [generalize] *)

  let generalize old_pool young_pool =

    (* We examine the variables in the young pool and immediately drop those
       which have become aliases for other (old or young) variables, using
       [UnionFind.redundant]. The variables that remain are sorted by rank
       using a simple bucket sort mechanism. (Recall that every variable in
       the young pool must have rank less than or equal to the pool's number.)
       They are also marked as ``young'', so as to be identifiable in constant
       time. *)

    let young_number = number young_pool in

    let sorted =
      Array.create (young_number + 1) [] in

    let young =
      Mark.fresh() in

     (* TRUNK: COMPLEXITE MODIFIED 
    List.iter 
      (fun v ->
         let desc = UnionFind.find v in
           desc.mark <- young;
           let rank = desc.rank in
             try
               sorted.(rank) <- 
               if not (List.exists (UnionFind.equivalent v) sorted.(rank)) 
                 then v :: sorted.(rank)
                 else sorted.(rank)
             with Invalid_argument _ -> 
               failwith (Printf.sprintf "Out of bound when generalizing %s/%s"
                           (string_of_int rank)
                           (string_of_int (Array.length sorted)))
      ) (inhabitants young_pool);
    *)

    let already_sorted =
      Mark.fresh() in

    List.iter 
      (fun v ->
         let desc = UnionFind.find v in
         if not (Mark.same desc.mark already_sorted) then
         (
           desc.mark <- already_sorted;
           let rank = desc.rank in
             try
               sorted.(rank) <- 
               if not (List.exists (UnionFind.equivalent v) sorted.(rank)) 
                 then v :: sorted.(rank)
                 else sorted.(rank)
             with Invalid_argument _ -> 
               failwith (Printf.sprintf "Out of bound when generalizing %s/%s"
                           (string_of_int rank)
                           (string_of_int (Array.length sorted)))
         )
      ) (inhabitants young_pool);

     List.iter (set_mark young) (inhabitants young_pool);


    (* Next, we update the ranks of the young variables that remain. One goal
       is to ensure that if [v1] is dominated by [v2], then the rank of [v1]
       is less than or equal to the rank of [v2], or, in other words, that
       ranks are nonincreasing along any path down the structure of terms.
       The second goal is to ensure that the rank of every young variable is
       exactly the maximum of the ranks of the variables that it dominates,
       if there are any.

       The process consists of several depth-first traversals of the forest
       whose entry points are the young variables. Traversals stop at old
       variables. Roughly speaking, the first goal is achieved on the way
       down, while the second goal is achieved on the way back up.

       During each traversal, every visited variable is marked as such, so as
       to avoid being visited again. To ensure that visiting every variable
       once is enough, traversals whose starting point have lower ranks must
       be performed first. In the absence of cycles, this enforces the
       following invariant: when performing a traversal whose starting point
       has rank [k], every variable marked as visited has rank [k] or less
       already. (In the presence of cycles, this algorithm is incomplete and
       may compute ranks that are slightly higher than necessary.) Conversely,
       every non-visited variable must have rank greater than or equal to
       [k]. This explains why [k] does not need to be updated while going
       down. *)

    let visited =
      Mark.fresh() in

    for k = 0 to young_number do
      let rec traverse v =
        let desc = UnionFind.find v in

        (* If the variable is young and was not visited before, we immediately
           mark it as visited (which is important, since terms may be cyclic).
           If the variable has no structure, we set its rank to [k]. If it has
           some structure, we first traverse its sons, then set its rank to the
           maximum of their ranks. *)
        if Mark.same desc.mark young then begin
          desc.mark <- visited;
          desc.rank <- match desc.structure with
          | Some term ->
              fold (fun son accu ->
                        max (traverse son) accu
                   ) term outermost
          | _ ->
              k
        end
        (* If the variable isn't marked ``young'' or ``visited'', then it must
           be old. Then, we update its rank, but do not pursue the computation
           any further. *)

        else if not (Mark.same desc.mark visited) then begin
          desc.mark <- visited;
          if k < desc.rank then
            desc.rank <- k
        end;
        (* If the variable was visited before, we do nothing. *)
        
        (* In either case, we return the variable's current (possibly updated)
           rank to the caller, so as to allow the maximum computation above. *)

        desc.rank

      in
        try
          Misc.iter traverse sorted.(k)
        with Invalid_argument _ -> 
          (* The invariant is broken. *)
          failwith "Out of bound in traverse"

    done;

    (* The rank of every young variable has now been determined as precisely
       as possible.

       Every young variable whose rank has become strictly less than the
       current pool's number may be safely turned into an old variable. We do
       so by moving it into the previous pool. In fact, it would be safe to
       move it directly to the pool that corresponds to its rank. However, in
       the current implementation, we do not have all pools at hand, but only
       the previous pool.

       Every young variable whose rank has remained equal to the current
       pool's number becomes universally quantified in the type scheme that is
       being created. We set its rank to [none]. *)

    for k = 0 to young_number - 1 do
      try
        List.iter (register old_pool) sorted.(k)
      with Invalid_argument _ -> 
        (* The invariant is broken. *)
        failwith "Out of bound in young refresh."
    done;

     List.fold_left (fun acu v ->
                 let desc = UnionFind.find v in
                   if desc.rank < young_number then (
                     register old_pool v;
                     acu
                   ) else (
                     if desc.kind = Flexible then desc.kind <- Rigid;
                     v::acu
                   )
                ) [] sorted.(young_number)
              

              
  (** [distinct_variables vl] checks that the variables in the list [vl]
      belong to distinct equivalence classes and that their structure is
      [None]. In other words, they do represent distinct (independent)
      variables (as opposed to nonvariable terms). *)
  exception DuplicatedMark of Mark.t
  let distinct_variables pos vl =
    let m = Mark.fresh() in
      try
        List.iter (fun v ->
                     let desc = UnionFind.find v in
                       match desc.structure with
                         | Some _ ->
                             raise (CannotGeneralize (pos, v))
                         | _ ->
                             if Mark.same desc.mark m then
                               raise (DuplicatedMark m);
                             desc.mark <- m
                  ) vl
      with DuplicatedMark m -> 
        let vl' = List.filter (has_mark m) vl in
          raise (NonDistinctVariables (pos, vl')) 

  (** [generic_variables vl] checks that every variable in the list [vl]
      has rank [none]. *)
  let generic_variables pos vl =
    List.iter (fun v ->
                 if variable_rank v <> Rank.none then (
                   raise (CannotInferShape (pos, v)))
              ) vl
              
              
(*-----------------------------------------------------------------------*)            
             
     
  (** [chop_monomorphic_parts chop_a_term term ] return a term' equal to term
      but where monomorphic subtrees have been replaced by a single variable. 
      A subtree is monomorphic when it contains no variables quantified by TGen,
      that is when it contains no Rigid variables. *)
      
  let chop_monomorphic_parts chop_a_term term = 
    
    let mark_bound = Mark.fresh() in

    let rec chop_rec = function
      | TVariable v as term -> term, not (has_mark mark_bound v)
      | TTerm t -> 
          let t_infos = map chop_rec t in
          let is_monomorphic = 
            fold (fun (_, is_mono) acu -> acu && is_mono) t_infos true in
          let t_subchopped = 
            TTerm (map (fun (subterm, _) -> subterm) t_infos) in
          let new_term = 
            if is_monomorphic 
              then TVariable (chop_a_term t_subchopped) 
              else t_subchopped in
          new_term, is_monomorphic
      | TGen (v, subterm) -> 
          assert (not (has_mark mark_bound v));
          set_mark mark_bound v;
          let subchopped, _ = chop_rec subterm in
          (TGen (v, subchopped)), false
      in  
      
    let result, is_monomorphic = chop_rec term in
    result



                        
  (** [generalize_term young_number sigma] return (gen gen_vars sigma), where gen_vars
      are the leaves variable of the fully expanded term sigma which rank is equal 
      to [young_number] *)                
  let generalize_term young_number sigma =
        
    let gen_vars = ref [] in
    
    let mark_gen = Mark.fresh() in
     
    let rec expand_rec = function
    
      | TVariable v -> 
          let local_gen_vars, sigma' = 
            extract_generalizable_crterm young_number mark_gen v in       
          gen_vars := local_gen_vars @ (!gen_vars);
          sigma'
          
      | TTerm t -> TTerm (map (fun term -> expand_rec term) t)

      | TGen (v, term) -> TGen (v, expand_rec term)
     
      in 
    
    let new_sigma = expand_rec sigma in 
    !gen_vars, new_sigma

    
  (** [normalize_crterm term] return a term' equal to [term] and in normal form *)          
  let normalize_crterm term = 
  
    let to_place_id = -10 in
    let counter = ref 1000 in
    let get_next_id () =
      incr counter;
      !counter in
  
    let rec normalize_rec = function
      | TVariable v -> 
          let desc = UnionFind.find v in
          if desc.rank = to_place_id
            then desc.rank <- get_next_id();
          TVariable v
          
      | TTerm t -> TTerm (map normalize_rec t)
      
      | TGen _ as term -> 
        
          let quantifiers, subterm = cut_heading_gen term in
          (* CHECK *) List.iter (fun v -> if not (has_rank_none v)
                                          then failwith "normalize_rec1") quantifiers;
          List.iter (set_rank to_place_id) quantifiers;
          let norm_subterm = normalize_rec subterm in
          let ranked_q = List.map (fun v -> v, variable_rank v) quantifiers in
          let ranked_q = List.filter (fun (v,r) -> r <> to_place_id) ranked_q in
          let compare (v1,r1) (v2,r2) = - (Rank.compare r1 r2) in
          let ranked_q = List.sort compare ranked_q in
          let keep_q = List.map (fun (v,r) -> set_rank_none v; v) ranked_q in
          gen keep_q norm_subterm  
      in
      
    normalize_rec term 

  
(*------------------------------------------------------------------------*)            

(* TODO_ERRORS : replace failwith by exceptions ? *)
 
  (** [get_data_construction term] match term with a structure (app cons args)*)          
  let get_data_construction term =
    let rec extract_from_vars acu v =
      match variable_structure v with
        | None -> v, acu
        | Some (App (v1,v2)) -> extract_from_vars (v2::acu) v1
        | Some (Var v') -> failwith "found a structure with Var inside it"
        | Some _ -> failwith "get_data_construction: structure should be an app"
      in            
    let rec extract_from_crterm acu = function
      | TVariable v -> 
          let cons, var_args = extract_from_vars [] v in
          cons, (List.map tvar var_args) @ acu          
      | TTerm (App (term1, term2)) -> extract_from_crterm (term2::acu) term1
      | TTerm _ -> failwith "get_data_construction: construction should only have TTerm App" 
      | TGen _ -> failwith "get_data_construction: argument should not have heading TGen" 
     in      
    extract_from_crterm [] term 


  (** [datacon_constraint contain pos op cons1 cons2 args1 args2] solve 
      comparison constraint between (app cons1 args1) and (app cons2 args2) *)          
  let datacon_constraint contain pos op cons1 cons2 args1 args2 =

    let c = 
      let variances = variance_of_constructor cons1 in
        if    List.length args1 <> List.length args2 
           || List.length args1 <> List.length variances
        then CFalse pos
        else (
        
          let args = List.combine args1 args2 in  
          
          let comparing variance = 
              match op with
                | ContainEqualImpred substitution -> ContainEqualImpred substitution
                | ContainEqual -> ContainEqual 
                | _ -> match variance with
                      | InVariant -> ContainEqual
                      | CoVariant -> op
                      | ContraVariant -> oppose_contain op
             in  
             
          let args_constraint = 
              List.map2 
                (fun variance (a1, a2) -> contain (comparing variance) a1 a2)        
                variances
                args
              in
          conj args_constraint 
        )
       in
       
    (TVariable cons1 =?= TVariable cons2) pos ^ c

  
  (** [datacon_constraint_one_var contain pos op term1 v term2] solve comparison
      constraint between  TTerm App _ as term1 and  Tvariable v as term2      *)   
  let datacon_constraint_one_var contain pos op term1 v term2 =    
    let datacon_constraint' = datacon_constraint contain pos op in
      
      if variable_kind v <> Flexible
        then raise (NotAbleToUnifyMonoAndPoly (pos, term2, term1));
      let cons1, args1 = get_data_construction term1 in
      let cons2, args2 = get_data_construction term2 in
      
      if variable_kind cons2 = Constant then (
         datacon_constraint' cons1 cons2 args1 args2
         
      ) else ( 
        let get_var () = variable Flexible ~pos:pos () in            
        let cons2 = get_var () in      
        let vargs2 = List.map (fun _ -> get_var()) args1 in
        let args2 = List.map tvar vargs2 in
        let term2' = app (tvar cons2) args2 in                   
        
        ex ~pos:pos (cons2::vargs2) (
            (term2 =?= term2') pos
          ^ datacon_constraint' cons1 cons2 args1 args2  
        )
            
     )                
            
(*------------------------------------------------------------------------*)            
 
(* Functions used to store in the rank field some information about impredicativity,
   used by find_impredivative and solve_contain_impred *)
          
let const_offset_impred = 10000 

let get_impredicative_id i =
  - i - const_offset_impred

let get_id_of_impredicative r =
  - ( const_offset_impred + r )
                 
let is_impredicative v =
  variable_rank v <= -const_offset_impred 
  
    
(*------------------------------------------------------------------------*)            
                
  (* [solve] *)

  let solve tracer env pool c =
    let final_env = ref env in
    let rec solve env pool c =
      let pos = cposition c in
        try
          solve_constraint env pool c
        with Inconsistency -> raise (TypingError pos)

    and solve_constraint env pool c =
      tracer (Solve c);
      (match c with
        
         | CFalse pos ->
(*           raise (NonExhaustiveMatch (pos, pat)) *)
             assert false
   
         | CTrue _ ->
             ()
               
         | CDump _ ->
             final_env := env
                     
         | CContainment (pos, comparator, term1, term2, prepared) -> 
            let prepare_for_containment_solving pos pool term =
              let chopped = chop_monomorphic_parts (chop pool) term in
              let instanced = instance_crterm pos chopped in
              let normalized = normalize_crterm instanced in
              normalized
              in
            let term1', term2' =
              if prepared 
                then term1, term2
                else map_pair (prepare_for_containment_solving pos pool) (term1, term2)
              in
            solve_containment pos pool comparator term1' term2'           
            
         | CConjunction cl ->
             List.iter (solve env pool) cl

         (* Deprecated : old invariant
           
         | CLet ([ Scheme (_, [], fqs, c, _) ], CTrue _) ->
            This encodes an existential constraint. In this restricted
                case, there is no need to stop and generalize. The code
                below is only an optimization of the general case. 
             TEMPORARY traiter un cas plus general que celui-ci?
            
             List.iter (introduce pool) fqs;
             solve env pool c
          *)

         | CLet (schemes, c2) ->
         
             let env' = List.fold_left 
                          (fun env' scheme -> (
                             concat env' (solve_scheme env pool scheme)
                           )) env schemes in (
                 solve env' pool c2)
                                               
         | CInstance (pos, name, sigma2) -> 
 
             let sigma1 = lookup pos name env in
             let c2 = (sigma1 <?= sigma2) pos in
             solve env pool c2;
           
         | CDisjunction cs -> 
             assert false
               
      );
      tracer (Solved c)
      
    and solve_scheme env pool = function
      
      | Scheme (pos, rqs, fqs, c1, header) ->
               
            let check = false in            
            
            if check then (
              List.iter (fun v -> if variable_kind v <> Flexible then assert false) fqs;  
              List.iter (fun v -> if not (is_rigid v) then assert false) rqs;  
            );                          
              
            (* there is a special treatment for impredicative variables *)
            let fqs = List.filter (fun v -> not (is_impredicative v)) fqs in
                   
            (* introduce all other variables in a new local pool *)
            let vars = rqs @ fqs in
            let pool' = new_pool pool in
            let cur_rank = number pool' in
            List.iter (introduce pool') vars;
                      
            (* chop_monomorphic_parts of terms in the header *)
            let header1 = StringMap.map 
               (fun (sigma, _) -> chop_monomorphic_parts (chop pool') sigma) 
               header in
            tracer (ChoppedHeader header1); 

            (* solve the scheme constraint in the local pool *)
            solve env pool' c1; 
            
            (* check rqs are distinct and have no structure *)
            distinct_variables pos rqs; 
            
            (* detect cyclic terms in the header and also in the fqs 
               we do not need to check rqs since we just checked that
               they haven't got any structure *)
            StringMap.iter (fun name sigma ->   
                try non_cyclic_term sigma 
                with CyclicVariable var_root
                  -> raise (CyclicStructureInferred (Some name, var_root))
                ) header1;
            List.iter (fun v ->   
                try non_cyclic_variable v 
                with CyclicVariable v_loop
                  -> raise (CyclicStructureInferred (variable_name v_loop, v_loop))
                ) fqs;
            
             
            (* Find which variables can be generalized among the current pool *)               
            tracer (Generalize (cur_rank, vars));
            let generalized = generalize pool pool' in   
            tracer (Generalized generalized);
  
          
            (* Universally quantify at the head of each term in the header 
              all the variables that occur in that term and that are generalized *)
            let header2 = StringMap.map (generalize_term cur_rank) header1 in 
            let header3 = StringMap.map (fun (gen_vars, sigma) ->
                  gen gen_vars sigma
                ) header2 in
            tracer (ReturnHeader header3);
              
            if check then (
              List.iter (fun v -> if variable_rank v <> cur_rank then assert false) generalized;  
            );
                
            (* All generalized variables have their rank turned to none, 
               since they can be in TGen in some terms of the header *)
            List.iter set_rank_none generalized;                       
       
            (* Check that all rqs variables belong to the generalized set *) 
            generic_variables pos rqs; 
            
            (* return the header *)            
            header3

    
    and solve_containment pos pool comparator term1 term2 = 
         
        (* these functions are used to avoid repreparing the terms *)
        let contain comp t1 t2 = CContainment (pos, comp, t1, t2, true) in 
        let (<?!=) = contain ContainLess in
        let (=?!=) = contain ContainEqual in
        
        match comparator, term1, term2 with  
        
        (* Special impredicativity containment *)
        
        | ContainLessImpred, term1, term2 
          -> solve_contain_impred pos pool term1 term2 
    
        | ContainEqualImpred substitution, TVariable alpha, (_ as sigma) 
            when is_impredicative alpha
          -> find_impredivative substitution alpha sigma
            
        | ContainEqualImpred substitution, (_ as sigma), TVariable alpha 
            when is_impredicative alpha
          -> find_impredivative substitution alpha sigma
        
        (* End of solve_containement *)
        
        | _, TVariable t1, TVariable t2 
          -> unify_terms pos pool t1 t2

        (* Through TGen *)
        
        | (ContainEqual | ContainEqualImpred _) as op, TGen (var, term), TGen (var', term') 
          ->
            let v = variable ~pos:pos Rigid () in
            set_flexible var; 
            set_flexible var'; 
            let c = 
              fl ~pos:pos [v] (
                ex ~pos:pos [var;var'] (
                    ((tvar v) =?= (tvar var)) pos
                  ^ ((tvar v) =?= (tvar var')) pos
                  ^ contain op term term'
                )
              ) in
            solve env pool c
        
        | (ContainEqual | ContainEqualImpred _), (_ as t'), (TGen _ as t)  
        | (ContainEqual | ContainEqualImpred _), (TGen _ as t), (_ as t') 
          -> raise (CannotEqualize (pos, t, t'))

        | ContainLess, (_ as term'), TGen (var, term) 
        | ContainMore, TGen (var, term), (_ as term') 
          ->
            let c = fl ~pos:pos [var] (term' <?!= term) in
            solve env pool c
        
        | ContainLess, TGen (var, term), (_ as term')
        | ContainMore, (_ as term'), TGen (var, term)
          -> 
            set_flexible var;
            let c = ex ~pos:pos [var] (term  <?!= term') in
            solve env pool c 
        
        (* Through data constructors *)
                
        | op, (TTerm (App _) as term1), (TTerm (App _) as term2) 
          ->
            let cons1, args1 = get_data_construction term1 in
            let cons2, args2 = get_data_construction term2 in
            let c = datacon_constraint contain pos op cons1 cons2 args1 args2 in
            solve env pool c
            
        | op, (TTerm (App _) as term1), (TVariable v as term2) 
          ->   
            let c = datacon_constraint_one_var contain pos op term1 v term2 in
            solve env pool c
                      
        | op, (TVariable v as term2), (TTerm (App _) as term1)  
          ->  
            let op' = oppose_contain op in
            let c = datacon_constraint_one_var contain pos op' term1 v term2 in
            solve env pool c
         
        (* Errors *)
                                                
        | _, TTerm (Var _), _ |  _, _ , TTerm (Var _) -> 
            failwith "TTerm (Var _) should not appear in containment"
   
        | _, _ , TTerm (RowUniform _) | _, TTerm (RowUniform _) , _
        | _, _ , TTerm (RowCons _)    | _, TTerm (RowCons _) , _ 
           ->  failwith "Row are not supported with polymorphic types" (* TODO_ROW *)
             
                  
      
    and find_impredivative substitution alpha sigma = 
      tracer (ImpredOccurence (alpha, sigma));
      let index = get_id_of_impredicative (variable_rank alpha) in
      substitution.(index) <- sigma::(substitution.(index))          

        
    and solve_contain_impred pos pool sigma1 sigma2 =
         
        let alphas, rho1 = cut_heading_gen sigma1 in
        let betas, rho2 = cut_heading_gen sigma2 in
        
        tracer (ImpredSolving alphas);
        
        let nb_alphas = List.length alphas in
        let substitutions = Array.make nb_alphas [] in
        
        list_iteri (fun i -> set_rank (get_impredicative_id i)) alphas;
        List.iter set_flexible alphas;
            
        (* Note that the rank of variables alphas will not be 
           overwritten by the 'let' constraint *)
        let c = 
          fl ~pos:pos betas (
            ex ~pos:pos alphas (
              CContainment (pos, ContainEqualImpred substitutions, 
                            rho1, rho2, true) 
            )
          ) in
        solve env pool c;       
      
        List.iter set_rank_none alphas;
        
        let substitutions = Array.to_list substitutions in
        let substitution, constraints = List.split (List.map (function
            | [] -> failwith "solve_contain_impred: missing occurences"
            | repr::rest -> 
                repr, conj (List.map (fun sigma -> (repr =?= sigma) pos) rest)
          ) substitutions) in
          
        solve env pool (conj constraints);

        let substitution_mapping = List.combine alphas substitution in
        tracer (ImpredSubstitution substitution_mapping);

        
          
    and concat env header =
      StringMap.fold (fun name v env ->
                        EEnvFrame (env, name, v)
                     ) header env

    and unify_terms pos pool t1 t2 =
(* TRUNK: USELESS : let saved_t1, saved_t2 = twice (fun x -> instance pool x) t1 t2 in *)
        let tracer_func = (fun v1 v2 -> tracer (UnifyVars (v1, v2))) in
        unify ~tracer:tracer_func pos (register pool) t1 t2
   
          
    in (
      solve env pool c;
      !final_env
    )

  (** [init] produces a fresh initial state. It consists of an empty
      environment and a fresh, empty pool. *)
  let init () =
    EEmpty, MultiEquation.init ()

  (** The public version of [solve] starts out with an initial state
      and produces no result, except possibly an exception. *)
  let solve ?tracer c =
    let env, pool = init() in
    let tracer = default ignore tracer in
      tracer (Init c);
      (* TEMPORARY integrer un occur check ici aussi *)
      solve tracer env pool c 

end

 


