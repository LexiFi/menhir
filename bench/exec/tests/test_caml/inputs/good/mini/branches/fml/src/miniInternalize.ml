
open Positions
open Misc
open Sig
open MiniKindInferencer
open MiniConstraint
open MiniAlgebra
open MiniMultiEquation
open MiniTypingEnvironment
open MiniTypingExceptions
open MiniTypes
open MiniEst

(* ------------------------------------------------------------ *) 


let internalize_typedecs tenv tds =
   let tenv', tds' = List.fold_left 
      (fun (tenv, tds_done) (pos, kind, name, def) -> 
    
        let ikind, variance = 
          try MiniKindInferencer.intern_kind (as_kind_env tenv) kind
          with ErrorOnDataConstructorDefinition case 
               -> raise (InvalidDataConstructorDefinition (pos, name, case))
          in
        
        let tenv', def', cons_var =
          match def with
          
          | MiniPst.DAlgebraic ds ->
        
             let ids_def = ref None in
             let cons_var = variable_for_typecon ~name:name ~pos:pos variance in
             let tenv' = add_type_constructor tenv name 
                             (ikind, cons_var, ids_def) in
             
             let ds' = List.map (fun (pos, dname, typ) -> 
                  let ityp = intern pos tenv' typ in 
                  (pos, dname, ityp)
                ) ds in

             let ids = List.fold_left (fun acu (pos, dname, ityp) -> (dname, ityp)::acu) 
                                      [] ds' in
             ids_def := Some ids;
             
             tenv', DAlgebraic ds', cons_var
                 
          | MiniPst.DAbbrev typ -> 
              
              let ityp = intern pos tenv typ in
              let tenv' = add_type_abbrev tenv name ityp in
              let cons_var = variable_for_typecon ~name:name ~pos:pos [] in
              let tenv'' = add_type_variable tenv' name (fresh_kind(), cons_var) in
              tenv'', DAbbrev ityp, cons_var
              
          in
          (tenv', (pos, ikind, variance, cons_var, name, def')::tds_done) 
      )
      (tenv, [])
      tds
      
  in tenv',  List.rev tds'
          
          
(* ------------------------------------------------------------ *) 

let rec internalize_expression tenv = function

  (** Core ML. *)
  
  | MiniPst.EVar (pos, name)
    -> EVar (pos, name)
    
  | MiniPst.ELambda (pos, pat, expr) 
    -> ELambda (pos, internalize_pattern tenv pat, 
                     internalize_expression tenv expr)
    
  | MiniPst.EAppAnnoted (pos, expr1, expr2, annot_opt) 
    -> EAppAnnoted (pos, internalize_expression tenv expr1,   
                         internalize_expression tenv expr2, 
                         internalize_annotation tenv pos annot_opt)
                 
  | MiniPst.EBinding (pos, bind, expr) 
    -> begin
       match bind with    
        | MiniPst.BindValue (pos, vdefs) 
          -> let bind' = BindValue (pos, List.map (internalize_vdef tenv) vdefs) in
             let expr' = internalize_expression tenv expr in 
             EBinding (pos, bind', expr')
             
        | MiniPst.BindRecValue (pos, vdefs) 
          -> let bind' = BindRecValue (pos, List.map (internalize_vdef tenv) vdefs) in
             let expr' = internalize_expression tenv expr in 
             EBinding (pos, bind', expr') 
  
        | MiniPst.TypeDec (pos, tds)
          -> let tenv', tds' = internalize_typedecs tenv tds in
             let expr' = internalize_expression tenv' expr in 
             EBinding (pos, TypeDec (pos, tds'), expr')  
       end
    
    
  | MiniPst.EPrimApp (pos, prim, exprs) 
    -> EPrimApp (pos, prim, List.map (internalize_expression tenv) exprs)
 
    
  (** Type annotations. *)
      
  | MiniPst.EForall (pos, tnames, expr) 
    -> let rqs, rtenv = fresh_rigid_vars pos tenv tnames in 
       let tenv' = add_type_variables rtenv tenv in
       EForall (pos, rqs, internalize_expression tenv' expr)
    
  | MiniPst.EExists (pos, tnames, expr) 
    -> let fqs, rtenv = fresh_flexible_vars pos tenv tnames in 
       let tenv' = add_type_variables rtenv tenv in
       EExists (pos, fqs, internalize_expression tenv' expr)
         
  | MiniPst.ETypeConstraint (pos, expr, etyp) 
    -> let ietyp = intern_existential_type pos tenv etyp in
       ETypeConstraint (pos, internalize_expression tenv expr, ietyp)
    
  | MiniPst.EImpredicativity (pos, expr, vs, opt_typ1, opt_typ2) 
    -> let fqs, tenv' = intern_quantifiers pos tenv vs in
       let ityp1 = match opt_typ1 with 
          | Some typ1 -> Some (intern pos tenv' typ1)
          | None -> None in
       let ityp2 = match opt_typ2 with 
          | Some typ2 -> Some (intern pos tenv' typ2)
          | None -> None in
       EImpredicativity (pos, internalize_expression tenv expr, fqs, ityp1, ityp2)

          
  (** Algebraic datatypes. *) 
  
  | MiniPst.EDCon (pos, dname) 
    -> EDCon (pos, dname)

  | MiniPst.EMatchAnnoted (pos, expr, annot_opt, clauses)
    -> EMatchAnnoted (pos, internalize_expression tenv expr, 
                           internalize_annotation tenv pos annot_opt, 
                           List.map (internalize_clauses tenv) clauses)

    
  (** Records. *)
  
  | MiniPst.ERecordEmpty pos 
    -> ERecordEmpty pos
    
  | MiniPst.ERecordAccess (pos, expr, lname) 
    -> ERecordAccess (pos, internalize_expression tenv expr, lname)
    
  | MiniPst.ERecordExtend (pos, recbinds, expr)
    -> ERecordExtend (pos, List.map (internalize_record_binding tenv) recbinds, 
                           internalize_expression tenv expr)
                        
  | MiniPst.ERecordUpdate (pos, expr1, lname, expr2)
    -> ERecordUpdate (pos, internalize_expression tenv expr1, 
                           lname, 
                           internalize_expression tenv expr2)

                           
  (** Misc. *)
  
  | MiniPst.EError (pos, pat)  
    -> EError (pos, internalize_pattern tenv pat)
    
  | MiniPst.EAssertFalse (pos)  
    -> EAssertFalse (pos) 
 
    
     
and internalize_vdef tenv = function
  | MiniPst.VDefAnnoted (pos, tnames, pat, expr, annot_opt) 
    -> let rqs, rtenv = fresh_rigid_vars pos tenv tnames in 
       let tenv' = add_type_variables rtenv tenv in
       (pos, rqs, 
             internalize_pattern tenv' pat, 
             internalize_expression tenv' expr, 
             internalize_annotation tenv' pos annot_opt)

              
and internalize_annotation tenv pos annot_opt =
  match annot_opt with 
    | Some etyp -> Some (intern_existential_type pos tenv etyp)
    | None -> None 
        
        
and internalize_clauses tenv (pos, pat, expr) =
  (pos, internalize_pattern tenv pat, 
        internalize_expression tenv expr)

  
and internalize_record_binding tenv (name, expr) =
  (name, internalize_expression tenv expr)    

  
and internalize_pattern tenv = function
  | MiniPst.PZero pos 
    -> PZero pos 
  | MiniPst.PVar (pos, name) 
    -> PVar (pos, name) 
  | MiniPst.PWildcard pos 
    -> PWildcard pos
  | MiniPst.PAlias (pos, name, pat) 
    -> PAlias (pos, name, internalize_pattern tenv pat) 
  | MiniPst.PTypeConstraint (pos, pat, etyp)
    -> PTypeConstraint (pos, internalize_pattern tenv pat, 
                             intern_existential_type pos tenv etyp)
  | MiniPst.PPrimitive (pos, prim) 
    -> PPrimitive (pos, prim)
  | MiniPst.PData (pos, tnames, dname, pats) 
    -> PData (pos, tnames, dname, List.map (internalize_pattern tenv) pats)
  | MiniPst.PAnd (pos, pats) 
    -> PAnd (pos, List.map (internalize_pattern tenv) pats)
  | MiniPst.POr (pos, pats) 
    -> POr (pos, List.map (internalize_pattern tenv) pats)    
  
    
(** [internalize_program init_env pst_program] *)    
let internalize_program init_env (pst_program : MiniPst.binding list) = 
  let (tenv', bindings') =
    List.fold_left (fun (tenv, acu_bindings) -> function
    
      | MiniPst.TypeDec (pos, tds)
        -> let tenv', tds' = internalize_typedecs tenv tds in
           tenv', (TypeDec (pos, tds'))::acu_bindings
          
      | MiniPst.BindValue (pos, vdefs) 
        -> let bind = BindValue (pos, List.map (internalize_vdef tenv) vdefs) in
           tenv, bind::acu_bindings
           
      | MiniPst.BindRecValue (pos, vdefs) 
        -> let bind = BindRecValue (pos, List.map (internalize_vdef tenv) vdefs) in
           tenv, bind::acu_bindings 
    ) (init_env, []) pst_program 
    in
  List.rev bindings', lookup_type_variable tenv' "->"

  
  
(* ------------------------------------------------------------------------ *)       
  
    
let init_kind_env () =
    let variance = [] in 
    List.fold_left (fun env (n, k) ->
                      add_type_constructor env n
                        (k, 
                         variable_for_typecon ~name:n variance, 
                         ref None)
                   )
        empty_environment
        MiniKindInferencer.initial_kind_env 
    
   let add_builtins pst_program = 
    let pos = undefined_position in
    let tds =
       List.map (fun (name, (_,_,_,kind, ds_unpos)) -> 
          let ds = List.map (fun (dname, typ) -> (pos, dname, typ)) ds_unpos in 
          (pos, kind, name, MiniPst.DAlgebraic ds)
        )
      (Array.to_list builtin_env)
      in
    (MiniPst.TypeDec (pos, tds)) :: pst_program

  
              
(* ------------------------------------------------------------------------ *)       
    
      
let internalize_task = "internalize-program"

let register_tasks parse_program_task  = 
  Processing.register 
    internalize_task 
    ([], ignore) 
    [ [ parse_program_task ] ]
    (fun t -> 
      let pst_program = List.hd t in
      internalize_program (init_kind_env ()) (add_builtins pst_program)
     ) 
    (Misc.const true);
