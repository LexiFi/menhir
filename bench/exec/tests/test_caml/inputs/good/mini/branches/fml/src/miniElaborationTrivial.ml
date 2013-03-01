
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
open MiniAst


(* ------------------------------------------------------------------------ *)       
  

let new_default_annot pos = 
  let x = variable Flexible ~pos:pos () in
  [x], TVariable x 
        

(* ------------------------------------------------------------------------ *)       
 
let rec elaborate_expression = function

  (** Core ML. *)
  
  | MiniEst.EVar (pos, name)
    -> EVar (pos, name)
    
  | MiniEst.ELambda (pos, pat, expr) 
    ->
        ELambda (pos, elaborate_pattern pat, 
                     elaborate_expression expr)
    
  | MiniEst.EAppAnnoted (pos, expr1, expr2, annot_opt) 
    -> EAppAnnoted (pos, elaborate_expression expr1,   
                         elaborate_expression expr2, 
                         elaborate_annotation pos annot_opt)
                 
  | MiniEst.EBinding (pos, bind, expr) 
    -> EBinding (pos, elaborate_binding bind, elaborate_expression expr) 
    
  | MiniEst.EPrimApp (pos, prim, exprs) 
    -> EPrimApp (pos, prim, List.map elaborate_expression exprs)
 
    
  (** Type annotations. *)
      
  | MiniEst.EForall (pos, vars, expr) 
    -> EForall (pos, vars, elaborate_expression expr) 
    
  | MiniEst.EExists (pos, tnames, expr) 
    -> EExists (pos, tnames, elaborate_expression expr) 
         
  | MiniEst.ETypeConstraint (pos, expr, ityp) 
    -> ETypeConstraint (pos, elaborate_expression expr, ityp)
    
  | MiniEst.EImpredicativity (pos, expr, vs, opt_ityp1, opt_ityp2) 
    -> 
      begin match opt_ityp1, opt_ityp2 with
      | Some ityp1, Some ityp2 ->
        EImpredicativity (pos, elaborate_expression expr, vs, ityp1, ityp2)
      | _ -> failwith "Trivial elaboration does not support partial \
                       impredicative instanciation" (* TODO_ERROR *)
      end
      
  (** Algebraic datatypes. *) 
  | MiniEst.EDCon (pos, dname) 
    -> EDCon (pos, dname)

  | MiniEst.EMatchAnnoted (pos, expr, annot_opt, clauses)
    -> EMatchAnnoted (pos, elaborate_expression expr, 
                           elaborate_annotation pos annot_opt, 
                           List.map elaborate_clauses clauses)

    
  (** Records. *)

  | MiniEst.ERecordEmpty pos 
    -> ERecordEmpty pos
    
  | MiniEst.ERecordAccess (pos, expr, lname) 
    -> ERecordAccess (pos, elaborate_expression expr, lname)
    
  | MiniEst.ERecordExtend (pos, recbinds, expr)
    -> ERecordExtend (pos, List.map elaborate_record_binding recbinds, 
                           elaborate_expression expr)
                        
  | MiniEst.ERecordUpdate (pos, expr1, lname, expr2)
    -> ERecordUpdate (pos, elaborate_expression expr1, 
                           lname, 
                           elaborate_expression expr2)

                           
  (** Misc. *)

  | MiniEst.EError (pos, pat)  
    -> EError (pos, elaborate_pattern pat)
    
  | MiniEst.EAssertFalse (pos)  
    -> EAssertFalse (pos) 
 

and elaborate_binding = function        
  | MiniEst.BindValue (pos, vdefs) 
    -> BindValue (pos, List.map elaborate_vdef vdefs)
       
  | MiniEst.BindRecValue (pos, vdefs) 
    -> BindRecValue (pos, List.map elaborate_vdef vdefs)

  | MiniEst.TypeDec (pos, tds)
    -> TypeDec (pos, tds)
       
     
and elaborate_vdef (pos, tnames, pat, expr, annot_opt) =
   (pos, tnames, 
         elaborate_pattern pat, 
         elaborate_expression expr, 
         elaborate_annotation pos annot_opt)

              
and elaborate_annotation pos annot_opt =
  match annot_opt with
  | Some annot -> annot
  | None -> new_default_annot pos 
  
        
and elaborate_clauses (pos, pat, expr) =
  (pos, elaborate_pattern pat, 
        elaborate_expression expr)

  
and elaborate_record_binding (name, expr) =
  (name, elaborate_expression expr)    

  
and elaborate_pattern = function
  | MiniEst.PZero pos 
    -> PZero pos 
  | MiniEst.PVar (pos, name) 
    -> PVar (pos, name) 
  | MiniEst.PWildcard pos 
    -> PWildcard pos
  | MiniEst.PAlias (pos, name, pat) 
    -> PAlias (pos, name, elaborate_pattern pat) 
  | MiniEst.PTypeConstraint (pos, pat, etyp)
    -> PTypeConstraint (pos, elaborate_pattern pat, etyp)
  | MiniEst.PPrimitive (pos, prim) 
    -> PPrimitive (pos, prim)
  | MiniEst.PData (pos, tnames, dname, pats) 
    -> PData (pos, tnames, dname, List.map elaborate_pattern pats)
  | MiniEst.PAnd (pos, pats) 
    -> PAnd (pos, List.map elaborate_pattern pats)
  | MiniEst.POr (pos, pats) 
    -> POr (pos, List.map elaborate_pattern pats)    
  
    
(** [elaborate_program init_env pst_program] *)    
let elaborate_program pst_program = 
  List.map elaborate_binding pst_program
      
(* ------------------------------------------------------------------------ *)       
    
let elaborate_task = "elaborate-program"

let register_tasks internalize_task  = 
  Processing.register 
    elaborate_task 
    ([], ignore) 
    [ [ internalize_task ] ]
    (fun t -> elaborate_program (List.hd t)) 
    (Misc.const true);
