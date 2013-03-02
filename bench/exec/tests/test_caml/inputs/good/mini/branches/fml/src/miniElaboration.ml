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

(* ---------------------------- TRIVIAL ANNOT --------------------------- *)      

(** [new_default_annot pos] returns [(x, Tvariable x)], that is a trivial
    annotation. It is used by the constraint parser *)
let new_default_annot pos = 
  let x = variable Flexible ~pos:pos () in
  [x], TVariable x 

  
(* ---------------------------- ARROW ----------------------------------- *)       

(* Keep a pointer on the variable corresponding to type constructor "->" 
   that has been created at internalization *)

let tenv_arrow = ref None

let get_arrow () =
  match !tenv_arrow with
  | Some t -> t 
  | None -> assert false
  
  
(* ---------------------- TYPE DECLARATIONS ----------------------------- *)       

(** defines shapes, in a similar way to arterm *)

type shape = 
  | Sharp
  | ShapeVar of variable
  | ShapeApp of shape term
  | ShapeGen of variable * shape

  
(** [is_sharp shape] return whether the shape is Sharp *)  
let is_sharp = ((=) Sharp)



type shape_environment = shape Env.t


let constructor_shape_from_env pos env name =
  try Env.lookup env name with
  | Not_found -> raise (UnboundDataConstructor (pos, name))
  
  
let variable_shape_from_env pos env name =
  try Env.lookup env name with
  | Not_found -> raise (UnboundIdentifier (pos, name))

  
  
(** [Infer] is used to propagate upwards information on shapes, while
    [Check] is used to propagate downwards information on shapes  *)     
type elaborate_mode = 
  | Infer
  | Check of shape

  
(** [elaboration_step] are used for tracing *)
type elaboration_step =
  | ElaborateBinding of pattern * expression * shape_environment 
  | ElaborateExpression of elaborate_mode * expression * shape
  | ElaborateTypeDec of dname * shape

     
(* -------------------- PRINTING AND TRACING ---------------------------- *)       

let as_string = MiniAstPrettyPrinter.as_string

let sharp_variable = variable ~name:"#"Flexible () 

(** String representation of a shape : we first build a crterm associated 
    to the shape, replacing Sharps by the sharp_variable defined above,
    and then we return the string representation of that crterm. *)          
let string_of_shape shape = 
  let rec build_rec = function
    | Sharp -> TVariable sharp_variable
    | ShapeVar v -> TVariable v
    | ShapeApp t -> TTerm (map build_rec t)
    | ShapeGen (v,sub_shape) -> TGen (v, build_rec sub_shape)
    in
  let shape_type = build_rec shape in
  MiniTermPrinter.print_crterm shape_type


(** output tracing corresponding to elaboration steps *)
let default_tracer = function

  | ElaborateBinding (pat, expr, delta_env) ->
      let print_mapping (n,s) = n ^^ " : " ^^ (string_of_shape s) in
      Printf.printf "ELABORATE_BIND: let %s = %s ==> %s\n\n"
      (as_string MiniAstPrettyPrinter.print_pattern pat)
      (as_string MiniAstPrettyPrinter.print_expression expr)
      (String.concat " , " (List.map print_mapping delta_env))
      
  | ElaborateExpression (mode, ast_expr, shape) ->
      (*DR: hide inner elaboration steps *)
      Printf.printf "ELABORATE_%s: %s ==> %s\n\n"
      (match mode with Infer -> "INFER" | Check _ -> "CHECK")
      (as_string MiniAstPrettyPrinter.print_expression ast_expr)
      (string_of_shape shape)
      

  | ElaborateTypeDec (dname, shape) -> 
     Printf.printf "ELABORATE_TYPEDEC: def_constructor %s ==> %s\n\n"
     dname
     (string_of_shape shape)
  

(* --------------------- ANNOTATIONS OF SHAPES -------------------------- *)       
 
(** [instance_shape position shape] puts fresh variables to replace thoses
    quantified at ShapeGen nodes. *)
let instance_shape position shape =  
   let mark_copied = Mark.fresh() in
   let rec copy_rec = function
    | Sharp -> Sharp
    | ShapeVar v -> 
        ShapeVar (
          if Mark.same mark_copied (UnionFind.find v).mark  
            then unSome (UnionFind.find v).var
            else v
        )  
    | ShapeApp t -> ShapeApp (map copy_rec t)
    | ShapeGen (v, subterm) ->     
        let desc = UnionFind.find v in   
        let v' = copy_gen_var position desc in
        desc.mark <- mark_copied;
        desc.var <- Some v';
        let sub' = copy_rec subterm in
        desc.var <- None;
        ShapeGen (v', sub')
    in    
  copy_rec shape 
  
(** [annot_of_shape position shape] returns [fqs, crterm] that is
    a valid annotation corresponding to the shape : all Sharp are 
    replaced by fresh flexible variables which are listed in fqs. *)
let annot_of_shape position shape =
  
  let fqs = ref [] in
  let fresh () =
    let v = variable Flexible () in
    fqs := v::!fqs;
    v in
      
  let rec build_rec = function
    | Sharp -> TVariable (fresh ())
    | ShapeVar v -> TVariable v
    | ShapeApp t -> TTerm (map build_rec t)
    | ShapeGen (v,sub_shape) -> TGen (v, build_rec sub_shape)
    in
    
  !fqs, build_rec (instance_shape position shape)


(* ---------------------- CONSTRUCT / DECONSTRUCT ----------------------- *)       

(** [construct_data_shape cons_shape args_shapes] return the shape :
    cons_shape args_shapes1 args_shapes2 ... args_shapesN *)
let construct_data_shape cons_shape args_shapes =
  if List.for_all is_sharp args_shapes
    then Sharp
    else List.fold_left 
           (fun acu s -> ShapeApp (App (acu, s)))
           cons_shape args_shapes
 
   
(* BIN
let construct_arrow_shape s_arg s_ret =
  if s_arg = Sharp && s_ret = Sharp then
    Sharp 
  else (
    let shape_arrow = ShapeVar (get_arrow ()) in
    ShapeApp (App (ShapeApp (App (shape_arrow, s_arg)), s_ret))
  )
*)

(** [construct_arrow_shape] specializes [construct_data_shape] for the
    arrow type constructor *)
let construct_arrow_shape s_arg s_ret =
  construct_data_shape (ShapeVar (get_arrow ())) [ s_arg; s_ret ]

   
(** [get_type_construction shape] returns [shape_cons, shape_args] as the
    reciprocal of function [construct_data_shape]. Assume that there is
    no leading ShapeGen *)
let get_type_construction shape =         
   let rec extract_rec acu = function
      | Sharp -> failwith "get_type_construction reached Sharp !"
      | ShapeGen _ -> failwith "get_type_construction has reached ShapeGen !"
      | ShapeVar v as cons -> cons, acu
      | ShapeApp (App (term1, term2)) -> extract_rec (term2::acu) term1
      | ShapeApp _ -> failwith "unsupported rows" 
      in    
   extract_rec [] shape
   

(** [canonical_shape_of] replaces by Sharp any subshape that contains only
   type constructors and Sharp nodes. *)       
let rec canonical_shape_of s = 
  match s with
  | Sharp -> Sharp
  | ShapeVar v -> ShapeVar v
  | ShapeApp t -> 
      let cons_shapevar, args_shape = get_type_construction (ShapeApp t) in
      let args_shape' = List.map canonical_shape_of args_shape in
      let r = if List.for_all is_sharp args_shape' 
        then Sharp
        else construct_data_shape cons_shapevar args_shape' in
       r
  | ShapeGen (v, sub_shape) -> ShapeGen (v, sub_shape)
  
          
(* ARTHUR: to change so that to enforce the canonical 
          form invariant at the end of that function*)
(** [replace_in_shape v_from shape_dest shape] is used to replace 
    all occurrences of a variable that was quantified by ShapeGen 
    with a given shape, usually Sharp *)
let replace_in_shape v_from shape_dest shape = 
  let rec replace_rec = function
    | Sharp -> Sharp
    | ShapeVar v when v = v_from -> shape_dest
    | ShapeVar v -> ShapeVar v
    | ShapeApp t -> ShapeApp (map replace_rec t)
    | ShapeGen (v, sub_shape) -> ShapeGen (v, replace_rec sub_shape)
  in
  replace_rec shape
    
  
(** [get_data_construction_shape pos dname nb_args shape] returns the
    shape of constructor and the shape of arguments *)
(* ARTHUR: use get_type_construction instead of extract_rec ? *)
let get_data_construction_shape pos dname nb_args shape =         
  
   let error () = 
      let fqs_s, typ_s = annot_of_shape pos shape in
      raise (InvalidAnnotationForDataConstructor (pos, dname, typ_s))
      in
      
   let rec extract_rec acu args_left = function
      | s when args_left = 0 -> s, acu
      | Sharp | ShapeGen _ | ShapeVar _ -> error()        
      | ShapeApp (App (term1, term2)) 
        -> extract_rec (term2::acu) (pred args_left) term1
      | ShapeApp _ -> failwith "unsupported rows" 
      in    
  
   let rec remove_head_gen = function
      | ShapeGen (v, s) -> remove_head_gen (replace_in_shape v Sharp s)
      | s -> s
      in
             
    match canonical_shape_of (remove_head_gen shape) with
      | Sharp -> Sharp, Array.to_list (Array.create nb_args Sharp)
      | _  -> extract_rec [] nb_args shape
  

(* ARTHUR: could be treated as a special case of the above function,
           but it may not be much simpler. *) 
let rec deconstruct_arrow_shape where pos = function
   
   | ShapeApp (App (ShapeApp (App (ShapeVar v, s1)), s2)) when v = get_arrow () 
       -> (s1, s2)
       
   | Sharp 
       -> (Sharp, Sharp)
       
   |  ShapeGen (v, s) 
       -> let s_r = replace_in_shape v Sharp s in
          let s' = canonical_shape_of s_r in 
          deconstruct_arrow_shape where pos s'
      
   | s 
       -> let fqs_s, typ_s = annot_of_shape pos s in
          Printf.printf "%s\n" where;
          raise (InvalidAnnotationForLambda (pos, typ_s))
   

(*------------------------ SHAPES OF ANNOTATIONS ------------------------ *)       

(** [shape_of_type crterm] extracts the shape of the [crterm], 
    in the canonical form *)
(* ARTHUR: could use canonical_shape_of after an naive translation *)
let shape_of_type crterm = 
  
  let mark_bound = Mark.fresh() in

  let rec extract_rec = function
  
    | TVariable v -> 
        if has_mark mark_bound v 
          then ShapeVar v 
          else Sharp
    
    | TTerm _ as term ->  
        let cons, args = get_data_construction_crterm term in
        let shape_args = List.map extract_rec args in
        if List.for_all is_sharp shape_args 
          then Sharp
          else construct_data_shape (ShapeVar cons) shape_args

    | TGen (v, subterm) -> 
        assert (not (has_mark mark_bound v));
        set_mark mark_bound v;
        ShapeGen (v, extract_rec subterm)

    in 
  extract_rec crterm

    
(** [shape_of_annot (fqs, ityp)] returns the shape of [ityp],
    ignoring the quantifiers *)
let shape_of_annot (fqs, ityp) =
  shape_of_type ityp

  
(* ---------------------------------------------------------------------- *)       

let split_data_arguments_infos infos =
    let delta_env = List.fold_left 
       (fun acu (_,_,delta_i) ->  Env.concat delta_i acu) 
       Env.empty infos in
    let pats' = List.map (fun (pat_i',_,_) -> pat_i') infos in
    let shapes' = List.map (fun (_,shape_i,_) -> shape_i) infos in
    delta_env, pats', shapes'
 
    
(*------------------------ ELABORATE EXPRESSIONS ------------------------ *)       
let level = ref 0
let set n = level := n
    
let rec elaborate_expression tracer env expr mode = 
  
  let elaborate_expr = elaborate_expression tracer in

  let shape, expr' = match mode, expr with
 
  | Check s, MiniEst.EVar (pos, name)
    -> 
       let shape = variable_shape_from_env pos env name in
       if !level > 0 then 
           let l1, t1 = annot_of_shape pos (shape)  in
           let l2, t2 = annot_of_shape pos (s) in
           let l = l1 @ l2 in
           s, EImpredicativity (pos, EVar (pos, name), l1 @ l2, t1, t2)
       else
           s, EVar (pos, name)

    
  | Infer  , MiniEst.EVar (pos, name)
    -> let shape = variable_shape_from_env pos env name in
       shape, EVar (pos, name)
    
       
  | Check s, MiniEst.ELambda (pos, pat, expr) 
    -> let s_pat, s_expr = deconstruct_arrow_shape "elambda" pos s in
       let pat', s_pat', delta_env = elaborate_pattern pat (Check s_pat) in
       let env' = Env.concat delta_env env in
       let expr', s_expr' = elaborate_expr env' expr (Check s_expr) in
       s, ELambda (pos, pat', expr')
        
  | Infer  , MiniEst.ELambda (pos, pat, expr) 
    -> let pat', s_pat', delta_env = elaborate_pattern pat Infer in
       let env' = Env.concat delta_env env in
       let expr', s_expr' = elaborate_expr env' expr Infer in
       let shape = construct_arrow_shape s_pat' s_expr' in
       shape, ELambda (pos, pat', expr')    
                       
           
  | mode   , MiniEst.EAppAnnoted (pos, expr1, expr2, None) 
    -> let expr1', s1 = elaborate_expr env expr1 Infer in
       let expr2', s2 = elaborate_expr env expr2 Infer in
       let s1a, s1r = deconstruct_arrow_shape "app" pos s1 in
       let shape = match mode with
         | Infer -> s1r
         | Check s -> s 
         in
       let annot = if s1a <> Sharp then s1a else s2 in
       shape, EAppAnnoted (pos, expr1', expr2', annot_of_shape pos annot)
                     
  | Check s, MiniEst.EAppAnnoted (pos, expr1, expr2, Some annot) 
    -> let func_shape = construct_arrow_shape (shape_of_annot annot) s in
       let expr1', s1 = elaborate_expr env expr1 (Check func_shape) in
       let expr2', s2 = elaborate_expr env expr2 (Check (shape_of_annot annot)) in
       s, EAppAnnoted (pos, expr1', expr2', annot)
    
  | Infer  , MiniEst.EAppAnnoted (pos, expr1, expr2, Some annot) 
    -> let expr1', s1 = elaborate_expr env expr1 Infer in
       let expr2', s2 = elaborate_expr env expr2 (Check (shape_of_annot annot)) in
       let s1a, s1r = deconstruct_arrow_shape "appannoted" pos s1 in
       s1r, EAppAnnoted (pos, expr1', expr2', annot)
    
    
  | mode   , MiniEst.EBinding (pos, bind, expr) 
    -> let bind', delta_env = elaborate_binding tracer env bind in
       let env' = Env.concat delta_env env in
       let expr', s' = elaborate_expr env' expr mode in
       s', EBinding (pos, bind', expr') 

           
  | mode   , MiniEst.EPrimApp (pos, prim, exprs) 
    -> assert (exprs = []); 
       let shape = match mode with
         | Infer | Check Sharp -> Sharp
         | Check s -> raise (InvalidAnnotationForPrimApp (pos, snd (annot_of_shape pos s)))
         in
       shape, EPrimApp (pos, prim, [])
 
          
  | mode   , MiniEst.EForall (pos, vars, expr) 
    -> let expr', s' = elaborate_expr env expr mode in
       s', EForall (pos, vars, expr') 
         
  | mode   , MiniEst.EExists (pos, vars, expr) 
    -> let expr', s' = elaborate_expr env expr mode in
       s', EExists (pos, vars, expr') 
  
         
  | mode   , MiniEst.ETypeConstraint (pos, expr, ietyp) 
    -> (* can check compatibility *)
       let expr', s' = elaborate_expr env expr (Check (shape_of_annot ietyp)) in
       let shape = match mode with 
         | Infer -> s'
         | Check s -> s
        in
       shape, ETypeConstraint (pos, expr', ietyp)
     
       
  | Check s, MiniEst.EImpredicativity (pos, expr, fqs, left_opt, right_opt)
    -> (* we could check compatibility of s and right *)
       let expr', left' = match left_opt with
          | None -> elaborate_expr env expr Infer 
          | Some left -> elaborate_expr env expr (Check (shape_of_type left)) 
          in
       let right' = match right_opt with
          | None -> s
          | Some right -> shape_of_type right
          in
       let fqs_left, type_left = match left_opt with
          | None -> annot_of_shape pos left' 
          | Some left -> [], left
          in 
       let fqs_right, type_right = match right_opt with
          | None -> annot_of_shape pos right' 
          | Some right -> [], right
          in   
       s, EImpredicativity (pos, expr', fqs @ fqs_left @ fqs_right, type_left, type_right)
               
   | Infer  , MiniEst.EImpredicativity (pos, expr, fqs, left_opt, Some right)
     -> let right' = shape_of_type right in
        let expr', left' = match left_opt with
          | None -> elaborate_expr env expr Infer 
          | Some left -> elaborate_expr env expr (Check (shape_of_type left)) 
          in
        let fqs_left, type_left = match left_opt with
          | None -> annot_of_shape pos left' 
          | Some left -> [], left
          in 
        right', EImpredicativity (pos, expr', fqs @ fqs_left, type_left, right)
        
    | Infer  , MiniEst.EImpredicativity (pos, expr, fqs, Some left, None)
      -> let expr', left' = elaborate_expr env expr (Check (shape_of_type left)) in
         left', ETypeConstraint (pos, expr', annot_of_shape pos left')
 
    | Infer  , MiniEst.EImpredicativity (pos, expr, fqs, None, None)
      -> let expr', left' = elaborate_expr env expr Infer in
         left', expr'

             
  | Check s, MiniEst.EDCon (pos, name)
    -> let shape = constructor_shape_from_env pos env name in
       (*s, EDCon (pos, name)*)
   if !level > 0 then
       let fqs_right, right = annot_of_shape pos (instance_shape pos shape) in
       let fqs_left, left = annot_of_shape pos (instance_shape pos s) in
       s, EImpredicativity (pos, EDCon (pos, name), fqs_right @ fqs_left, right, left) 
   else
       s, EDCon (pos, name)

    
  | Infer  , MiniEst.EDCon (pos, name)
    -> let shape = constructor_shape_from_env pos env name in
       shape, EDCon (pos, name)
         
       
  | mode   , MiniEst.EMatchAnnoted (pos, expr, annot_opt, clauses)
    -> (* we could try to infer from pat_i or expr_i, but which result to choose ? *)
       let expr', shape_expr = match annot_opt with
          | None -> elaborate_expr env expr Infer
          | Some annot -> elaborate_expr env expr (Check (shape_of_annot annot))
          in 
       let shape_return = match mode with
          | Infer -> Sharp
          | Check s -> s
          in
       let clauses' = List.map (fun (pos, pat_i, expr_i) ->
            let pat_i', s_pat_i', delta_env_i = elaborate_pattern pat_i (Check shape_expr) in
            let env_i = Env.concat delta_env_i env in
            let expr_i', sharp_shape = elaborate_expr env_i expr_i (Check shape_return) in
            (pos, pat_i', expr_i')
          ) clauses in
       shape_return, EMatchAnnoted (pos, expr', annot_of_shape pos shape_expr, clauses')
        
  | _, MiniEst.ERecordEmpty pos -> assert false    
  | _, MiniEst.ERecordAccess (pos, expr, lname) -> assert false
  | _, MiniEst.ERecordExtend (pos, recbinds, expr) -> assert false 
  | _, MiniEst.ERecordUpdate (pos, expr1, lname, expr2) -> assert false
  | _, MiniEst.EError (pos, pat) -> assert false    
  | _, MiniEst.EAssertFalse (pos) -> assert false

  in
  tracer (ElaborateExpression (mode, expr', shape));   
  (expr', shape)

  
(* ---------------------- ELABORATE BINDINGS ---------------------------- *)       

(** returns [binding', delta_env] *)  
and elaborate_binding tracer env = function        
  
(* ARTHUR : we could share code between annoted and non annoted versions *)

  | MiniEst.BindValue (pos, vdefs) 
    -> let vdefs', all_delta_env = List.split (List.map (function 
    
          | pos, vars, pat, expr, None ->         
              let pat', s_pat', delta_env = elaborate_pattern pat Infer in
              let expr', s_expr = elaborate_expression tracer env expr Infer in
              
              (* if not enough info in pattern, try to look in expr *)
              let shape, delta_env =
                if s_pat' <> Sharp then s_pat', delta_env else (
                  let pat'', s_pat'', delta_env = elaborate_pattern pat (Check s_expr) in
                  s_pat'', delta_env
                )  in
                
              tracer (ElaborateBinding (pat', expr', delta_env));   
              (pos, vars, pat', expr', annot_of_shape pos shape), delta_env

          | pos, vars, pat, expr, Some annot ->
              let shape_def = shape_of_annot annot in
              let pat', s_pat', delta_env = elaborate_pattern pat (Check shape_def) in
              let expr', s_expr = elaborate_expression tracer env expr (Check shape_def) in
              tracer (ElaborateBinding (pat', expr', delta_env));   
              (pos, vars, pat', expr', annot_of_shape pos shape_def), delta_env
          ) vdefs) in
       
       BindValue (pos, vdefs'), List.concat all_delta_env
       
    
  | MiniEst.BindRecValue (pos, vdefs)  (* ARTHUR: check function *)
    -> let vdefs', all_delta_env = List.split (List.map (function 
    
          | pos, vars, pat, expr, None -> 
              let pat', func_name = match pat with
                  | MiniEst.PVar (pos, name) -> PVar (pos, name), name
                  | _ -> failwith "letrec pattern should be a name"
                  in
              let env' = Env.add env func_name Sharp in
              let expr', s_expr = elaborate_expression tracer env' expr Infer in           
              let delta_env' = Env.add Env.empty func_name s_expr in
              tracer (ElaborateBinding (pat', expr', delta_env'));   
              (pos, vars, pat', expr', annot_of_shape pos s_expr), delta_env'
            
          | pos, vars, pat, expr, Some annot ->
          
              let pat', func_name = match pat with
                  | MiniEst.PVar (pos, name) -> PVar (pos, name), name
                  | _ -> failwith "letrec pattern should be a name"
                  in
              let shape_def = shape_of_annot annot in
              let delta_env = Env.add Env.empty func_name shape_def in
              let env' = Env.concat delta_env env in
              let expr', s_expr = elaborate_expression tracer env' expr (Check shape_def) in           
              tracer (ElaborateBinding (pat', expr', delta_env));   
              (pos, vars, pat', expr', annot_of_shape pos shape_def), delta_env

         ) vdefs) in
       
       BindRecValue (pos, vdefs'), List.concat all_delta_env
       

  | MiniEst.TypeDec (pos, tds)
    -> let all_delta_env = List.map (fun (_, _, _, _, _, typedef) ->
           match typedef with
            | MiniEst.DAlgebraic ds 
              -> List.map (fun (pos, dname, typ) -> 
                    let shape = shape_of_type typ in
                    tracer (ElaborateTypeDec (dname, shape));
                    (dname, shape)
                  ) ds
            | MiniEst.DAbbrev typ 
              -> Env.empty
         )
          tds in
       TypeDec (pos, tds), List.concat all_delta_env (* todo: use Env.concat_list ? *) 
          

(* ----------------------- ELABORATE PATTERN ---------------------------- *)       

(** return [pat', s_pat', delta_env] *)
and elaborate_pattern pat mode = 

  match mode, pat with
  
  | Check s, MiniEst.PZero pos 
    -> PZero pos, s, Env.empty 
  
  | Infer  , MiniEst.PZero pos 
    -> PZero pos, Sharp, Env.empty 
     
    
  | Check s, MiniEst.PVar (pos, name) 
    -> PVar (pos, name), s, Env.add Env.empty name s
  
  | Infer  , MiniEst.PVar (pos, name) 
    -> PVar (pos, name), Sharp, Env.add Env.empty name Sharp
    
    
  | Check s, MiniEst.PWildcard pos 
    -> PWildcard pos, s, Env.empty 
    
  | Infer  , MiniEst.PWildcard pos 
    -> PWildcard pos, Sharp, Env.empty 
    
    
  | mode   , MiniEst.PAlias (pos, name, pat) 
    -> let pat', s_pat', delta_env = elaborate_pattern pat mode in
       PAlias (pos, name, pat'), s_pat', Env.add delta_env name s_pat'
    
    
  | mode   , MiniEst.PTypeConstraint (pos, pat, eityp)
    -> let sub_mode = Check (shape_of_annot eityp) in
       let pat', s_pat', delta_env = elaborate_pattern pat sub_mode in
       PTypeConstraint (pos, pat', eityp), s_pat', delta_env

    
  | mode   , MiniEst.PPrimitive (pos, prim) 
    -> let shape = match mode with
         | Infer | Check Sharp -> Sharp
         | Check s -> raise (InvalidAnnotationForPrimApp (pos, snd (annot_of_shape pos s)))
         in
       PPrimitive (pos, prim), shape, Env.empty
    
       
  | mode  , MiniEst.PData (pos, tnames, dname, pats)     
    -> assert (tnames = []); (* or todo *)
       let infos  = List.map (fun pat_i -> elaborate_pattern pat_i Infer) pats in
       let delta_env, pats', shapes' = split_data_arguments_infos infos in
       let shape = 
          match mode with
          | Infer -> construct_data_shape Sharp shapes' 
          | Check s -> s 
          in
       PData (pos, tnames, dname, pats'), shape, delta_env
    
  | mode  , (MiniEst.PAnd _ | MiniEst.POr _) 
    -> assert false (* todo *)
  

(* ------------------------ ELABORATE PROGRAM --------------------------- *)       

(** Add to an empty shape environment the shapes of
    all built-in data constructors *)
let init_env () =
  Env.add Env.empty "_Tuple" Sharp
            
    
(** [elaborate_program init_env pst_program] *)    
let elaborate_program tracer (est_program : MiniEst.program) = 
  let bindings', final_env = List.fold_left (fun (bindings, env) binding -> 
      let (binding' : MiniAst.binding), delta_env = elaborate_binding tracer env binding in  
      let env' = Env.concat delta_env env in
      (binding'::bindings, env')
    ) ([], init_env ()) est_program
    in
  (List.rev bindings' : MiniAst.program)
  
  
(* --------------------------------------------------------------------- *)       
let elaborate_task = "elaborate-program"

let optional_tracer () =
  if Processing.is_task_traced elaborate_task
    then (fun step -> (default_tracer step))
    else ignore

let register_tasks internalize_task  = 
  Processing.register 
    elaborate_task 
    ([], ignore) 
    [ [ internalize_task ] ]
    (fun t ->
      let program, arrow = (List.hd t) in
      tenv_arrow := Some arrow;
      elaborate_program (optional_tracer ()) program) 
    (Misc.const true);

