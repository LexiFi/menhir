(* $Id$ *)

(** This module implements a simple kind inferencer.  *)

open Sig
open Positions
open MiniPst
open MiniTypingExceptions
open Misc

module Ast = MiniPst
   
module IdSet = Set.Make (struct
                           type t = string
                           let compare = compare
                         end)

module RowDomain = BasicSetEquations.Make (struct
                                             
    include IdSet

    let print s =
      try
        let label = choose s in
        fold (fun label accu ->
          label ^ "+" ^ accu
        ) (remove label s) label
      with Not_found ->
        ""

  end)

type variable = 
    descriptor UnionFind.point
      
and descriptor = 
    {
      mutable structure : term option;
      mutable name : string;
      mutable constant : bool;
    }

and term =
  | App of variable * variable
  | Row of RowDomain.term

type t = variable

type env = (name -> t) * (name -> t -> unit)

let count = ref 0

let new_name () =
  incr count;
  "V" ^ string_of_int !count

let variable ?name () =
  let constant = (name <> None) 
  and name = match name with None -> new_name () | Some n -> n in
    UnionFind.fresh { structure = None; name = name; constant = constant }

let structure v = 
  (UnionFind.find v).structure
    
let iter_term f = function
    App (t1, t2) -> 
      f t1;
      f t2
  | _ -> ()

let iter f v = iter_term f (unSome (structure v))
                 
let lookup id tenv = (fst tenv) id
        
let bind id t tenv = (snd tenv) id t; tenv
               
let term_handler t = 
  UnionFind.fresh {
    name = "";
    structure = Some t;
    constant = false
  }
    
let times = "__x"
let arrow = "__=>"
let star = "__*"
             
let count = ref 0
                
let symbol tenv i = 
  (fst tenv) i
    
let binop tenv op x y = 
  let a = symbol tenv op in
  let w = term_handler (App (a, x)) in
    term_handler (App (w, y))    
      
let mkarrow tenv = 
  binop tenv arrow
    
let mkstar tenv = 
  symbol tenv star

let mktimes tenv = 
  binop tenv times
    
let rec mkconstructor tenv arity =
  if arity = 0 then mkstar tenv else
    mkarrow tenv (mkstar tenv) (mkconstructor tenv (arity - 1))  

let row d = 
  term_handler (Row d)

let initial_kind_env = 
  [ 
    (star , variable ~name:"@" ()); 
    (arrow, variable ~name:"=>" ());
    (times, variable ~name:"x" ())
  ] 
    
let is_star env k = 
  UnionFind.equivalent k (mkstar env)
    
let fresh_kind () = 
  variable ()
        
let rec kind_arity env v = 
  let arrow_sym = symbol env arrow in
    match (UnionFind.find v).structure with
      | None -> 
          if is_star env v then 0
          else
            assert false
      | Some t ->
          (match t with 
             | App (s, k) when s = arrow_sym -> 1 + kind_arity env k
             | App (k, _) -> kind_arity env k
             | _ -> 0)

let rec print_term = function
  | App (v1, v2) -> String.concat "" [ "(" ; print v1 ; "," ; print v2 ; ")" ]
  | Row v -> "Row("^ RowDomain.print v ^ ")"
      
and print v = 
  match (UnionFind.find v).structure with
    | None -> name v
    | Some t -> print_term t

and name v = (UnionFind.find v).name

let is_constant v = (UnionFind.find v).constant             

let assign_point k1 k2 = 
  let name, has_name = 
    if is_constant k1 then name k1, true
    else if is_constant k2 then name k2, true
    else "", false
  in 
    UnionFind.union k1 k2;
    if has_name then (
      (UnionFind.find k2).name <- name; 
      (UnionFind.find k2).constant <- true
    ) 

let assign pos k1 k2 = 
  iter (fun k -> if UnionFind.equivalent k1 k then raise (KindError pos)) k2; 
  assign_point k1 k2

(* FIXME: do it better ! It is not correct. *)
let occur_check v1 v2 = 
  let rec variables acu v = 
    match (structure v) with 
      | None -> if not (List.memq v acu) then v::acu else acu
      | Some (App (v1, v2)) -> variables (variables acu v1) v2
      | _ -> acu (* Incorrect ! *)
  in
  let vars1 = variables [] v1 
  and vars2 = variables [] v2 
  in
    List.memq v1 vars2 ||
    List.memq v2 vars1 

let rec unify pos k1 k2 = 
(*  Printf.eprintf "%s =?= %s\n" (print k1) (print k2); *)
  if not (UnionFind.equivalent k1 k2) then (
    if occur_check k1 k2 then
      raise (KindError pos);

    match structure k1, structure k2 with
        
      | None, None -> 
          if (is_constant k1 && is_constant k2 && name k1 <> name k2) then
            (failwith "là";  raise (KindError pos))
          assign_point k1 k2

      | (None, _ | _, None) when is_constant k1 || is_constant k2 -> 
          raise (KindError pos)
            
      | None, _ -> 
          assign pos k1 k2

      | _, None -> 
          assign pos k2 k1

      | Some (App (t1, t2)), Some (App (t1', t2')) ->
          unify pos t1 t1';
          unify pos t2 t2'

      | Some (Row d1), Some (Row d2) ->
          RowDomain.unify d1 d2

      | _ -> assert false)

(* FIXME: Should be done on internal term representation to factorize
   some checking.
*)
let rec infer env t = 
  let intern t = infer env t
  in 
    match t with
        TypVar (p, id) -> 
          lookup id env

      | TypApp (p, tc, ts) ->
          let k = variable () in
          let kd = 
            List.fold_right (fun t acu -> mkarrow env (infer env t) acu) 
              ts k 
              
          in
            unify p (infer env tc) kd; 
            k
            
      | TypGen (p, tnames, typ) -> 
          let env_read, env_update = env in
          let extends_reading read_in_env gen_name =
            let k = UnionFind.fresh 
              { structure = None; name = gen_name; constant = false } in
            (fun id -> if gen_name = id then k else read_in_env id) 
            in 
          let new_read = List.fold_left extends_reading (fst env) tnames in
          let new_env = new_read, env_update in
          infer new_env typ
          
      | TypRowCons (p, attributes, t) ->
          List.iter (fun (_,ta) -> check p env ta (mkstar env)) attributes;
          let defined_labels = 
            List.fold_left 
              (fun acu (l,_) -> 
                 if IdSet.mem l acu then raise (MultipleLabels (p, l))
                 else IdSet.add l acu) 
              IdSet.empty 
              attributes in
          let domain = RowDomain.svariable () in
            check p env t (row (RowDomain.sum defined_labels domain));
            row domain

      | TypRowUniform (p, typ) ->
          row (RowDomain.svariable ())
  
and check pos env t k = 
  unify pos (infer env t) k

  (* Deprecated : variance has been added
let rec intern_kind env = function
  | KStar -> mkstar env
  | KTimes (k1, k2) -> mktimes env (intern_kind env k1) (intern_kind env k2)
  | KArrow (k1, k2) -> mkarrow env (intern_kind env k1) (intern_kind env k2)
  | KEmptyRow -> term_handler (Row (RowDomain.empty))
*)

let variance_of_kvariance = function
  | KInVariant -> InVariant
  | KCoVariant -> CoVariant
  | KContraVariant -> ContraVariant
  | _ -> raise (Invalid_argument "variance_of_kvariance expects a KVariance")

  
let intern_kind env k =

  let rec extract_kind = function
    | KInVariant | KCoVariant | KContraVariant -> mkstar env
    (* Deprecated : useless | KTimes (k1, k2) -> mktimes env (intern_kind env k1) (intern_kind env k2) *)
    | KArrow (k1, k2) -> mkarrow env (extract_kind k1) (extract_kind k2)
    | KEmptyRow -> term_handler (Row (RowDomain.empty))
    in
    
  let rec extract_variance = function
    | KInVariant 
        -> []
    | KArrow ((KInVariant| KCoVariant | KContraVariant) as kvar, rest) 
        -> (variance_of_kvariance kvar)::(extract_variance rest)
    | KCoVariant | KContraVariant | KArrow _ 
        -> raise (ErrorOnDataConstructorDefinition InvalidDataConstructorBadKind)
    | KEmptyRow 
        -> failwith "row are not supported in kind" (* TODO_ROW *)   
    in 
        
  extract_kind k, extract_variance k
