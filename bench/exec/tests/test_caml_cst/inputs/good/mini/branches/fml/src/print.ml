(* $Id$ *)

(** This module provides a simple pretty-printer for the terms
    maintained by a unifier.

    We follow the convention that types and type schemes are represented
    using the same data structure. In the case of type schemes, universally
    quantified type variables are distinguished by the fact that their rank
    is [none]. The pretty-printer binds these variables locally, while other
    (non-quantified) type variables are considered part of a global namespace. 
*)

open Sig
open Misc

module Make (MultiEquation : MultiEquation)
: TermPrinter with type variable = MultiEquation.variable
          and type term = MultiEquation.crterm

= struct

  open MultiEquation
  open MultiEquation.Algebra

  (** The things that we print are [variable]s, that is, entry points
      for types or type schemes. *)
  type variable = MultiEquation.variable

  type term = MultiEquation.crterm

  (** [gi] is the last consumed number. *)
  let gi =
     ref (-1)

  (** [ghistory] is a mapping from variables to variable names. *)
  let ghistory =
    ref []

  (** [reset()] clears the global namespace, which is implemented
     by [gi] and [ghistory]. *)
  let reset () =
    gi := -1;
    ghistory := []

  type arg = 
      Arg of (MultiEquation.variable * string * arg list 
              * bool * associativity * bool)
    
  let paren b e = if b then "("^^e^^")" else e


  (** [print is_type_scheme v] returns a printable representation of
      the type or type scheme whose entry point is [v]. The parameter
      [is_type_scheme] tells whether [v] should be interpreted as a
      type or as a type scheme. Recursive types are dealt with by
      printing inline equations. Consecutive calls to [print] share
      the same variable naming conventions, unless [reset] is called
      in between. *)
  let printer ?user_name_from_int is_type_scheme =
    let name_from_int = default Misc.name_from_int user_name_from_int in

    (** Create marks to deal with cycles. *)
    let visiting = Mark.fresh()
    and hit = Mark.fresh() in

    (** Create a local namespace for this type scheme. *)
    let i = ref (-1)
    and history = ref [] in

    (** [name v] looks up or assigns a name to the variable [v]. When
        dealing with a type scheme, then the local or global namespace
        is used, depending on whether [v] is universally quantified or
        not. When dealing with a type, only the global namespace is
        used. *)

    (* FIXME: necessite du prefixe ? *)
    let rec var_name hits visited v =
      let desc = UnionFind.find v in
      let autoname () = 
        let prefix, c, h =
          if is_type_scheme && desc.rank = Rank.none 
          then
            "", i, history
          else
            "_", gi, ghistory
        in
          try
            Misc.assocp (UnionFind.equivalent v) !h 
          with Not_found ->
            incr c;
            let result = (* prefix ^ *) name_from_int !c in
              desc.name <- Some result;
              h := (v, result) :: !h;
              result
      in
        (match desc.name with
          | Some name -> 
              if desc.kind <> Constant then
                try
                    Misc.assocp (UnionFind.equivalent v) !ghistory              
                with Not_found ->
                  history := (v, name) :: !history;
                  name
              else name
          | _ -> autoname ())
(*        ^ ("["^string_of_int (Obj.magic desc.rank : int)^"]")*)
(*        ^ (match desc.kind with Constant -> "#" | Rigid -> "!" | _ -> "?")*)
    in

    (* Term traversal. *)
    let rec print_variable ?use_user_def hits visited v = 

      let is_hit v =
        Mark.same (UnionFind.find v).mark hit
      and is_visited v = 
        Mark.same (UnionFind.find v).mark visiting
      in
      let var_or_sym v = 
        (match variable_name v with
          | Some name ->
              (match as_symbol name with
                 | Some sym ->
                     (v, name, [], infix sym, associativity sym, false)
                 | None -> 
                     (v, var_name hits visited v, [], false, NonAssoc, false))
          | None -> 
              (v, var_name hits visited v, [], false, NonAssoc, false))
      in
      let desc = UnionFind.find v in
        
      (* If this variable was visited already, we mark it as ``hit
         again'', so as to record the fact that we need to print an
         equation at that node when going back up. *)

        if is_hit v || is_visited v then
          begin
            desc.mark <- hit;
            var_or_sym v 
          end

      (* If this variable was never visited, we mark it as ``being
         visited'' before processing it, so as to detect cycles.
         If, when we are done with this variable, its mark has
         changed to ``hit again'', then it must be part of a cycle,
         and we annotate it with an inline equation. *)

      else begin

        desc.mark <- visiting;
        match desc.structure with
          | None -> var_or_sym v
          | Some t -> 
              let (v', name, args, infix, assoc, p) as r = 
                print_term hits visited t in
                (* if is_hit v then 
                  (v, var_name hits visited v^" =", 
                   [ Arg (v', name, args, infix, assoc, p) ],
                   false, assoc, true) 
                else (desc.mark <- Mark.none; r) *)
                if is_hit v then ( 
                  (v, var_name hits visited v ^ " =", 
                   [ Arg (v', name, args, infix, assoc, p) ],
                   false, assoc, true) 
                ) else (desc.mark <- Mark.none; r)
      end

    and print_term ?use_user_def hits visited t = 
      let at_left  = function [] -> true | [ x ] -> false | _ -> assert false
      and at_right = function [] -> true | [ x ] -> false | _ -> assert false
      in
      let rec print = function
              
        | App (t1, t2) -> 
            
            let (op1, name1, args1, infix1, assoc1, force_paren1) = 
              print_variable hits visited t1
            and (op2, name2, args2, infix2, assoc2, force_paren2) =
              print_variable hits visited t2
            in
            let priority name = 
              match as_symbol name with
                | Some sym -> Algebra.priority sym
                | None     -> -1
            in
            let paren_t2 = force_paren2 || 
              if are_equivalent op1 op2 then
                (assoc2 = AssocLeft && at_right args1) 
                || (assoc2 = AssocRight && at_left args1) 
              else
                (priority name2 > priority name1)
            in
              (op1, name1,
               (args1 @ [ Arg (op2, name2, args2, infix2, assoc2, paren_t2)]), 
               infix1, assoc1, force_paren1)
                
        | Var v -> print_variable hits visited v

        | RowCons (label, typ, r) -> 
            let typv = print_variable hits visited typ in
            (variable Flexible (), ";", 
             [
               Arg (variable Flexible (), RowLabel.export label^":", 
                    [ Arg typv ], false, NonAssoc, false);
               Arg (print_variable hits visited r)],
               true, NonAssoc, false)

        | RowUniform typ ->
            (variable Flexible (), "\\", 
             [ Arg (print_variable hits visited typ) ],
             false, NonAssoc, false)

      in print t
    in
    let prefix hits visited () = 
      if is_type_scheme then
        match !history with
          | [] ->
              ""
          | history ->
              List.fold_left 
                (fun quantifiers (v, _) -> 
                   quantifiers ^ " " ^ (var_name hits visited v)) 
                "forall" (List.rev history) ^ ". " 
      else ""

    in let as_string f r = 
        let rec loop (Arg (_, name, args, infix, assoc, is_paren)) =
          if args = [] then name 
          else 
            paren is_paren
              (if infix then 
                 print_separated_list (" "^^name^^" ") loop args
               else 
                 (match assoc with EnclosedBy (t, _) -> t | _ -> name) ^^
                   (if args <> [] then " " else "")^^
                   (print_separated_list " " loop args)^^
                   (match assoc with EnclosedBy (_,t) -> " "^t | _ -> "")
              )
        in
        let hits, visited = ref [], ref [] in
        let (op, name, args, infix, assoc, _) = f hits visited r in
          prefix hits visited () 
          ^ loop (Arg (op, name, args, infix, assoc, false))
    in
      (as_string print_variable, as_string print_term)
 
        
      
  (*--------------------------------------------------------------------*)

   
  (* debug flags *)    
  let show_rigid_vars = false
  let show_constant_vars = false
  let show_positionned_vars = false
  let show_ranked_none_vars = false
  let show_rank = false
 
  let debug_suffix v = 
      (if show_rigid_vars && (UnionFind.find v).kind = Rigid then "°" else "")
    ^ (if show_constant_vars && (UnionFind.find v).kind = Constant then "~" else "")
    ^ (if show_positionned_vars && (UnionFind.find v).pos = None then "@" else "")
    ^ (if show_ranked_none_vars && (UnionFind.find v).rank = Rank.none then "!" else "")
    ^ (if show_rank then "_" ^ (string_of_int (UnionFind.find v).rank) else "")
      
 let print_variable ?user_name_from_int b v =
    let s = (fst (printer ?user_name_from_int:user_name_from_int b)) v in
    s ^ (debug_suffix v)
    
  
  let print_rec_variable v =
     print_variable false v
  
  exception Cycling of Positions.position list
  
  let extract_pos_of_cyclic_variable var_root = 

    let cyclic_mark = Mark.fresh() in
    
    let add_pos_if_defined list = function
      | None -> list
      | Some pos -> pos::list
      in
        
    let rec find_pos v =     
      let desc = UnionFind.find v in            
      if Mark.same desc.mark cyclic_mark 
        then add_pos_if_defined [] desc.pos
        else begin          
          match desc.structure with
            | None -> add_pos_if_defined [] desc.pos  
            | Some structure ->   
                desc.mark <- cyclic_mark;            
                let found = ref [] in
                iter (fun var -> found := (find_pos var) @ !found) structure;
                desc.mark <- Mark.none; 
                !found
          end
      in  
        
    find_pos var_root

          
  let label_forall = if false then "$" else "forall "
  
  (* NameForVariable is not correct but helps to debug *)
  type naming_convention = NameForClasses | NameForVariable
  let naming_convention = NameForClasses
                   
  type genarg = 
    | ArgFunc of (MultiEquation.variable * string * genarg list 
            * bool * associativity * bool)
    | ArgGen of (string list * genarg * bool)

  
  let rec extract_crterm v = 
    non_cyclic_variable v;
    let desc = UnionFind.find v in
    match desc.structure with
      | None -> TVariable v
      | Some structure -> TTerm (map extract_crterm structure)    
  
  let rec full_expand = function
      | TVariable v -> extract_crterm v
      | TTerm t -> TTerm (map full_expand t)
      | TGen (v,subterm) -> TGen (v, full_expand subterm)

            
  let print_crterm t =
   
    let rec var_name v =
      let desc = UnionFind.find v in  
      let name = match desc.name with
        | Some name -> 
            if desc.kind <> Constant then (
              try
                match naming_convention with
                  | NameForClasses -> Misc.assocp 
                                        (UnionFind.equivalent v) 
                                        !ghistory
                  | NameForVariable -> List.assq v !ghistory
              with Not_found ->
                ghistory := (v, name) :: !ghistory;
                name
            ) else (
              name
            )
        | _ -> 
            try
              Misc.assocp (UnionFind.equivalent v) !ghistory 
            with Not_found ->
              incr gi;
              let result = name_from_int !gi in
              desc.name <- Some result;
              ghistory := (v, result) :: !ghistory;
              result
        in
      name ^ (debug_suffix v) in
      
                
    let var_or_sym v = 
      match variable_name v with
        | Some name ->
            (match as_symbol name with
               | Some sym ->
                   (v, name ^ (debug_suffix v), [], 
                    infix sym, associativity sym, false)
               | None -> 
                   (v, var_name v, [], false, NonAssoc, false)
            )
        | None ->  (v, var_name v, [], false, NonAssoc, false)
      in
           
                 
    let rec print_term = function
        
      | TVariable v -> ArgFunc (var_or_sym v)
      
      | TTerm t -> print_tterm t
      
      | TGen (u, subterm) -> 
          let rec decompose acu = function
            | TGen (v, sub) -> decompose ((var_name v)::acu) sub 
            | _ as rest -> acu, rest
            in
          let names, arg = decompose [var_name u] subterm in
          ArgGen (List.rev names, print_term arg, true)
      
      
    and print_tterm = function
              
    | App (t1, t2) -> 
        begin
        match print_term t1, print_term t2 with
       
        | ArgGen _ , _ -> 
        
          failwith "print_tterm: App cannot have a TGen as first argument"
       
        | ArgFunc (op1, name1, args1, infix1, assoc1, force_paren1) , (ArgGen _ as arg_gen) -> 

          ArgFunc (op1, name1, args1 @ [ arg_gen ], infix1, assoc1, force_paren1)              
        
        | ArgFunc (op1, name1, args1, infix1, assoc1, force_paren1) , 
          ArgFunc (op2, name2, args2, infix2, assoc2, force_paren2) -> 
      
          let priority name = 
            match as_symbol name with
              | Some sym -> Algebra.priority sym
              | None     -> if name = "Gen" then 1000 else -1
          in
          let at_left  = function [] -> true | [ x ] -> false | _ -> assert false in
          let at_right = function [] -> true | [ x ] -> false | _ -> assert false in
          let paren_t2 = force_paren2 || 
            if are_equivalent op1 op2 then
              (assoc2 = AssocLeft && at_right args1) 
              || (assoc2 = AssocRight && at_left args1) 
            else
              (priority name2 > priority name1)
          in
          let all_args = args1 @ [ ArgFunc (op2, name2, args2, infix2, assoc2, paren_t2)] in
          ArgFunc (op1, name1, all_args, infix1, assoc1, force_paren1)
                     
       end     
    | Var v -> print_term v

    | RowCons (label, typ, r) -> failwith "Row printing is not supported"
        (* let typv = print_variable hits visited typ in
        (variable Flexible (), ";", 
         [
           Arg (variable Flexible (), RowLabel.export label^":", 
                [ Arg typv ], false, NonAssoc, false);
           Arg (print_variable hits visited r)],
           true, NonAssoc, false)
        *)
    | RowUniform typ -> failwith "Row printing is not supported"
        (*
        (variable Flexible (), "\\", 
         [ Arg (print_variable hits visited typ) ],
         false, NonAssoc, false)
         *)
         

        in

      let rec loop = function
        | ArgFunc (_, name, args, infix, assoc, is_paren) ->
            if args = [] then name 
            else 
              paren is_paren
                (if infix then 
                   print_separated_list (" "^^name^^" ") loop args
                 else 
                   (match assoc with EnclosedBy (t, _) -> t | _ -> name) ^^
                     (if args <> [] then " " else "")^^
                     (print_separated_list " " loop args)^^
                     (match assoc with EnclosedBy (_,t) -> " "^t | _ -> "")
                )
        | ArgGen (names, arg, is_paren) ->
              let string_names = print_separated_list " " (fun s->s) names in
              paren is_paren (Printf.sprintf "%s%s. %s" label_forall string_names (loop arg))
      in
      
      try
        let full_term = full_expand t in
        loop (match print_term full_term with 
             | ArgFunc (op, name, args, infix, assoc, _) -> ArgFunc (op, name, args, infix, assoc, false)
             | ArgGen (names, arg, _) -> ArgGen (names, arg, false) 
           )
      with CyclicVariable _ -> 
        match t with 
        | TVariable v -> print_rec_variable v
        | _ -> "cyclic_crterm" (* failwith "trying to print a cyclic crterm" *)
         
        (*failwith "trying to print a cyclic term";
         raise (MiniTypingExceptions.CyclicStructureInferred ("undefined_name_(at_printing)", v)) *)
    

    

   
  let print_term ?user_name_from_int b t =
    print_crterm t
             

  let print_variable ?user_name_from_int b v =
    print_crterm (TVariable v)

      
 (* support ?
     let t = match t with
      | TVariable (v : variable) -> TVariable (explode v)
      | _ -> t 
      in 
  
 *)
  
    
end

