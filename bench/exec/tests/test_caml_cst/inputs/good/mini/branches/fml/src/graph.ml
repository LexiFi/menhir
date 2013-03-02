(* $Id$ *)

(** This module implements a mutable graph representation of multi-equation. *)
open Sig
open Misc
open InfiniteArray
open Printf

module Make (Constraint: Constraint) =
struct
  open Constraint
  open Constraint.MultiEquation.Algebra
  open Constraint.MultiEquation
  open Pervasives
    
  type node = int
  type group = int
  type 'a node_data = 
      {
        mutable succs: node list;
        mutable preds: node list;
        mutable data : 'a option;
        mutable group: group;
        mutable graph: 'a graph option;
      }
        
  and 'a graph =
      {
        mutable nodes: ('a node_data) InfiniteArray.t;
        mutable nodes_size: int;
        mutable groups: (int list) InfiniteArray.t;
        mutable groups_size: int;
      } 

  let fresh_node_data init gp g = 
    {
      succs = [];
      preds = [];
      data  = init;
      graph = g;
      group = gp;
    }

  let on_node_data f default d = 
    match d.data with
        None -> default
      | Some x -> f x

  let empty_graph () = 
    {
      nodes = InfiniteArray.make (fresh_node_data None 0 None);
      nodes_size = 0;
      groups = InfiniteArray.make [];
      groups_size = 0;
    }

  let root_group g = 
    0
      
  exception Found of int

  let search_node g f =
    try 
      InfiniteArray.iteri 
        (fun i x -> if on_node_data f false x then raise (Found i)) g.nodes;
      raise Not_found
    with Found i -> i

  let search_nodes g f =
    let ret = ref [] in
      InfiniteArray.iteri 
        (fun i x -> if on_node_data f false x then ret := i :: !ret) g.nodes;
      !ret
  
  let data_of_node g n = 
    unSome (InfiniteArray.get g.nodes n).data
      
  let add_group g gp = 
    let idx = g.groups_size + 1 in
      g.groups_size <- g.groups_size + 1;
      set g.groups idx [];
      set g.groups gp (-idx :: (get g.groups gp));
      idx
          
  let add_node g gp data =
    let idx = g.nodes_size in
      g.nodes_size <- g.nodes_size + 1;
      set g.nodes idx (fresh_node_data (Some data) gp (Some g));
      set g.groups gp (idx :: (get g.groups gp));
      idx

  let replace g n n' =
    let change k = if k = n then n' else k in
      (get g.nodes n).data <- None;
      InfiniteArray.iteri 
        (fun i x -> 
           if x.data <> None then (
             x.succs <- List.map change x.succs;
             x.preds <- List.map change x.preds
           )) g.nodes;
      InfiniteArray.iteri 
        (fun i l -> set g.groups i (List.map change l)) g.groups
           
  let connect g n1 n2 =
    let n1d = get g.nodes n1
    and n2d = get g.nodes n2 in
      n1d.succs <- n2 :: n1d.succs;
      n2d.preds <- n1 :: n2d.preds

  (* FIXME: we should ensure that the node is
     outside the group. *)
  let connect_to_group g n gp =
    let nd = get g.nodes n in
      nd.succs <- (-gp) :: nd.succs

  let is_predecessor g n n' =
    List.mem n' (InfiniteArray.get g.nodes n).preds

  let dot_font = 
    "Arial"

  let dump_node dp out g n = 
    let nd = get g.nodes n in
      if nd.data <> None then 
        fprintf out "n%d [%s];\n" n (dp n (unSome nd.data))
          
  let dump_node_edges out g n nd =
    let name n = "n"^string_of_int n in
    List.iter (fun n' -> 
                 if is_predecessor g n n' then 
                   fprintf out "%s -> %s[dir=none,style=dotted];\n" 
                     (name n) (name n')
                 else 
                   fprintf out "%s -> %s[dir=forward];\n" (name n) (name n'))
      nd.succs

  let rec dump_group dp out g gp nodes = 
    if nodes <> [] then
      let gnode = "groupnode"^string_of_int gp in
        fprintf out "%s" ("subgraph cluster_"^string_of_int gp^" {\n");
        fprintf out "style=solid;\n";
        fprintf out "color=black;\n";
        List.iter (fun idx -> 
                     if idx >= 0 then 
                       dump_node dp out g idx
                     else 
                       dump_group dp out g (-idx) (get g.groups (-idx)))
          nodes;
        fprintf out "}\n"

  let dump_as_dot dp out g = 
    fprintf out "strict digraph c {\n";
    fprintf out "node [fontname=\"%s\"];" dot_font;
    dump_group dp out g 0 (get g.groups 0);
    InfiniteArray.iteri (dump_node_edges out g) g.nodes;
    fprintf out "}"
      
  type extra_info = 
      ConstraintNode of tconstraint
    | TermNode of crterm
    | VarNode of variable
    | IdNode of string
    | NoInfo

  type solved_state = No | Being | Yes

  type constraint_data = 
      {
        extra_info: extra_info;
        mutable label: string;
        mutable solved: solved_state;
        node_group: group
      }

  let mkdata gp i l =
    {
      extra_info   = i;
      label        = l;
      solved       = No;
      node_group   = gp;
    }

  let node_of_term g gp (f: 'b graph -> group -> 'a -> node) (t : 'a term) 
      node = 
      match t with
        | App (t1, t2) -> 
          let node_t1 = f g gp t1
          and node_t2 = f g gp t2 in
            (data_of_node g node).label <- "@"; 
            connect g node node_t1;
            connect g node node_t2;
            node
              
        | Var t -> f g gp t
            
        | RowCons (l, t1, t2) -> 
            let ls = RowLabel.export l in
            let node_t1 = f g gp t1 
            and node_t2 = f g gp t2
            and node_l =  add_node g gp (mkdata gp (IdNode ls) ls) 
            in
              (data_of_node g node).label <- "Row"; 
              connect g node_l node_t1;
              connect g node node_l;
              connect g node node_t2;
              node

        | RowUniform t ->
            let node_d = add_node g gp (mkdata gp (IdNode "d/") "d/") 
            and node_t = f g gp t in
              (data_of_node g node).label <- "Row"; 
              connect g node_d node_t;
              connect g node node_d;
              node

  let rec nodify g gp c =
    let add l = add_node g gp (mkdata gp (ConstraintNode c) l) in
      match c with 
        | CTrue _  -> add "true"

        | CDump _  -> add "dump"

        | CFalse _ ->  add "false"

        | CContainment (_, op, t1, t2, _) ->
            let eq = match op with 
              | ContainEqual -> add "=?=" 
              | ContainLess -> add "<?=" 
              | ContainMore -> add ">?=" 
              | ContainLessImpred -> add "<?F"
              | ContainEqualImpred _ -> add "=?F" 
              in
            let t1_node = node_of_crterm g gp t1 
            and t2_node = node_of_crterm g gp t2 in
              connect g eq t1_node;
              connect g eq t2_node;
              eq
 
         | CInstance (_, id, t) -> 
            let inst = add (id ^" <?")
            and t_node = node_of_crterm g gp t 
            in
              connect g inst t_node;
              inst

        | CConjunction cs ->
            let conj = add "and" in
              List.iter (connect g conj) (List.map (nodify g gp) cs);
              conj

        | CDisjunction cs -> 
            let disj = add "disj" in
              List.iter (connect g disj) (List.map (nodify g gp) cs);
              disj

        | CLet (schemes, c) -> 
            let let_node = add "let" in
            let schemes_nodes, vars = 
              List.split (List.map (node_of_scheme gp g) schemes) 
            in
              List.iter (connect g let_node) schemes_nodes;
              (match c with
                   CTrue _ -> ()
                 | c -> 
                     let gp = add_group g gp in 
                     let c_node = nodify g gp c in
                     connect g let_node c_node);
              let_node
                
  and node_of_var g gp v = 
    try 
      search_node g (fun i -> match i.extra_info with
                          VarNode v' -> UnionFind.equivalent v v'
                        | _ -> false)
    with Not_found ->
      (* FIXME: use the name of the variable if possible. *)
      let node = add_node g gp (mkdata gp (VarNode v) "V") in
      match variable_structure v with
          None   -> node
        | Some t -> node_of_term g gp node_of_var t node
              
  and node_of_crterm g gp (t : crterm) = 
    match t with
        TVariable v -> node_of_var g gp v
      | TTerm t' -> 
          let node = add_node g gp (mkdata gp (TermNode t) "T") in
          node_of_term g gp node_of_crterm t' node
            | TGen _ -> failwith "unsupported for TGen"
  
  and node_of_scheme gp_parent g = function
      Scheme (pos, rqs, fqs, c, h) ->
        let gp = add_group g gp_parent in 
        let rpool_g = add_group g gp in
        let fpool_g = add_group g gp in
        let node = nodify g gp c in
        let h_nodes = 
          StringMap.iter 
            (fun id (t, _) -> 
               let node_t = node_of_crterm g gp t 
               and node_id = 
                 add_node g gp (mkdata gp (IdNode id) id)
               in
                 connect g node_id node_t)
            h
        in
          node,
          (List.map (node_of_var g rpool_g) rqs 
           @ List.map (node_of_var g fpool_g) fqs)

  let constraint_as_graph c = 
    let g = empty_graph () in
      ignore (nodify g (root_group g) c);
      g

end
