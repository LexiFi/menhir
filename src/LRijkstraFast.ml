(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

module Run
    (X : sig
       val validate : bool
       val verbose : bool
     end)
    () :
  LRijkstra.REACHABILITY_ALGORITHM
=
struct
  open Grammar

  (* The set of all terminals, useful for encoding default reduction *)
  let all_terminals = TerminalSet.universe

  let terminal_partition =
    let module TerminalPartition = Refine.Make(TerminalSet) in
    fun sets ->
      TerminalPartition.partition (List.sort_uniq TerminalSet.compare sets)

  (* Explicit representation of transitions *)
  open Fix.Numbering.Typed
  open Infix

  module Lr1C = struct
    include (val const Lr1.n)
    let of_g lr1 = Index.of_int n (Lr1.number lr1)
    let to_g lr1 = Lr1.of_number (Index.to_int lr1)
  end

  module Transition : sig
    type nt and t and any
    val nt : nt cardinal
    val t : t cardinal
    val any : any cardinal

    val of_nt : nt index -> any index
    val of_t : t index -> any index
    val split : any index -> (nt index, t index) either

    val find_nt : Lr1C.n index -> Nonterminal.t -> nt index
    val find : Lr1C.n index -> Symbol.t -> any index

    val source : any index -> Lr1C.n index
    val target : any index -> Lr1C.n index
    val symbol : any index -> Symbol.t
    val symbol_nt : nt index -> Nonterminal.t
    val symbol_t : t index -> Terminal.t

    val successors : Lr1C.n index -> any index list
    val predecessors : Lr1C.n index -> any index list
  end =
  struct
    module NT = Gensym()
    module T = Gensym()

    let () =
      Lr1.iter (fun lr1 ->
          SymbolMap.iter (fun sym _ -> match sym with
              | Symbol.T t ->
                if Terminal.real t then
                  ignore (T.fresh ())
              | Symbol.N _ -> ignore (NT.fresh ())
            ) (Lr1.transitions lr1)
        );

    type nt = NT.n
    let nt = NT.n

    type t = T.n
    let t = T.n

    module Any = (val sum nt t)
    type any = Any.n
    let any = Any.n

    let of_nt = Any.inj_l
    let of_t  = Any.inj_r
    let split = Any.prj

    let nt_sources = Vector.make' nt (fun () -> Index.of_int Lr1C.n 0)
    let nt_symbols = Vector.make' nt (fun () -> Nonterminal.i2n 0)
    let nt_targets = Vector.make' nt (fun () -> Index.of_int Lr1C.n 0)

    let t_sources = Vector.make' t (fun () -> Index.of_int Lr1C.n 0)
    let t_symbols = Vector.make' t (fun () -> Terminal.i2t 0)
    let t_targets = Vector.make' t (fun () -> Index.of_int Lr1C.n 0)

    let nt_table = Hashtbl.create 7
    let nt_pack lr1 nt = Index.to_int lr1 * Nonterminal.n + Nonterminal.n2i nt

    let t_table = Hashtbl.create 7
    let t_pack lr1 t = Index.to_int lr1 * Terminal.n + Terminal.t2i t

    let predecessors = Vector.make Lr1C.n []

    let successors =
      let next_nt = Index.enumerate nt in
      let next_t = Index.enumerate t in
      Vector.init Lr1C.n begin fun source ->
        SymbolMap.fold begin fun sym target acc ->
          match sym with
          | Symbol.T t when not (Terminal.real t) -> acc
          | _ ->
            let target = Lr1C.of_g target in
            let index = match sym with
              | Symbol.T t ->
                let index = next_t () in
                t_sources.%(index) <- source;
                t_symbols.%(index) <- t;
                t_targets.%(index) <- target;
                Hashtbl.add t_table (t_pack source t) index;
                Any.inj_r index
              | Symbol.N nt ->
                let index = next_nt () in
                nt_sources.%(index) <- source;
                nt_symbols.%(index) <- nt;
                nt_targets.%(index) <- target;
                Hashtbl.add nt_table (nt_pack source nt) index;
                Any.inj_l index
            in
            predecessors.%::(target) <- index;
            index :: acc
        end (Lr1.transitions (Lr1C.to_g source)) []
      end

    let successors lr1 = successors.%(lr1)
    let predecessors lr1 = predecessors.%(lr1)

    let find_nt source nt = Hashtbl.find nt_table (nt_pack source nt)

    let find source = function
      | Symbol.T t -> of_t (Hashtbl.find t_table (t_pack source t))
      | Symbol.N nt -> of_nt (Hashtbl.find nt_table (nt_pack source nt))

    let source i =
      match split i with
      | L i -> nt_sources.%(i)
      | R i -> t_sources.%(i)

    let symbol i =
      match split i with
      | L i -> Symbol.N nt_symbols.%(i)
      | R i -> Symbol.T t_symbols.%(i)

    let symbol_nt i = nt_symbols.%(i)
    let symbol_t i = t_symbols.%(i)

    let target i =
      match split i with
      | L i -> nt_targets.%(i)
      | R i -> t_targets.%(i)

    let () = Time.tick "Ijkstra: populate transition table"

    let () =
      if false then
        Printf.printf "%d terminals\n%d lr1 states\n%d transitions\n%!"
          Terminal.n Lr1.n (cardinal any)
  end

  (* Ungoto graph: undo a goto transition, going from an edge state to the
     states the reduction can have originated from *)
  module Reductions : sig
    type t = {
      production: Production.index;
      lookahead: TerminalSet.t;
      steps: Transition.any index list;
      state: Lr1C.n index;
    }

    val of_transition: Transition.nt index -> t list
  end = struct

    type t = {
      production: Production.index;
      lookahead: TerminalSet.t;
      steps: Transition.any index list;
      state: Lr1C.n index;
    }

    let lookahead_terminals ts = ts
    let lookahead_default = all_terminals

    let support_default = true

    let has_shift lr1 = SymbolMap.exists (fun sym _ -> match sym with
        | Symbol.T _ -> true
        | Symbol.N _ -> false
      ) (Lr1.transitions (Lr1C.to_g lr1))

    let table = Vector.make Transition.nt []

    let () =
      Index.iter Lr1C.n begin fun lr1 ->
        let reductions =
          let raw = Lr1.reductions (Lr1C.to_g lr1) in
          let raw =
            TerminalMap.fold (fun t ps acc ->
                MList.cons_if (Terminal.non_error t) (t, List.hd ps) acc
              ) raw []
          in
          (* Regroup lookahead tokens by production *)
          MList.group_by raw
            ~compare:(fun (_, p1) (_, p2) ->
                Int.compare (Production.p2i p1) (Production.p2i p2)
              )
            ~group:(fun (t, p) tps ->
                let set = List.fold_left
                    (fun set (t, _) -> TerminalSet.add t set)
                    (TerminalSet.singleton t) tps
                in
                (p, set)
              )
        in
        let is_default = support_default && match reductions with
          | [_] -> not (has_shift lr1)
          | _ -> false
        in
        reductions |> List.iter (fun (production, ts) ->
            if not (Production.is_start production) then
              let lookahead =
                if is_default
                then lookahead_default
                else lookahead_terminals ts
              in
              let lhs = Production.nt production in
              let rhs = Production.rhs production in
              let states =
                Array.fold_right (fun _ states ->
                    let expand acc (state, steps) =
                      List.fold_left (fun acc tr ->
                          (Transition.source tr, tr :: steps) :: acc
                        ) acc (Transition.predecessors state)
                    in
                    List.fold_left expand [] states
                  ) rhs [lr1, []]
              in
              List.iter (fun (source, steps) ->
                  table.%::(Transition.find_nt source lhs) <-
                    { production; lookahead; steps; state=lr1 }
                ) states
          )
      end

    let of_transition tr = table.%(tr)

    let () = Time.tick "Ijkstra: populate reduction table"

    let () =
      if false then
      Printf.eprintf "Reduction table has %d entries\n"
        (Array.fold_left (fun sum e -> sum + List.length e) 0
           (table : _ vector :> _ array))
  end

  (* Compute classes refinement *)

  module Classes = struct
    module Node = (val sum Lr1C.n Transition.nt)
    module Gr = struct
      type node = Node.n index
      let n = (cardinal Node.n)

      let index = Index.to_int

      let ungoto_edge f tr =
        List.iter (fun {Reductions. lookahead; state; _} -> f lookahead state)
          (Reductions.of_transition tr)

      let visit_edge f tr =
        List.iter (fun {Reductions. state; _} -> f (Node.inj_l state))
          (Reductions.of_transition tr)

      let visit_lr1 f lr1 =
        match Lr1.incoming_symbol (Lr1C.to_g lr1) with
        | None | Some (Symbol.T _) -> ()
        | Some (Symbol.N _) ->
          List.iter (fun tr ->
              match Transition.split tr with
              | L nt -> f (Node.inj_r nt)
              | R _ -> assert false
            )
            (Transition.predecessors lr1)

      let successors f i =
        match Node.prj i with
        | L lr1 -> visit_lr1 f lr1
        | R e -> visit_edge f e

      let iter f = Index.iter Node.n f
    end

    module Scc = Tarjan.Run(Gr)

    let () = Time.tick "Ijkstra: class graph SCC"

    let classes = Vector.make Node.n []

    let () =
      Scc.rev_topological_iter begin fun _ nodes ->
        let classes_of acc node =
          let acc = ref acc in
          begin match Node.prj node with
            | L lr1 ->
              Gr.visit_lr1 (fun n -> acc := classes.%(n) @ !acc) lr1
            | R edge ->
              Gr.ungoto_edge begin fun la lr1 ->
                let base = classes.%(Node.inj_l lr1) in
                let base =
                  if la != all_terminals
                  then List.map (TerminalSet.inter la) base
                  else base
                in
                acc := (la :: base) @ !acc
              end edge
          end;
          !acc
        in
        let coarse_classes =
          nodes
          |> List.fold_left classes_of []
          |> terminal_partition
        in
        match nodes with
        | [node] -> classes.%(node) <- coarse_classes
        | nodes ->
          List.iter begin fun node ->
            match Node.prj node with
            | L _ -> ()
            | R e ->
              let coarse = ref TerminalSet.empty in
              Gr.ungoto_edge
                (fun la _ -> coarse := TerminalSet.union la !coarse) e;
              classes.%(node) <-
                coarse_classes
                |> List.map (TerminalSet.inter !coarse)
                |> terminal_partition
          end nodes;
          List.iter begin fun node ->
            match Node.prj node with
            | R _ -> ()
            | L lr1 ->
              let acc = ref [] in
              Gr.visit_lr1 (fun n -> acc := classes.%(n) @ !acc) lr1;
              classes.%(node) <- terminal_partition !acc
          end nodes;
      end

    let () = Index.iter Lr1C.n (fun lr1 ->
        match Lr1.incoming_symbol (Lr1C.to_g lr1) with
        | Some (Symbol.N _) -> ()
        | None | Some (Symbol.T _) ->
          classes.%(Node.inj_l lr1) <- [all_terminals]
      )

    let classes = Vector.map Array.of_list classes

    let for_edge nte =
      classes.%(Node.inj_r nte)

    let for_lr1 st =
      classes.%(Node.inj_l st)

    let t_singletons =
      let table =
        Array.init Terminal.n
          (fun t -> [|TerminalSet.singleton (Terminal.i2t t)|])
      in
      fun t -> table.(Terminal.t2i t)

    let all_terminals =
      [|all_terminals|]

    let before_transition tr =
      match Transition.symbol tr with
      | Symbol.T t -> t_singletons t
      | Symbol.N _ -> for_lr1 (Transition.source tr)

    let after_transition tr =
      match Transition.split tr with
      | L edge -> for_edge edge
      | R _ -> all_terminals

    let () = Time.tick "Ijkstra: token classes for each transition"
  end

  module ConsedTree () : sig
    include CARDINAL
    module Inner : CARDINAL

    val leaf : Transition.any index -> n index
    val node : n index -> n index -> n index
    val import : Transition.any index Mcop.solution -> n index

    val inject : Inner.n index -> n index
    val split : n index -> (Transition.any index, Inner.n index) either

    module Freeze() : sig
      type definition =
        | Leaf of Transition.any index
        | Node of (n index * n index)
      val definition : n index -> definition
      val define : Inner.n index -> n index * n index
    end
  end = struct
    module Inner = Gensym()
    include (val sum Transition.any Inner.n)

    let leaf = inj_l
    let inject = inj_r
    let split = prj

    type pack = n index * n index
    let pack t u = (t, u)
    let unpack x = x

    (* Negligible performance impact
       type pack = int
       let pack t u = (Index.to_int t lsl 32) lor (Index.to_int u)
       let unpack x =
         let t = x lsr 32 in
         let u = x land (1 lsl 32 - 1) in
         (Index.of_int n t, Index.of_int n u)
    *)

    let node_table : (pack, Inner.n index) Hashtbl.t = Hashtbl.create 7

    let node l r =
      let p = pack l r in
      let node_index =
        try Hashtbl.find node_table p
        with Not_found ->
          let i = Inner.fresh () in
          Hashtbl.add node_table p i;
          i
      in
      inj_r node_index

    let rec import = function
      | Mcop.Matrix l -> leaf l
      | Mcop.Product (l, r) -> node (import l) (import r)

    module Freeze () =
    struct
      let rev_index = Vector.make' Inner.n
          (fun () -> let dummy = Index.of_int n 0 in (dummy, dummy))

      let define ix = rev_index.%(ix)

      let () =
        Hashtbl.iter
          (fun pair index -> rev_index.%(index) <- unpack pair)
          node_table;

      type definition =
        | Leaf of Transition.any index
        | Node of (n index * n index)

      let definition t =
        match prj t with
        | L tr -> Leaf tr
        | R ix -> Node (define ix)
    end
  end

  module CTree = ConsedTree()

  let nt_equations =
    Vector.init Transition.nt @@ fun tr ->
    let first_dim =
      Array.length (Classes.before_transition (Transition.of_nt tr))
    in
    let transition_size tr =
      Array.length (Classes.after_transition tr)
    in
    let nullable, non_nullable =
      MList. partition_map begin fun {Reductions. steps; lookahead; _} ->
        let dimensions = first_dim :: List.map transition_size steps in
        match Mcop.dynamic_solution (Array.of_list dimensions) with
        | exception Mcop.Empty -> `L lookahead
        | solution ->
          let steps = Array.of_list steps in
          let solution = Mcop.map_solution (fun i -> steps.(i)) solution in
          `R (CTree.import solution, lookahead)
      end (Reductions.of_transition tr)
    in
    (List.fold_left TerminalSet.union TerminalSet.empty nullable, non_nullable)

  module CNode = CTree.Freeze()

  let () = Time.tick "Ijkstra: built equation tree"

  module CClasses = struct
    let before_node = Vector.make CTree.Inner.n [||]
    let after_node = Vector.make CTree.Inner.n [||]

    let before t = match CTree.split t with
      | L tr -> Classes.before_transition tr
      | R ix -> before_node.%(ix)

    let after t = match CTree.split t with
      | L tr -> Classes.after_transition tr
      | R ix -> after_node.%(ix)

    let () =
      Index.iter CTree.Inner.n @@ fun node ->
      let l, r = CNode.define node in
      before_node.%(node) <- before l;
      after_node.%(node) <- after r;
  end

  let () = Time.tick "Ijkstra: classes for each tree node"

  type pre_map =
    | Pre_identity
    | Pre_singleton of int

  type post_map = {
    forward: int array array;
    backward: int array;
  }

  let pre_map outer inner =
    if outer == inner then
      Some (Pre_identity)
    else (
      assert (Array.length inner = 1);
      assert (TerminalSet.is_singleton inner.(0));
      let t = TerminalSet.choose inner.(0) in
      match MArray.findi (fun _ ts -> TerminalSet.mem t ts) 0 outer with
      | i -> Some (Pre_singleton i)
      | exception Not_found ->
        Printf.eprintf "WARNING: Missing transition on %s; partitions:\n"
          (Terminal.print t);
        Array.iter (fun ts ->
            Printf.eprintf "- %s\n" (TerminalSet.print ts)
          ) outer;
        None
    )

  let quick_subset a b =
    let result = TerminalSet.quick_subset a b in
    (*if result then (*sanity*)assert (TerminalSet.subset a b);*)
    result

  (*let iter_premap outer premap f =
      match premap with
      | Pre_identity -> Array.iteri f outer | Pre_singleton i -> f i outer.(i)*)

  (*let iter_premap_forward outer premap f =
      match premap with
      | Pre_identity -> Array.iteri f outer
      | Pre_singleton i -> f 0 outer.(i)*)

  let post_map ?lookahead before after =
    let forward_size = Array.make (Array.length before) 0 in
    let backward =
      Array.map (fun ca ->
          let keep = match lookahead with
            | None -> true
            | Some la -> quick_subset ca la
          in
          if keep then (
            match MArray.findi (fun _ cb -> quick_subset ca cb) 0 before with
            | exception Not_found -> -1
            | i -> forward_size.(i) <- 1 + forward_size.(i); i
          ) else (-1)
        ) after
    in
    let forward = Array.map (fun sz -> Array.make sz 0) forward_size in
    Array.iteri (fun i_b i_f ->
        if i_f <> -1 then (
          let pos = forward_size.(i_f) - 1 in
          forward_size.(i_f) <- pos;
          forward.(i_f).(pos) <- i_b
        )
      ) backward;
    { forward; backward }

  module Vars : sig
    type cell = private int
    val cell_count : int

    val encode_index : CTree.n index -> int -> int -> cell
    (*val decode_index : cell -> CTree.n index * int * int*)

    val cost : cell -> int
    val append_word : cell -> Terminal.t list -> Terminal.t list
  end = struct
    type cell = int

    let cell_count = ref 0

    let cost_table =
      Vector.init CTree.n (fun node ->
          let count =
            Array.length (CClasses.before node) *
            Array.length (CClasses.after node)
          in
          cell_count := !cell_count + count;
          Array.make count max_int
        )

    let cell_count = !cell_count

    let shift =
      let rec loop i =
        if cardinal CTree.n <= 1 lsl i
        then i
        else loop (i + 1)
      in
      loop 0

    let encode_index2 node cell =
      Index.to_int node lor (cell lsl shift)

    let cell_index node =
      let sz = Array.length (CClasses.after node) in
      fun i_b i_a -> (i_b * sz + i_a)

    let encode_index node =
      let cell_index = cell_index node in
      fun i_b i_a -> encode_index2 node (cell_index i_b i_a)

    let decode_index2 i =
      let node = Index.of_int CTree.n (i land (1 lsl shift - 1)) in
      (node, i lsr shift)

    let decode_index i =
      let node, offset = decode_index2 i in
      let sz = Array.length (CClasses.after node) in
      (node, offset / sz, offset mod sz)

    (*(* Sanity checks *)
      let decode_index i =
      let n, b, a as result = decode_index i in
      assert (i = encode_index n b a);
      result

      and encode_index node i_before i_after =
      let result = encode_index node i_before i_after in
      assert (decode_index result = (node, i_before, i_after));
      result*)

    type reverse_dependency =
      | Eqn of pre_map * int array array * Transition.nt index
      | Inner of post_map * CTree.Inner.n index

    let dependencies =
      Vector.make CTree.n []

    let min_cost a b : int =
      if a < b then a else b

    (*let add_cost a b =
      let result = a + b in
      if result < 0 then max_int else result*)

    module G = struct
      type variable = int

      let foreach_root f =
        Index.iter Transition.any begin fun tr ->
          let node = CTree.leaf tr in
          let before = CClasses.before node in
          let after = CClasses.after node in
          let count = Array.length before * Array.length after in
          let cells = cost_table.%(node) in
          for i = 0 to count - 1 do
            let p = cells.(i) in
            if p < max_int then f (encode_index2 node i) p
          done
        end

      let foreach_successor index cost f =
        (*Printf.printf "successor cost = %d\n" cost;*)
        assert (cost < max_int);
        let node, i_before, i_after = decode_index index in
        let update_dep = function
          | Eqn (pre, post, parent) ->
            let parent = CTree.leaf (Transition.of_nt parent) in
            let i_before' = match pre with
              | Pre_singleton i -> i
              | Pre_identity -> i_before
            in
            let parent_index = encode_index parent in
            Array.iter
              (fun i_after' -> f (parent_index i_before' i_after') cost)
              post.(i_after)
          | Inner (inner, parent) ->
            let l, r = CNode.define parent in
            let parent_index = encode_index (CTree.inject parent) in
            if l = node then (
              let r_costs = cost_table.%(r) in
              let cell_index = cell_index r in
              for i_after' = 0 to Array.length (CClasses.after r) - 1 do
                let r_cost = Array.fold_left
                    (fun r_cost i_before' ->
                       min_cost r_cost r_costs.(cell_index i_before' i_after'))
                    max_int inner.forward.(i_after)
                in
                if r_cost < max_int then (
                  f (parent_index i_before i_after') (cost + r_cost)
                )
              done
            ) else (
              (*sanity*)assert (r = node);
              match inner.backward.(i_before) with
              | -1 -> ()
              | l_post ->
                let l_costs = cost_table.%(l) in
                let cell_index = cell_index l in
                for i_before = 0 to Array.length (CClasses.before l) - 1 do
                  let l_cost = l_costs.(cell_index i_before l_post) in
                  if l_cost < max_int then (
                    f (parent_index i_before i_after) (l_cost + cost)
                  )
                done
            )
        in
        List.iter update_dep dependencies.%(node)

      type property = int
    end

    let () =
      Index.iter CTree.n begin fun node ->
        match CTree.split node with
        | L tr ->
          let before = CClasses.before node in
          let after = CClasses.after node in
          begin match Transition.split tr with
            | R _ ->
              (*sanity*)assert (Array.length before = 1);
              (*sanity*)assert (Array.length after = 1);
              cost_table.%(node).(0) <- 1;
            | L nt ->
              let nullable, non_nullable = nt_equations.%(nt) in
              let cell_index = cell_index node in
              if not (TerminalSet.is_empty nullable) then (
                Array.iteri begin fun i_a a ->
                  if quick_subset a nullable then
                    Array.iteri begin fun i_b b ->
                      if not (TerminalSet.disjoint b a) then (
                        (*Printf.eprintf
                          "nullable:\n\
                          \  nt: %s\n\
                          \  lookahead: %s\n\
                          \  before: %s\n\
                          \  after: %s\n\
                          \n"
                          (Nonterminal.print false (Transition.symbol_nt nt))
                          (TerminalSet.print nullable)
                          (TerminalSet.print b)
                          (TerminalSet.print a);*)
                        cost_table.%(node).(cell_index i_b i_a) <- 0
                      )
                    end before
                end after
              );
              List.iter begin fun (node', lookahead) ->
                match pre_map before (CClasses.before node') with
                | None -> ()
                | Some pre ->
                  let after' = CClasses.after node' in
                  let post = post_map after' after ~lookahead in
                  dependencies.%::(node') <- Eqn (pre, post.forward, nt)
              end non_nullable
          end
        | R inner ->
          let (l, r) = CNode.define inner in
          (*(*sanity*)assert (CClasses.before l == CClasses.before node);*)
          (*(*sanity*)assert (CClasses.after r == CClasses.after node);*)
          let c1 = CClasses.after l in
          let c2 = CClasses.before r in
          let mapping = post_map c1 c2 in
          let dep = Inner (mapping, inner) in
          assert (Array.length c2 = Array.length mapping.backward);
          dependencies.%::(l) <- dep;
          dependencies.%::(r) <- dep
      end

    let () = Time.tick "Ikjstra: reverse dependencies"

    let () =
      if false then
        Printf.printf "%d nodes and %d cells in consed tree\n"
          (cardinal CTree.n) cell_count

    include Fix.DataFlow.ForCustomMaps
        (struct
          type property = G.property
          let leq_join = min_cost
        end)
        (G)
        (struct
          let get index =
            let node, offset = decode_index2 index in
            cost_table.%(node).(offset)

          let set index v =
            let node, offset = decode_index2 index in
            cost_table.%(node).(offset) <- v
        end)
        (struct
          let data = Vector.map
              (fun costs -> Bytes.make (Array.length costs) '\x00')
              cost_table

          let get var =
            let node, cell = decode_index2 var in
            Bytes.get data.%(node) cell <> '\x00'

          let set var value =
            let node, cell = decode_index2 var in
            Bytes.set data.%(node) cell (if value then '\x01' else '\x00')
        end)

    let () = Time.tick "Ijkstra: data flow solution"

    let cost index =
      let node, offset = decode_index2 index in
      cost_table.%(node).(offset)

    let rec append_word cell acc =
      let node, i_b, i_a = decode_index cell in
      match CTree.split node with
      | L tr ->
        begin match Transition.split tr with
          | R t -> Transition.symbol_t t :: acc
          | L nt ->
            let nullable, non_nullable = nt_equations.%(nt) in
            let b = (CClasses.before node).(i_b) in
            let a = (CClasses.after node).(i_a) in
            if not (TerminalSet.is_empty nullable) &&
               quick_subset a nullable &&
               not (TerminalSet.disjoint b a) then
              acc
            else
              let current_cost = cost cell in
              match
                MList.find_map (fun (node', lookahead) ->
                    let costs = cost_table.%(node') in
                    let cell_index = cell_index node' in
                    if TerminalSet.disjoint a lookahead then None
                    else match CClasses.before node' with
                      | [|b'|] when TerminalSet.disjoint b' b -> None
                      | b' ->
                        match
                          MArray.findi
                            (fun _ b' -> TerminalSet.quick_subset b' b)
                            0 b',
                          MArray.findi
                            (fun _ a' -> TerminalSet.quick_subset a a')
                            0 (CClasses.after node')
                        with
                        | exception Not_found -> None
                        | i_b', i_a' ->
                          let cell = cell_index i_b' i_a' in
                          if costs.(cell) = current_cost
                          then Some (encode_index2 node' cell)
                          else None
                  ) non_nullable
              with
              | None ->
                Printf.eprintf "abort, cost = %d\n%!" current_cost;
                assert false
              | Some cell' -> append_word cell' acc
        end
      | R inner ->
        let current_cost = cost cell in
        let l, r = CNode.define inner in
        let mapping = post_map (CClasses.after l) (CClasses.before r) in
        let exception Break of Terminal.t list in
        let l_index = encode_index l in
        let r_index = encode_index r in
        try
          Array.iteri (fun i_al i_brs ->
              let l_cost = cost (l_index i_b i_al) in
              Array.iter (fun i_br ->
                  let r_cost = cost (r_index i_br i_a) in
                  if l_cost + r_cost = current_cost then (
                    let acc = append_word (r_index i_br i_a) acc in
                    let acc = append_word (l_index i_b i_al) acc in
                    raise (Break acc)
                  )
                ) i_brs
            ) mapping.forward;
          assert false
        with Break acc -> acc

  end


  (*let () =
    let max_word = ref (-1) in
    Index.iter Transition.nt begin fun nt ->
    let minimum = ref max_int in
    let node = CTree0.leaf (Transition.of_nt nt) in
    let before = CClasses.before node in
    let after = CClasses.after node in
    for i_b = 0 to Array.length before - 1 do
      for i_a = 0 to Array.length after - 1 do
        minimum := min !minimum
            Vars.cost_table.(Vars.encode_index node i_b i_a)
      done
    done;
    if !minimum < max_int then
      max_word := max !max_word !minimum
    end;
    Printf.printf "max word length: %d\n%!" !max_word*)

  (*let () = Gc.print_stat stderr*)

  module Word = struct
    type t = Terminal.t list
    let singleton x = [x]
    let elements xs = xs
    let compare xs1 xs2 = MList.compare Terminal.compare xs1 xs2
    let length = List.length
  end

  module Graph = struct
    (* A vertex is a pair [s, z], where [z] is a real terminal symbol. *)
    type node = Lr1C.n index * int

    let state (s, _c) =
      Lr1C.to_g s

    let lookaheads (s, c) =
      (Classes.for_lr1 s).(c)

    let equal (s1, c1 : node) (s2, c2 : node) =
      Int.equal (s1 :> int) (s2 :> int) && Int.equal c1 c2

    let hash (node : node) =
      Hashtbl.hash node

    (* An edge is labeled with a word. *)
    type label = Vars.cell

    let append_word = Vars.append_word

    (* We search forward from every [s, z], where [s] is an initial state. *)
    let sources f =
      ProductionMap.iter begin fun _ lr1 ->
        let lr1 = Lr1C.of_g lr1 in
        (* In practice, there should be only one class for an initial state. *)
        let classes = Classes.for_lr1 lr1 in
        assert (Array.length classes = 1);
        for i = 0 to Array.length classes - 1 do
          f (lr1, i)
        done
      end Lr1.entry

    (* The successors of [s, z] are defined as follows. *)
    let successors (source, i_b : node) (edge : label -> int -> node -> unit) =
      let before = Classes.for_lr1 source in
      List.iter begin fun tr ->
        match
          match Classes.before_transition tr with
          | [|b|] when TerminalSet.is_singleton b ->
            if TerminalSet.disjoint b before.(i_b) then
              None
            else
              Some 0
          | _ -> Some i_b
        with
        | None -> ()
        | Some i_b ->
          let after = Classes.after_transition tr in
          let target = Transition.target tr in
          let after' = Classes.for_lr1 target in
          let mapping = post_map after after' in
          Array.iteri begin fun i_a i_a's ->
            let cell = Vars.encode_index (CTree.leaf tr) i_b i_a in
            let cost = Vars.cost cell in
            if cost < max_int then
              Array.iter (fun i_a' -> edge cell cost (target, i_a')) i_a's
          end mapping.forward
      end (Transition.successors source)
  end

  module Statistics = struct
    let header = ""

    let print _ ~time:_ ~heap:_ =
      failwith "TODO"
  end

  (* Workaround unused values *)
  let _ = Transition.t
  let _ = Transition.of_t
  let _ = Transition.find
  let _ = Transition.symbol_nt
  let _ = CTree.node
  let _ = CNode.definition
  let _ = Vars.cell_count

  let () =
    if X.validate then begin
      let module Classic = LRijkstraClassic.Run(X)() in
      Index.iter Transition.nt begin fun tr ->
        let nt = Transition.symbol_nt tr in
        let tr = Transition.of_nt tr in
        let node = CTree.leaf tr in
        let lr1 = Transition.source tr in
        (*let lr1' = Transition.target tr in
          Printf.eprintf "ijkstra: checking item %d/%d: %d-%s->%d\n%!"
          (tr :> int) (Transition.nt :> int)
          (lr1 :> int) (Nonterminal.print false nt) (lr1' :> int)
          ;*)
        let missing classes =
          let diff a b = TerminalSet.fold TerminalSet.remove b a in
          Array.fold_left diff TerminalSet.universe classes
        in
        let missing_before = missing (CClasses.before node) in
        let missing_after = missing (CClasses.after node) in
        TerminalSet.iter begin fun t ->
          Classic.query (Lr1C.to_g lr1) nt t (fun _ _ -> assert false)
        end missing_before;
        Array.iteri begin fun i_b s_b ->
          let min_table = Hashtbl.create 7 in
          TerminalSet.iter begin fun t ->
            Classic.query (Lr1C.to_g lr1) nt t begin fun w t' ->
              if TerminalSet.mem t' missing_after then (
                Printf.eprintf "not expecting %s in {%s}\n%!"
                  (Terminal.print t')
                  (TerminalSet.print missing_after);
                assert false;
              );
              let n = Classic.Word.length w in
              match Hashtbl.find_opt min_table t' with
              | Some (_, n') when n' <= n -> ()
              | _ -> Hashtbl.replace min_table t' (w, n)
            end
          end s_b;
          Array.iteri begin fun i_a s_a ->
            let cell = Vars.encode_index node i_b i_a in
            let cost = Vars.cost cell in
            TerminalSet.iter begin fun t ->
              match Hashtbl.find min_table t with
              | (w, n)->
                if n <> cost then (
                  let ts ts = "{" ^ TerminalSet.print ts ^ "}" in
                  let word ts =
                    String.concat " " (List.map Terminal.print ts)
                  in
                  Printf.eprintf "  lengths differ: %d <> %d\n\
                                 \    before: %s\n\
                                 \    after: %s\n\
                                 \    LRijkstra: %s\n\
                                 \    Ijkstra: %s\n%!" n cost
                    (ts s_b) (ts s_a)
                    (word (Classic.Word.elements w))
                    (word (Vars.append_word cell []))
                  ;
                )
              | exception Not_found -> ()
            end s_a
          end (CClasses.after node)
        end (CClasses.before node);
      end
    end
end
