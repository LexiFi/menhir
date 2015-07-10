open Grammar

module W : sig

  type word
  val epsilon: word
  val singleton: Terminal.t -> word
  val append: word -> word -> word
  val length: word -> int
  val first: word -> Terminal.t (* word must be nonempty *)

end = struct

  type word = {
    data: Terminal.t Seq.seq;
    length: int;
  }

  let epsilon = {
    data = Seq.empty;
    length = 0;
  }

  (* TEMPORARY tabulate? *)
  let singleton t = {
    data = Seq.singleton t;
    length = 1;
  }

  let append w1 w2 =
    if w1.length = 0 then
      w2
    else if w2.length = 0 then
      w1
    else {
      data = Seq.append w1.data w2.data;
      length = w1.length + w2.length;
    }

  let length w =
    w.length

  let first w =
    Seq.first w.data

end

module Q = LowIntegerPriorityQueue

type assumption =
  Terminal.t option

type fact = {
  source: Lr1.node;
  height: int;
  target: Lr1.node;
  word: W.word;
  lookahead: assumption
}

let foreach_terminal f =
  Terminal.iter (fun t ->
    if not (Terminal.equal t Terminal.error) then
      f t
  )

exception Found

let has_nonterminal_transition s =
  try
    SymbolMap.iter (fun sym _ ->
      match sym with
      | Symbol.T _ ->
          ()
      | Symbol.N _ ->
          raise Found
    ) (Lr1.transitions s);
    false
  with Found ->
    true

(* This returns the list of reductions of [state] on token [z]. This
   is a list of zero or one elements. *)

let reductions s z =
  assert (not (Terminal.equal z Terminal.error));
  try
    TerminalMap.find z (Lr1.reductions s)
  with Not_found ->
    []

let q =
  Q.create()

let add fact =
  (* The length of the word serves as the priority of this fact. *)
  Q.add q fact (W.length fact.word)

let init s =
  if has_nonterminal_transition s then
    add {
      source = s;
      height = 0;
      target = s;
      word = W.epsilon;
      lookahead = None;
    }

let compatible lookahead t =
  assert (not (Terminal.equal t Terminal.error));
  match lookahead with
  | None ->
      true
  | Some t' ->
      Terminal.equal t t'

module T : sig
  val add: fact -> bool (* true if fact is new *)
  (* target/z *)
  val query: Lr1.node -> assumption -> (fact -> unit) -> unit
end = struct
  let add _ = assert false
  let query _ = assert false
end

(* The module [E] is in charge of recording the non-terminal edges that we have
   discovered, or more precisely, the conditions under which these edges can be
   taken. *)

module E : sig

  (* [register s nt w z] records that, in state [s], the outgoing edge labeled
     [nt] can be taken by consuming the word [w], if the next symbol satisfies
     [z]. *)
  val register: Lr1.node -> Nonterminal.t -> W.word -> assumption -> unit

  (* [query s nt a z] answers whether, in state [s], the outgoing edge labeled
     [nt] can be taken by consuming some word [w], under the assumption that
     the next symbol is [z], and under the constraint that the first symbol of
     [w.z] satisfies [a]. *)
  val query: Lr1.node -> Nonterminal.t -> assumption -> Terminal.t -> (W.word -> unit) -> unit

end = struct

  (* For now, we implement a mapping of [s, nt, a, z] to [w]. *)

  module M =
    Map.Make(struct
      type t = Lr1.node * Nonterminal.t * Terminal.t * Terminal.t
      let compare (s1, nt1, a1, z1) (s2, nt2, a2, z2) =
        let c = Lr1.Node.compare s1 s2 in
        if c <> 0 then c else
        let c = Nonterminal.compare nt1 nt2 in
        if c <> 0 then c else
        let c = Terminal.compare a1 a2 in
        if c <> 0 then c else
        Terminal.compare z1 z2
    end)

  let m =
    ref M.empty

  let rec register s nt w oz =
    match oz with
    | Some z ->
        let a = W.first (W.append w (W.singleton z)) in (* TEMPORARY can be optimised *)
        m := M.add (s, nt, a, z) w !m
    | None ->
        (* TEMPORARY naive *)
        foreach_terminal (fun z ->
          register s nt w (Some z)
        )

  let query s nt oa z f =
    match oa with
    | None ->
        (* TEMPORARY naive; search for every a, taking minimum; should begin with z *)
        assert false
    | Some a ->
        match M.find (s, nt, a, z) !m with
        | w -> f w
        | exception Not_found -> ()

end

let extend fact target w lookahead =
  (* TEMPORARY sanity check *)
  (* assert (compatible fact.lookahead (W.first (W.append w (W.singleton lookahead)))); *)
  {
    source = fact.source;
    height = fact.height + 1;
    target = target;
    word = W.append fact.word w;
    lookahead = lookahead
  }

let new_edge s nt w lookahead =
  E.register s nt w lookahead;
  T.query s lookahead (* TEMPORARY bug? *) (fun fact ->
    add (extend fact s w lookahead)
  )

(* [consequences fact] is invoked when we discover a new fact (i.e., one that
   was not previously known). It studies the consequences of this fact. These
   consequences are of two kinds:

   - As in Dijkstra's algorithm, the new fact can be viewed as a newly
   discovered vertex. We study its (currently known) outgoing edges,
   and enqueue new facts in the priority queue.

   - Sometimes, a fact can also be viewed as a newly discovered edge.
   This is the case when the word from [fact.source] to [fact.target]
   represents a production of the grammar and [fact.target] is willing
   to reduce this production. We record the existence of this edge,
   and re-inspect any previously discovered vertices which are
   interested in this outgoing edge.
*)
(**)

let consequences fact =

  (* 1. View [fact] as a vertex. Examine the transitions out of [fact.target]. *)
  
  SymbolMap.iter (fun sym s ->
    match sym with
    | Symbol.T t ->

        (* 1a. There is a transition labeled [t] out of [fact.target]. If the
           lookahead assumption [fact.lookahead] accepts [t], then we derive a
           new fact, where one more edge has been taken. We enqueue this new
           fact for later examination. *)
        (**)

        if compatible fact.lookahead t then
          add (extend fact s (W.singleton t) None)

    | Symbol.N nt ->

        (* 1b. There is a transition labeled [nt] out of [fact.target]. We
           need to know how this nonterminal edge can be taken. We query for a
           word [w] that allows us to take this edge. The answer depends on
           the terminal symbol [z] that comes *after* this word: we try all
           such symbols. Furthermore, we need the first symbol of [w.z] to
           satisfy the lookahead assumption [fact.lookahead], so the answer
           also depends on this assumption. *)
        (**)
      
        foreach_terminal (fun z ->
          E.query fact.target nt fact.lookahead z (fun w ->
            add (extend fact s w (Some z))
          )
        )
    
  ) (Lr1.transitions fact.target);

  (* 2. View [fact] as a possible edge. This is possible if the path from
     [fact.source] to [fact.target] represents a production [prod] and
     [fact.target] is willing to reduce this production. We check that
     [fact.height] equals the length of [prod]. This guarantees that
     reducing [prod] takes us all the way back to [fact.source]. Thus,
     this production gives rise to an edge labeled [nt] -- the left-hand
     side of [prod] -- out of [fact.source]. This edge is subject to the
     lookahead assumption [fact.lookahead], so we record that. *)
  (**)

  match Invariant.has_default_reduction fact.target with
  | Some (prod, _) ->
      if Production.length prod = fact.height then
        new_edge fact.source (Production.nt prod) fact.word fact.lookahead
  | None ->
      match fact.lookahead with
      | None ->
          TerminalMap.iter (fun z prods ->
            let prod = Misc.single prods in
            if Production.length prod = fact.height then
              new_edge fact.source (Production.nt prod) fact.word (Some z)
          ) (Lr1.reductions fact.target)
      | Some z ->
          let prods = reductions fact.target z in
          let prod = Misc.single prods in
          if Production.length prod = fact.height then
            new_edge fact.source (Production.nt prod) fact.word (Some z)

let discover fact =
  if T.add fact then
    consequences fact

let main () =
  Lr1.iter init;
  Q.repeat q discover
