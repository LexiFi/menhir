(* This algorithm is based on dynamic programming. Its time and space
   complexity is linear in the number of entries that are eventually stored in
   the memoization table. This number itself is usually linear in the size of
   the DCST that is settled.

   Unfortunately, maintaining a hash table of linear size is expensive. Our
   benchmarks show that this algorithm can be about x50 slower than parsing.
   For this reason, we deprecate this algorithm and use a different (eager)
   algorithm in [Resolve.ml]. *)

let debug =
  false

module type HashedType =
  Hashtbl.HashedType

let (=) = Int.equal
  (* avoid inadvertent use of [(=)] at a type other than [int] *)

(* We do not actually need the subsumption relation [terminal :> int]. Any
   type [terminal] that supports equality and hashing would do. We require
   this relation because this simplifies things at the use site of the
   functor [Make]. *)

(* Similarly, we do not really need an injection [number] of [state] into
   [int]. We just need the type [state] to support equality and hashing. We
   require [number : state -> int] because this simplifies things at the use
   site of the functor [Make]. *)

module[@inline] Make (A : sig
  type terminal = private int
  type nonterminal
  type production = private int
  type state
  val number: state -> int
  type token
  val token2terminal: token -> terminal
  val lhs : production -> nonterminal
  val maybe_shift_t : state -> terminal -> state option
  val maybe_goto_nt : state -> nonterminal -> state option
  val may_reduce_prod : state -> terminal -> production -> bool
end) = struct
open A

(* -------------------------------------------------------------------------- *)

(* Terminal symbols. *)

module Terminal = struct
  type t = terminal
  let equal (t1 : terminal) (t2 : terminal) = (t1 :> int) = (t2 :> int)
  let hash (t : terminal) = (t :> int)
end

(* States. *)

module State = struct
  let equal (s1 : state) (s2 : state) = number s1 = number s2
  let hash (s : state) = number s
end

(* Productions. *)

module Production = struct
  let equal (prod1 : production) (prod2 : production) =
    (prod1 :> int) = (prod2 :> int)
  let hash (prod : production) = (prod :> int)
end

(* State-production pairs. *)

module SP = struct
  type t = state * production
  let hash (s, prod) =
    (State.hash s, Production.hash prod) |> Hashtbl.hash
  let equal (s1, prod1) (s2, prod2) =
    State.equal s1 s2 &&
    Production.equal prod1 prod2
end

(* -------------------------------------------------------------------------- *)

(* Sets of terminal symbols. *)

(* We use ordered sets (that is, lists without duplicate elements), as opposed
   to ordinary sets, because the order of the branches in a choice matters. We
   want the first branch to be tried first. *)

module TerminalSet =
  OrderedSet.Make(Terminal)

let empty, singleton, (@) =
  TerminalSet.(empty, singleton, union)

let delta (b : bool) (t : terminal) =
  if b then singleton t else empty

type fset =
  TerminalSet.t

(* -------------------------------------------------------------------------- *)

module CST = struct

type cst =
  | Terminal    of token
  | NonTerminal of production * cst array

end

type cst =
  CST.cst

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

module DCST = struct

(**We assign every node a unique identifier. This gives us efficient hashing
   of DCSTs, therefore efficient memoized functions on DCSTs (or on tuples
   that have a DCST as a component). Hash-consing (that is, maximal sharing)
   is not needed.

   Nullability and FIRST sets are computed once, bottom-up, when a DCST is
   constructed. One could instead define [nullable] and [first] as memoized
   functions on DCSTs, but this approach is simpler and more efficient. *)
type dcst = {

  uid: int;
  (**[uid] is the unique identifier of this DCST. *)

  shape: shape;
  (**[shape] is the shape of this DCST. *)

  nullable: bool;
  (**The Boolean flag [nullable] indicates whether this DCST is nullable. *)

  nullable_nonterminal: bool array;
  (**If this DCST has the form [NonTerminal (prod, dcsts)], then the
     immutable array [nullable_nonterminal] stores the nullability of
     every suffix of the sequence [dcsts]. This array has length [n+1],
     where [n] is the length of [dcsts]. *)

  first: fset;
  (**The set [first] is the FIRST set of this DCST. *)

  first_nonterminal: fset array;
  (**If this DCST has the form [NonTerminal (prod, dcsts)], then the
     immutable array [first_nonterminal] stores the FIRST set of every
     suffix of the sequence [dcsts]. This array has length [n+1],
     where [n] is the length of [dcsts]. *)

}

and shape =
  | Choice      of dcst * dcst
  | Terminal    of token
  | NonTerminal of production * dcst array

(* -------------------------------------------------------------------------- *)

(* A projector. *)

let project_nonterminal (dcst : dcst) : production * dcst array =
  match dcst.shape with
  | NonTerminal (prod, dcsts) ->
      prod, dcsts
  | Terminal _
  | Choice _ ->
      assert false

let[@inline] is_nonterminal (dcst : dcst) : bool =
  let _, _ = project_nonterminal dcst in true

(* -------------------------------------------------------------------------- *)

(* A CST is null if and only if its fringe (a sequence of terminal symbols)
   is the empty sequence. A DCST is nullable if and only if at least one of
   the CSTs that it represents is nullable. *)

(* Access. *)

let[@inline] nullable (dcst : dcst) : bool =
  dcst.nullable

let[@inline] nullable_nonterminal (dcst : dcst) : bool array =
  if debug then assert (is_nonterminal dcst);
  dcst.nullable_nonterminal

(* Initialization. *)

let init_nullable (shape : shape) : bool * bool array =
  let dummy = [||] in
  match shape with
  | Choice (dcst1, dcst2) ->
      (* A disjunction node is nullable if one of its branches is nullable. *)
      nullable dcst1 && nullable dcst2,
      dummy
  | Terminal _tok ->
      (* A terminal symbol is not nullable. *)
      false,
      dummy
  | NonTerminal (_prod, dcsts) ->
      (* At a non-terminal node, first compute [nv], an array of nullability
         bits. *)
      let n = Array.length dcsts in
      (* Allocate an array [nv] of size [n+1]. [true] is an appropriate
         value for [nv.(n)] because an empty sequence is nullable. *)
      let nv = Array.make (n+1) true in
      (* Initialize the remaining slots. A sequence is nullable if both of
         its components are nullable, hence the use of conjunction [&&]. *)
      for i = n-1 downto 0 do
        nv.(i) <- nv.(i+1) && nullable dcsts.(i)
      done;
      (* The bit at offset 0 in the array [nv] tells us whether the sequence
         [dcsts] is nullable. We return both this bit and the whole array. *)
      nv.(0), nv

(* -------------------------------------------------------------------------- *)

(* The FIRST set of a CST is a singleton set whose single element is the first
   terminal symbol in the fringe of this CST. (If the fringe is empty, then the
   FIRST set is the empty set.) *)

(* Access. *)

let[@inline] first (dcst : dcst) : fset =
  dcst.first

let[@inline] first_nonterminal (dcst : dcst) : fset array =
  if debug then assert (is_nonterminal dcst);
  dcst.first_nonterminal

(* Initialization. *)

let init_first (shape : shape) : fset * fset array =
  let dummy = [||] in
  match shape with
  | Choice (dcst1, dcst2) ->
      (* The FIRST set of a disjunction node is the union of the FIRST sets
         of the branches. *)
      first dcst1 @ first dcst2,
      dummy
  | Terminal tok ->
      (* The FIRST set of a terminal symbol is a singleton. *)
      singleton (token2terminal tok),
      dummy
  | NonTerminal (_prod, dcsts) ->
      (* At a non-terminal node, first compute [fv], an array of FIRST sets. *)
      let n = Array.length dcsts in
      (* Allocate an array [fv] of size [n+1]. [empty] is an appropriate value for
         [fv.(n)] because the FIRST set of an empty sequence is empty. *)
      let fv = Array.make (n+1) empty in
      (* Initialize the remaining slots. *)
      for i = n-1 downto 0 do
        let dcst = dcsts.(i) in
        fv.(i) <-
          if nullable dcst then
            first dcst @ fv.(i+1)
          else
            first dcst
      done;
      (* The entry at offset 0 in the array [fv] is the FIRST set of the
         sequence [dcsts]. We return both this set and the whole array. *)
      fv.(0), fv

(* -------------------------------------------------------------------------- *)

(* The head symbol of a DCST. *)

(* We could give separate definitions of the type [symbol], of the function
   [head], which returns the head symbol of a DCST, and of the function
   [transition], which takes a symbol as an argument.

   Instead, we give a direct definition of the function [transition_head],
   which is the composition of [head] and [transition]. This removes the
   need to define the type [symbol] and saves one memory allocation. *)

(* Omitted:

type symbol =
  | T of terminal
  | N of nonterminal

let rec head dcst : symbol =
  match dcst.shape with
  | Choice (dcst1, _dcst2) ->
      head dcst1
  | Terminal tok ->
      T (token2terminal tok)
  | NonTerminal (prod, _) ->
      N (lhs prod)

let transition (s : state) (symbol : symbol) : state option =
  match symbol with
  | T t ->
      maybe_shift_t s t
  | N nt ->
      maybe_goto_nt s nt

 *)

let rec transition_head (s : state) (dcst : dcst) : state option =
  match dcst.shape with
  | Choice (dcst1, _dcst2) ->
      transition_head s dcst1
  | Terminal tok ->
      maybe_shift_t s (token2terminal tok)
  | NonTerminal (prod, _) ->
      maybe_goto_nt s (lhs prod)

(* -------------------------------------------------------------------------- *)

(* Smart constructors for DCSTs. *)

(* An allocator of fresh unique identifiers. *)

let limit =
  ref 0

let next () : int =
  let y = !limit in
  limit := y + 1;
  y

let stats () =
  Printf.eprintf "%d DCST nodes have been allocated.\n%!" !limit

(* The auxiliary function [make] assigns a unique identifier to the new node.
   Furthermore, it computes [nullable] and [first] information. [make] is and
   must be the only way of constructing DCSTs; one must not forge DCSTs by
   other means. As far as the end user is concerned, [dcst] is presented as
   an abstract type; the user must go through the smart constructors. *)

let make shape =
  let uid = next() in
  let nullable, nullable_nonterminal = init_nullable shape
  and first, first_nonterminal = init_first shape in
  { uid; shape; nullable; nullable_nonterminal; first; first_nonterminal }

(* The smart constructor [nonterminal] could check that [prod] is not a
   start production, that the number of subtrees is correct, and that the
   head symbols of the subtrees are correct. The smart constructor [choice]
   could check that both branches of a choice have the same head symbol.
   Fortunately, if a strongly-typed view of DCSTs and CSTs is offered to the
   end user, then these checks are unnecessary. Such a strongly-typed view
   is *not* offered by the API of this module (so, this API is unsafe). A
   safe API *can* be constructed a posteriori on top of this unsafe API. *)

(* The smart constructor of [terminal] DCSTs. *)

let terminal tok =
  make (Terminal tok)

(* The smart constructor of [nonterminal] DCSTs. *)

let nonterminal prod dcsts =
  (* Omitted check:
  assert (not (is_start prod));
  let rhs = Production.rhs prod in
  assert (Array.length rhs = Array.length dcsts);
  assert (for i = 0 to Array.length rhs - 1 do
    let symbol, _, _ = rhs.(i) in
    assert (Symbol.equal symbol (head dcsts.(i)));
  done; true);
   *)
  make (NonTerminal (prod, dcsts))

(* The smart constructor of [choice] DCSTs. *)

let choice dcst1 dcst2 =
  (* Omitted check:
  assert (Symbol.equal (head dcst1) (head dcst2));
   *)
  make (Choice (dcst1, dcst2))

(* -------------------------------------------------------------------------- *)

(* Hashing and equality. *)

(* In the equality test, we can either compare the [uid] fields or use
   address equality. Address equality is more efficient and is in fact the
   only truly correct implementation: in the (unlikely) event of an integer
   overflow, the integers stored in [uid] fields would no longer be unique,
   so an equality test based on [uid] fields would be incorrect, yet address
   equality would still give correct results. Furthermore, hashing based on
   non-unique [uid] fields is still correct. *)

let equal dcst1 dcst2 =
  dcst1 == dcst2

let hash dcst =
  dcst.uid

end (* DCST *)

type dcst =
  DCST.dcst

type dcsts =
  dcst array

let nullable, nullable_nonterminal, first, first_nonterminal =
  DCST.(nullable, nullable_nonterminal, first, first_nonterminal)

(* Helper functions that compute FIRST sets. *)

(* [first_delta dcst t'] is the FIRST set of the tree [dcst] followed with
   the terminal symbol [t']. *)

let[@inline] first_delta dcst t' : fset =
  first dcst @ delta (nullable dcst) t'

(* [first_suffix_delta] is the FIRST set of the sequence of subtrees
   determined by [dcst] and [i], followed with the terminal symbol [t']. *)

let[@inline] first_suffix_delta dcst i t' : fset =
  let nv = nullable_nonterminal dcst
  and fv = first_nonterminal dcst in
  fv.(i) @ delta nv.(i) t'

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

(* Memoization of a recursive function. *)

(* We give a direct implementation because we do not wish to depend on the
   library Fix. *)

module[@inline] HashTablesAsImperativeMaps (H : HashedType) = struct

  include Hashtbl.Make(H)

  let[@inline] create () =
    create 1023

  let[@inline] add key data table =
    add table key data;
    data

  let[@inline] find key table =
    find table key

  let statistics table =
    let stats = stats table in
    Printf.eprintf "The table contains %d entries.\n%!"
      stats.num_bindings

end

type statistics =
  unit -> unit

type 'a fix =
  ('a -> 'a) -> statistics * 'a

module[@inline] Memoize (T : HashedType) : sig
  val fix : (T.t -> 'a) fix
end = struct

  module M = HashTablesAsImperativeMaps(T)

  let fix (ff : (T.t -> 'a) -> T.t -> 'a) : statistics * (T.t -> 'a) =
    let table = M.create() in
    let rec f x : 'a =
      try
	M.find x table
      with Not_found ->
        M.add x (ff f x) table
    in
    let stats () = M.statistics table in
    stats, f

end

(* -------------------------------------------------------------------------- *)

(* The [Option] monad. *)

(* Monadic notation. (This requires OCaml 4.08.) *)

let[@inline] ( let* ) o f =
  match o with
  | None -> None
  | Some x -> f x

let[@inline] return x =
  Some x

(* Failure. (The neutral element of choice.) *)

let fail =
  None

(* Success. This is just [return ()]. *)

let succeed =
  Some ()

(* Conditional failure. *)

let[@inline] require (b : bool) : unit option =
  if b then succeed else fail

(* Binary choice. *)

(* This is a prioritized choice: the left branch is tried first;
   if it fails, the second branch is tried. *)

let[@inline] choice (o1 : 'x -> 'b option) (o2 : 'x -> 'b option)
: 'x -> 'b option =
  fun x ->
    let outcome1 = o1 x in
    match outcome1 with
    | Some _ -> outcome1
    | None   -> o2 x

(* N-ary choice over a family that is indexed by a list. *)

let rec ibigchoice (xs : 'x list) (branch : 'x -> 'b option)
: 'b option =
  match xs with
  | [] ->
      fail
  | x :: xs ->
      choice branch (fun _ -> ibigchoice xs branch) x

(* N-ary choice over a family that is indexed by a set. *)

let[@inline] ibigchoice (ts : TerminalSet.t) (branch : terminal -> 'b option)
: 'b option =
  ibigchoice (TerminalSet.elements ts) branch

(* Use [let&] as a lightweight syntax for N-ary choice. *)

let[@inline] (let&) ts branch =
  ibigchoice ts branch

(* -------------------------------------------------------------------------- *)

(* The following functions test whether a certain path in the automaton exists,
   and if so, what state this path leads to. *)

(* [follow_suffix s dcsts i] follows the path that begins at the state [s] and
   whose edges are labeled with the head symbols of the trees found at offset
   [i] in the array [dcsts]. It returns [None] if this path does not exist.
   It returns [Some s'] if this path exists and leads to the state [s']. *)

let rec follow_suffix (s : state) (dcsts : dcsts) (i : int) : state option =
  let n = Array.length dcsts in
  if debug then assert (0 <= i && i <= n);
  if i = n then
    return s
  else
    let* s' : state = DCST.transition_head s dcsts.(i) in
    follow_suffix s' dcsts (i+1)

(* [follow] specializes [follow_suffix] with the offset 0. *)

let[@inline] follow (s : state) (dcsts : dcsts) : state option =
  follow_suffix s dcsts 0

(* Under the assumption that the trees [dcsts] form a valid right-hand side
   for the production [prod], this new definition of [follow prod s dcsts]
   returns the same result as [follow s dcsts] above. It is memoized, using
   a hash table whose keys are pairs [(s, prod)]. *)

(* Intuitively, it may seem that the parameter [dcsts] should not be needed,
   as the sequence of the head symbols of [dcsts] is the right-hand side of
   the production [prod], so it is determined by [prod]. However, we do not
   have a table that maps a production to its right-hand side. This is why
   [dcsts] is needed here (and it is fortunately at hand). *)

let follow : state -> production -> dcsts -> state option =
  let module T = Hashtbl.Make(SP) in
  let table = T.create 1023 in
  fun s prod dcsts ->
    try
      T.find table (s, prod)
    with Not_found ->
      let o = follow s dcsts in
      T.add table (s, prod) o;
      o

(* -------------------------------------------------------------------------- *)

(* A query [(dcst, s, t, t')] asks whether the DCST [dcst] can be settled, that
   is, converted to a CST [cst], subject to the following conditions:

     1. the first element of the sequence [fringe cst ++ [t']] must be [t];

     2. the parser, beginning in state [s] and fed with the input sequence
        [fringe cst ++ [t']], recognizes [cst], leaves [t'] unconsumed, and
        follows one edge (labeled with the symbol [head dcst]) from state [s]
        to some state [s'].

   If [cst] is not null then Condition 1 means that the first element of the
   sequence [fringe cst] must be [t]. If [cst] is null then Condition 1 means
   that the symbols [t] and [t'] must coincide.

   Condition 2 requires the parser's behavior to be independent of the stack.
   Whatever the initial stack may be, the parser ends up pushing one new cell
   on top of this existing stack. The new cell contains the state [s'] and
   corresponds to the CST [cst]. *)

type query =
  dcst * state * terminal * terminal

(* At a nonterminal node, whose array of subtrees [dcsts] has length [n], we
   settle each subtree in turn. Thus, we naturally formulate auxiliary queries
   about suffixes of the sequence [dcsts]. Such a suffix is identified by an
   integer index [i] such that [0 <= i && i <= n] holds. The subsequence of
   interest is then the suffix of [dcsts] delimited by the range [\[i, n)].

   Solving such a query amounts to solving a query for each subtree in turn
   and checking that, in the final state that is thus reached, production
   [prod] can be reduced. In fact, we make this check a precondition: the
   query cannot be formulated unless this property holds. *)

type suffix_query =
  dcst * int * state * terminal * terminal

(* The function [settle] answers a query with a success, denoted by [Some ()],
   or failure, denoted by [None]. In case of success, it does *not* return a
   pair [(cst, s')], as one might expect. This pair is computed in a separate
   readback phase. *)

(* We use [unit option], as opposed to [bool], because this is just as cheap
   and makes our code more uniform. *)

type settle =
  query -> unit option

(* The function [settle_suffix] answers a suffix query with either [None],
   which denotes failure, or [Some it], which denotes success, and (when
   [i < n] holds) indicates which intermediate terminal symbol [it] led to
   this success. It does *not* return a pair [(csts, s')], as one might
   expect. This pair is computed in a separate readback phase. *)

type settle_suffix =
  suffix_query -> terminal option

(* The functions [settle] and [settle_suffix] are mutually recursive.

   The function [settle_suffix] is memoized: this is vital, as it allows shared
   subgoals to be recognized. The function [settle] is not memoized: indeed,
   after a small amount of computation, it terminates or calls itself or calls
   [settle_suffix]. Memoizing it would not be worthwhile; we find that, by not
   memoizing it, we save a factor of 1.5x or 2x in time.

   We first define an open version of each function, which is parameterized
   over [settle_suffix]. Then, using the memoizing fixed point combinator
   [fix], we obtain a closed version of [settle_suffix], and, from there, we
   get [settle]. *)

(* [settle] has the precondition that the state [s] must have an outgoing
   transition whose label is the head symbol of the tree [dcst]. *)

let rec settle
  (settle_suffix : settle_suffix)
: settle =
fun ((dcst, s, t, t') as query) ->
  if debug then assert (DCST.transition_head s dcst <> None);

  (* Compute the FIRST set of the sequence [dcst; t']. Then, check that [t] is
     a member of this set. If this is not the case, then Condition 1 cannot be
     satisfied, so we fail. When going down into a choice, this test guides us
     down into the correct branch, as selected by the terminal symbol [t]. *)
  if TerminalSet.mem t (first_delta dcst t') then
    settle_checked settle_suffix query
  else
    fail

(* [settle_checked] has the additional precondition that [t] is a member of
   the set [first_delta dcst t']. *)

and settle_checked
  (settle_suffix : settle_suffix)
: settle =
fun (dcst, s, t, t') ->
  if debug then assert (TerminalSet.mem t (first_delta dcst t'));

  (* Analyze the tree [dcst]. *)
  match dcst.shape with

  | DCST.Choice (dcst1, dcst2) ->
      (* Perform a prioritized choice. Try the left possibility first;
         if it fails, try the second possibility. Note, however, that
         the constraint imposed by the parameter [t] may well mean that
         only one branch can hope to succeed, anyway. *)
      (* TODO explain why second call cannot be [settle_checked] *)
      choice
        (fun () -> settle settle_suffix (dcst1, s, t, t'))
        (fun () -> settle settle_suffix (dcst2, s, t, t'))
        ()

  | DCST.Terminal tok ->
      (* This DCST is not nullable, and its FIRST set contains just [tok].
         The above assertion implies that [tok] must match [t]. *)
      if debug then assert (Terminal.equal (token2terminal tok) t);
      (* Succeed. *)
      succeed

  | DCST.NonTerminal (prod, dcsts) ->
      (* A fast path. If the path from state [s], following the symbols
         in the right-hand side of production [prod], does not exist in
         the automaton, or if this path exists but leads to a state [s']
         where production [prod] cannot be reduced, then this query has
         no solution. *)
      let* s' : state = follow s prod dcsts in
      let* () = require (may_reduce_prod s' t' prod) in
      (* Settle each of the subtrees in turn and check that, in the final
         state of this branch, production [prod] can be reduced. *)
      let* _ : terminal = settle_suffix (dcst, 0, s, t, t') in
      (* Succeed. *)
      succeed

(* [settle_suffix] has the precondition that there must exist a path out of the
   state [s] labeled with the symbols in the right-hand side of production
   [prod] (beginning at offset [i]) and that, in the final state of this path,
   it must be permitted to reduce production [prod]. *)

(* [settle_suffix] has the additional precondition that [t] must be a member of
   the set [first_suffix_delta dcst i t']. *)

let settle_suffix
  (settle_suffix : settle_suffix)
: settle_suffix =
fun (dcst, i, s, t, t') ->
  let prod, dcsts = DCST.project_nonterminal dcst in
  let n = Array.length dcsts in
  if debug then assert (0 <= i && i <= n);
  if debug then assert (TerminalSet.mem t (first_suffix_delta dcst i t'));

  (* If [i = n] holds, then we are being asked to settle an empty sequence. *)
  if i = n then begin
    (* The equality [t = t'] follows from the above precondition. *)
    if debug then assert (Terminal.equal t t');
    (* Our precondition guarantees that in state [s], with lookahead symbol
       [t'], reducing production [prod] is permitted. *)
    if debug then assert (may_reduce_prod s t' prod);
    (* Succeed. *)
    let dummy : terminal = t' in
    return dummy
  end
  else

  (* If [i < n] holds, then we settle the subtree [dcsts.(i)] first, followed
     with the remaining subtrees. This requires recursive calls to [settle] and
     [settle_suffix]. A certain intermediate terminal symbol [it] must be both
     the first unconsumed symbol while solving the subtree [dcsts.(i)] and the
     first symbol while solving the remaining subtrees. We cannot know for
     sure which symbol this might be, so we try all possible symbols, under
     the constraint that [it] must be a member of the FIRST set of the
     sequence (remaining subtrees; t'). *)
  let* is : state = DCST.transition_head s dcsts.(i) in (* this cannot fail *)
  let& it = first_suffix_delta dcst (i+1) t' in
  let* () = settle_checked settle_suffix (dcsts.(i), s, t, it) in
  let* _ : terminal =
    (* An optional optimization: instead of performing a recursive call to
       [settle_suffix], which would always be valid, if we see that this call
       would be a base case, then we inline it. Thus, we save the time and
       space of a memoized function call. On the other hand, some time can be
       wasted if this function call is repeated; then, work is duplicated. We
       find that this optimization is worthwhile; it saves about 10% in time,
       overall, and leads to a 25% reduction in the size of the memoization
       table. *)
    if i+1 = n then begin
      (* This is the base case above, inlined, with [s] replaced by [is]. *)
      if debug then assert (may_reduce_prod is t' prod);
      let dummy : terminal = t' in
      return dummy
    end
    else
      settle_suffix (dcst, i+1, is, it, t')
  in
  return it

(* -------------------------------------------------------------------------- *)

(* Readback. *)

(* Separating [settle] and [readback] allows us to store a smaller amount of
   information in the memoization table maintained by [settle]. The missing
   information is reconstructed during the readback phase, which constructs
   a concrete syntax tree.

   This should be worthwhile if the dynamic programming algorithm explores
   many subgoals before finding a solution. Our benchmarks suggest that
   this is not the case: a small number of subgoals are actually explored.
   Therefore, separating the readback phase is probably not worthwhile.
   We keep the code in this form anyway, for the record. *)

let rec readback
  (settle : settle)
  (settle_suffix : settle_suffix)
 : query -> cst =
fun (dcst, s, t, t') ->

  match dcst.shape with

  | DCST.Choice (dcst1, dcst2) ->
      let query1 = (dcst1, s, t, t') in
      begin match settle query1 with
      | Some () -> readback settle settle_suffix query1
      | None    -> readback settle settle_suffix (dcst2, s, t, t')
      end

  | DCST.Terminal tok ->
      CST.Terminal tok

  | DCST.NonTerminal (prod, dcsts) ->
      (* Read back each of the subtrees in turn and store them directly
         into a new array [csts]. *)
      let n = Array.length dcsts in
      let s = ref s in
      let t = ref t in
      let csts = Array.init n @@ fun i ->
        let query = (dcst, i, !s, !t, t') in
        (* [settle_suffix] has recorded the intermediate symbol [it] that
           was used to settle this query, so we do not need to enumerate
           all possible choices of [it]. *)
        let it : terminal = Option.get (settle_suffix query) in
        let is : state = Option.get (DCST.transition_head !s dcsts.(i)) in
        let cst = readback settle settle_suffix (dcsts.(i), !s, !t, it) in
        s := is;
        t := it;
        cst
      in
      (* Construct a CST node above the subtrees [csts]. *)
      CST.NonTerminal (prod, csts)

(* -------------------------------------------------------------------------- *)

(* Hashing and equality functions for queries. *)

module SuffixQuery = struct
  type t =
    suffix_query
  let hash (dcst, i, s, t, t') =
    (DCST.hash dcst, i, State.hash s, Terminal.hash t, Terminal.hash t')
    |> Hashtbl.hash
  let equal (dcst1, i1, s1, t1, t'1) (dcst2, i2, s2, t2, t'2) =
    DCST.equal dcst1 dcst2 &&
    i1 = i2 &&
    State.equal s1 s2 &&
    Terminal.equal t1 t2 &&
    Terminal.equal t'1 t'2
end

module M =
  Memoize(SuffixQuery)

(* -------------------------------------------------------------------------- *)

(* A public entry point. *)

(* We remove the parameter [t]. The parameters [dcst], [s], [t'] remain. *)

let info =
  false

let settle (dcst, s, t') =
  (* Construct a memoized [settle] function, starting off with an empty
     memoization table. This table does not persist across invocations
     of the public function [settle]. *)
  let stats, settle_suffix = M.fix settle_suffix in
  let settle query = settle settle_suffix query in
  (* We try every terminal symbol [t] in the set [first dcst], so [settle]
     can be invoked several times, until it succeeds; then [readback] is
     invoked, and is invoked at most once. *)
  let& t = first dcst in
  let* _ : state = DCST.transition_head s dcst in
  let* () = settle (dcst, s, t, t') in
  let cst = readback settle settle_suffix (dcst, s, t, t') in
  (* If [info] is set, print internal statistics. *)
  if info then begin
    (* The number of DCST nodes created since the very beginning. *)
    DCST.stats();
    (* The size of the memoization tables built by THIS run of [settle]. *)
    stats()
  end;
  (* Return just [cst]. *)
  return cst

end
