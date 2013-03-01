(* TEMPORARY assumption: the transfer function must not recursively invoke
   the fixpoint function that we are in the process of defining! In order
   to check that, one could add a Boolean flag and dynamically check against
   reentrant calls to the client version of [get]. *)

(* TEMPORARY to emulate Smolka and Liu, add an optimization: when a node has
   reached a maximal value, it need not observe or be observed any more. *)

(* TEMPORARY in order to fully emulate Smolka and Liu, the transfer
   function should self-update so as to no longer ask for the value of
   a key that has been known to reach Top already... One could think
   of changing the type of the transfer function so as to make it
   apparent that it is a game between the client and the FIX module,
   and only a suffix of the game needs to be memorized if a prefix of
   the game has converged. *)

(* TEMPORARY in order to emulate Smolka and Liu, the transfer function
   should preserve bottom. As a result, when we attempt to evaluate a
   node whose descendants are uninitialized, evaluating the descendants
   produces bottom (and enqueues them), the node itself produces bottom,
   and no signal is sent to the node's parents -- which would be bad,
   since it has active descendants. In fact, one would like to guarantee
   that no descendant of an active node (a node in the worklist) is active.
   This might be possible if the graph is acyclic. A weaker property might
   be that if a node and its descendant are both active, then the descendant
   appears first in the worklist (so will be treated first). Again assumes
   acyclicity. Achieving this property might require assigning integer depths
   to nodes, and using a priority queue. One could even build a spanning tree
   of the dependency graph, but would it be useful? What about identifying
   the strongly connected components and guaranteeing that they are treated
   in topological order? *)

(* TEMPORARY on peut remplacer la KeyMap par un tableau, si on suppose les
   clefs numerotees, puisqu'on n'a pas besoin de la persistence. Mais cela
   a l'inconvenient d'exiger une enumeration totale du domaine des clefs,
   qu'on n'a peut-etre pas envie de faire. On pourrait alors exiger en entr'ee
   des maps non necessairement persistantes, sans preciser comment elles doivent
   etre implementees (tableaux sur options, hash table, ref sur map persistante...). *)

(* -------------------------------------------------------------------------- *)

(* Maps over keys. *)

module type KEY = sig
  type key
  type 'a t
  val empty: 'a t
  val add: key -> 'a -> 'a t -> 'a t
  val find: key -> 'a t -> 'a
  val map: ('a -> 'b) -> 'a t -> 'b t
end

(* -------------------------------------------------------------------------- *)

(* Imperative maps over keys. *)

module type IKEY = sig
  type key
  type 'a t
  val create: unit -> 'a t
  val add: key -> 'a -> 'a t -> unit
  val find: key -> 'a t -> 'a
  val map: ('a -> 'b) -> 'a t -> 'b t
end

(* -------------------------------------------------------------------------- *)

(* Properties. *)

module type PROPERTY = sig
  type property
  val bottom: property
  val maximal: property -> bool (* can be conservative, i.e. always return [false] *)
  val equal: property -> property -> bool
end

(* -------------------------------------------------------------------------- *)

(* Transfer functions. *)

module type TRANSFER = sig
  type key
  type property
  val transfer: (key -> property) -> (key -> property)
end

(* -------------------------------------------------------------------------- *)

(* An interface for querying the valuation produced by the least fixed point
   algorithm. *)

module type VALUATION = sig

  type key

  type property

  type 'a map

  (* The least fixed point of the transfer function. Invocations of [get]
     trigger the fixed point computation on demand. *)

  val get: key -> property

  (* [dump] produces an explicit representation of a fragment of the
     least fixed point -- the fragment that has been computed so far,
     due to earlier calls to [get]. No new computation is caused by
     invoking [dump]. *)

  val dump: unit -> property map

end

(* -------------------------------------------------------------------------- *)

(* The fixed point computation functor. *)

module IMake
  (K : IKEY)
  (P : PROPERTY)
  (T : TRANSFER with type key = K.key and type property = P.property)
 = struct

type key =
    K.key

type property =
    P.property

type 'a map =
    'a K.t

(* -------------------------------------------------------------------------- *)

(* Maintain a directed graph, where each node is annotated with a key (which
   is fixed) and a property (which can increase over time, and represents the
   current valuation). An edge from [node1] to [node2] means that [node1]
   observes [node2], that is, an update of the current valuation at [node2]
   will cause a signal to be sent to [node1]. A node can be its own successor. *)

type annotation = {

  (* The key associated with this node. *)

  key: key;

  (* The current valuation at this node. *)

  mutable valuation: property

}

type node =
    annotation ImperativeGraph.node

let graph : node map =
  K.create()

(* -------------------------------------------------------------------------- *)

(* Set up a worklist of nodes waiting to be processed.

   Invariant: a node in the worklist has no successors. (It can have
   predecessors.) This means that a predecessor (an observer) of some
   node is never in the worklist.

   Invariant: a node never appears twice in the worklist. This is
   because a node is inserted into the worklist only when it is first
   initialized or when it receives a signal from a successor node.
   Initialization only happens once, so it alone cannot account for a
   second insertion in the worklist. Furthermore, when a signal is
   sent, the nodes that are inserted cannot be on the worklist
   already, thanks to the previous invariant, and no node is inserted
   twice, because incoming edge lists have no duplicates.

   Invariant: whenever we receive control from or return control to
   the client, the worklist is empty. *)

   (* TEMPORARY Should the worklist be FIFO or LIFO? It seems that it
      should be LIFO, so as to perform depth-first discovery of the
      graph and bottom-up evaluation. This should speed things up. *)
   (* TEMPORARY note that stack will behave as a call-stack when the
      dependency graph is acyclic. In that case, the behavior is the
      same as that of a classic memoizing fixpoint combinator. *)

   (* TEMPORARY think more about complexity in practice *)

module Worklist : sig

  val insert: node -> unit
  val process: (node -> unit) -> unit
  val is_empty: unit -> bool

end = struct

  let worklist =
    Stack.create()

  let insert node =
    (* precondition: [x] has no successors *)
    Stack.push node worklist

  let rec process f =
    try
      while true do
	f (Stack.pop worklist)
      done
    with Stack.Empty ->
      ()

  let is_empty () =
    Stack.is_empty worklist

end

(* -------------------------------------------------------------------------- *)

(* [valuation node] returns the value of the current valuation at [node]. *)

let valuation node =
  (ImperativeGraph.data node).valuation

(* [lookup key] returns the graph node associated with the key [key].
   If no such node exists (because the key was yet unknown to us), it
   is created on the fly, and inserted into the worklist. *)

let lookup (key : key) : node =
  try
    K.find key graph
  with Not_found ->
    let node =
      ImperativeGraph.create {
        key = key;
	valuation = P.bottom;
      }
    in
    K.add key node graph;
    Worklist.insert node;
    node

(* -------------------------------------------------------------------------- *)

(* When a key broadcasts a signal, all of its predecessors (observers)
   receive the signal. Any key that receives the signal loses all of its
   successors (that is, it ceases to observe anything) and is inserted into
   the worklist. *)

let signal (subject : node) =
  List.iter (fun (observer : node) ->
    ImperativeGraph.clear_successors observer;
    Worklist.insert observer
  ) (ImperativeGraph.predecessors subject)
  (* postcondition: [subject] has no predecessors *)

(* -------------------------------------------------------------------------- *)

(* Worklist processing. *)

(* Complexity: [O(nh)+O(d)], where [n] is the number of keys, [h] is
   the height of the property preorder, and [d] is the total number of
   requests to [get] by the transfer function. *)

(* TEMPORARY in the case of Liu and Smolka, [h] is a constant, and [d]
   is linear in the size of the equation system, because the transfer
   function is self-updating. Hence, linear overall time complexity. *)

let process () =

  Worklist.process (fun (node : node) ->

    (* Retrieve the annotation carried by this node. *)

    let annotation = ImperativeGraph.data node in

    (* Retrieve the key associated with this node. *)

    let key : key = annotation.key in

    (* Retrieve the current valuation at this node. *)

    let previous : property = annotation.valuation in

    (* Compute an updated value at [key]. This is done by invoking the
       client-provided transfer function. We supply the client with a
       version of [get] that provides access to the current valuation
       and dynamically records dependencies. This yields a set of
       dependencies that can be reasonably fine-grained, and is
       correct by construction. *)

    let subjects : node list ref =
      ref [] in

    let get (key : key) : property =
      let node = lookup key in
      subjects := node :: !subjects;
      valuation node
    in

    let updated : property =
      T.transfer get key
    in

    (* At this point, [node] has no successors (subjects), since it was in the
       worklist. Thus, the precondition of [set_successors] is met. *)

    ImperativeGraph.set_successors node !subjects;

    (* If the updated value differs from the previous value, record
       the updated value and send a signal to all observers of [node]. *)

    if not (P.equal previous updated) then begin
      annotation.valuation <- updated;
      signal node
    end

  )

(* -------------------------------------------------------------------------- *)

(* Requests trigger evaluation. *)

let get key =
  assert (Worklist.is_empty());
  let node = lookup key in
  process();
  valuation node

(* [dump] produces an explicit representation of a fragment of the
   least fixed point -- the fragment that has been computed so far,
   due to earlier calls to [get]. No new computation is caused by
   invoking [dump]. *)

let dump () =
  assert (Worklist.is_empty());
  K.map valuation graph

 end

(* -------------------------------------------------------------------------- *)

(* Transfer functions in continuation passing style. *)

module type CPS_TRANSFER = sig
  type key
  type property
  type interaction =
    | Return of property
    | Read of key * (property -> interaction)
  val transfer: key -> interaction
end

(* TEMPORARY *)
(* 1. ecrire un foncteur qui transforme un CPS_TRANSFER en un TRANSFER memoisant. *)
(* 2. noter qu'on pourrait optimiser manuellement la composition des deux foncteurs, donc
      travailler directement sur un CPS_TRANSFER, et gagner un facteur constant. *)
(* 3. noter que transformer un TRANSFER en CPS_TRANSFER demande call/cc. *)

module CPS2Direct
  (K : IKEY)
  (P : PROPERTY)
  (T : CPS_TRANSFER with type key = K.key and type property = P.property)
     : TRANSFER with type key = K.key and type property = P.property
= struct

  type key =
      T.key

  type property =
      T.property

  type 'a map =
      'a K.t

  type interaction =
      T.interaction

  let interactions : interaction map =
    K.create()

  let lookup (key : key) : interaction =
    try
      K.find key interactions
    with Not_found ->
      let i : interaction = T.transfer key in
      K.add key i interactions;
      i

  let transfer (get : key -> property) : key -> property =
    
    let rec interpret (i : interaction) : property =
      match i with
      | T.Return p ->
	  p
      | T.Read (subject, continuation) ->
	  interpret (continuation (get subject))
    in

    let rec transfer (key : key) : property =
      match lookup key with
      | T.Return p ->
	  p
      | T.Read (subject, continuation) ->
	  let p : property = get subject in
	  if P.maximal p then begin
	    K.add key (continuation p) interactions;
	    transfer key
	  end
	  else
	    interpret (continuation p)
    in

    transfer

end

(* -------------------------------------------------------------------------- *)

(* KEY to IKEY. *)

module Make
  (K : KEY)
  (P : PROPERTY)
  (T : TRANSFER with type key = K.key and type property = P.property)
= struct

  module IM =
    IMake (struct
      type key = K.key
      type 'a t = 'a K.t ref
      let create () = ref K.empty
      let add k d t = t := K.add k d !t
      let find k t = K.find k !t
      let map f t = ref (K.map f !t)
    end) (P) (T)

  type key =
      K.key

  type property =
      P.property

  type 'a map =
      'a K.t

  let get =
    IM.get

  let dump () =
    !(IM.dump())

end

(* -------------------------------------------------------------------------- *)

(* The Boolean lattice. *)

(* TEMPORARY il suffit de changer bottom en [true] pour avoir un calcul de GFP.
   La fonction de transfert reste la meme... *)

module Boolean : PROPERTY with type property = bool = struct

  type property =
      bool

  let bottom =
    false

  let maximal p =
    p

  let equal (b1 : bool) (b2 : bool) =
    b1 = b2

end

(* -------------------------------------------------------------------------- *)

(* Systems of recursive Boolean equations. *)

module type EQUATIONS = sig

  type variable

  type rhs =
    | Disjunction of variable list
    | Conjunction of variable list

  val rhs: variable -> rhs

end

(* -------------------------------------------------------------------------- *)

(* Solving a system of recursive Boolean equations. This is done by computing
   the fixed point of an appropriate transfer function over the Boolean
   lattice. *)

module Solve (K : KEY) (E : EQUATIONS with type variable = K.key) =
  Make (K) (Boolean) (struct

    type key =
	K.key

    type property =
	bool

    open E

    let transfer get x =
      match rhs x with
      | Disjunction xs ->
	  List.fold_left (fun accu x ->
	    accu || get x
	  ) false xs
      | Conjunction xs ->
	  List.fold_left (fun accu x ->
	    accu && get x
	  ) true xs

  end)

(* -------------------------------------------------------------------------- *)

module SolveBis (K : IKEY) (E : EQUATIONS with type variable = K.key)
: CPS_TRANSFER with type key = K.key and type property = bool
= struct

  type key =
      K.key

  type property =
      bool

  open E

  type interaction =
      | Return of bool
      | Read of variable * (bool -> interaction)

  let transfer x =
    match rhs x with
    | Disjunction xs ->
	let rec disjunction xs =
	  match xs with
	  | [] ->
	      Return false
	  | x :: xs ->
	      Read (x, fun vx -> if vx then Return true else disjunction xs)
	in
	disjunction xs
    | Conjunction xs ->
	let rec conjunction xs =
	  match xs with
	  | [] ->
	      Return true
	  | x :: xs ->
	      Read (x, fun vx -> if not vx then Return false else conjunction xs)
	in
	conjunction xs
	  
end

