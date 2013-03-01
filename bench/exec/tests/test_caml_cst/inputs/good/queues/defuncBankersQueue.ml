(* This implementation of banker's queues is derived from Okasaki's
   original implementation (see [BankersQueue]) through the following
   steps:

   1. inline definitions and code for streams;

   2. since [++] and [rev] are always used in combination, define a
   combined version of these functions, also called [++]; this allows
   eliminating a force/lazy redex;

   3. expose the fact that a thunk is just a reference to either a
   suspended computation or a cell; note that there are two cell
   constructors, namely [Nil] and [Cons]; examine all applications of
   the [lazy] keyword to non-values; there is just one, within the
   definition of [++], with two free variables [xs] and [ys];
   defunctionalize; this yields one computation constructor,
   [AppendRev]; now, flatten these sums, yielding a type [ccell] with
   three summands; define a defunctionalized [force] function for this
   type, where the [AppendRev] case corresponds to the previous body
   of [++]; the body of [++] is replaced with an application of [ref
   . AppendRev];

*)

type 'a stream =
  'a ccell ref

and 'a ccell =
  | Nil
  | Cons of 'a * 'a stream
  | AppendRev of 'a stream * 'a list

type 'a queue = {
  lenf: int;
  f: 'a stream;
  lenr: int;
  r: 'a list;
}

(* Stream construction. *)

(* TEMPORARY how to work around the value restriction? Here, it would be
   safe to generalize, because a reference to Nil is never written. In
   the meantime, I am forced to introduce a dummy lambda abstraction. *)

let nil () : 'a stream =
  ref Nil

let cons (x : 'a) (xs : 'a stream) : 'a stream =
  ref (Cons (x, xs))

let (++) (xs : 'a stream) (ys : 'a list) : 'a stream =
  ref (AppendRev (xs, ys))

(* Reversal. *)

(* [rev r f] is the reverse of [r] concatenated with [f]. Its cost
   (which is paid up front) is linear in the size of [r]. *)

let rec rev (r : 'a list) (f : 'a ccell) : 'a ccell =
  match r with
  | [] ->
      f
  | x :: r ->
      rev r (Cons (x, ref f))

(* Forcing. *)

let rec force (xs : 'a stream) : 'a ccell =
  let cc = !xs in
  match cc with
  | Nil
  | Cons _ ->
      cc
  | AppendRev (xs, ys) ->
      let cc =
	match force xs with
	| Cons (x, xs) ->
	    Cons (x, xs ++ ys)
	| Nil ->
	    rev ys Nil (* this never returns [AppendRev] *)
	| AppendRev _ ->
	    assert false (* [force] never returns [AppendRev] *)
      in
      xs := cc;
      cc

(* Destruction. *)

let extract xs =
  match force xs with
  | Nil ->
      raise Not_found
  | Cons (x, xs) ->
      x, xs
  | AppendRev _ ->
      assert false (* [force] never returns [AppendRev] *)

(* Construction. *)

let empty () : 'a queue = {
  lenf = 0;
  f = nil();
  lenr = 0;
  r = [];
}

(* Insertion at the left end. *)

let cons x q = { q with
  lenf = q.lenf + 1;
  f = cons x q.f;
}

(* Re-balancing. *)

let check q =
  if q.lenf >= q.lenr then
    q
  else {
    lenf = q.lenf + q.lenr;
    f = q.f ++ q.r;
    lenr = 0;
    r = [];
  }

(* Insertion at the right end. *)

let snoc q x = check { q with
  lenr = q.lenr + 1;
  r = x :: q.r;
}

(* Test for emptiness. *)

let is_empty q =
  q.lenf = 0

(* Removal at the left end. *)

let extract q =
  let x, f = extract q.f in
  x,
  check { q with
    f = f;
    lenf = q.lenf - 1;
  }

