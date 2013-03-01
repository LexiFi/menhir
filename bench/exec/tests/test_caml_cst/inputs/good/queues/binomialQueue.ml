(* $Header: /home/yquem/cristal/fpottier/cvs/queues/binomialQueue.ml,v 1.5 2005/12/15 12:53:50 fpottier Exp $ *)

(* This module implements (purely functional) binomial queues. *)

(* The module is parameterized by the type of the elements. Elements must
   be equipped with an ordering that reflects their priority. *)

module Make (X : sig

  type t
  val compare: t -> t -> int

end) = struct

  type element = X.t

  (* A full binomial forest is encoded as a complete binary tree, where left
     descendants represent sons and right descendants represent siblings. *)

  type tree =
    | TEmpty
    | TNode of element * tree * tree

  (* A binomial tree is encoded as a topped binary tree, that is, a
     pair of a root element and the binomial forest of its
     sons. Binomial trees are carried by 1 digits (see below). *)

  type topped =
    | Topped of element * tree

  (* A nonempty binomial queue is encoded as a sequence of digits,
     with least significant digits near the head of the sequence. The
     digits are exactly those found in the binary decomposition of the
     positive number [n], where [n] is the number of elements in the
     queue. The leading digit (the one at the tail of the list) is
     always one. A zero digit holds no information. A one digit holds
     a binomial tree. *)

  type number =
    | Skip of number          (* a 0 digit, followed with more digits *)
    | Cons of topped * number (* a 1 digit, carrying a topped binomial tree, followed with more digits *)
    | One of topped           (* the trailing 1 digit, carrying a topped binomial tree *)

  (* A queue either is empty or holds a positive number of elements. *)

  type t =
    | QZero
    | QPositive of number

  (* [empty] is the empty queue, i.e. the number 0. *)

  let empty =
    QZero

  (* [singleton x] is a queue containing only the element [x]. *)

  let singleton x =
    QPositive (One (Topped (x, TEmpty)))

  (* Combinining two binomial trees to form a new binomial tree twice
     their size. The smaller of the two roots becomes the new root. *)

  let carry (d1 : topped) (d2 : topped) : topped =
    let Topped (root1, sons1), Topped (root2, sons2) = d1, d2 in
    if X.compare root1 root2 < 0 then
      Topped (root1, TNode (root2, sons2, sons1))
    else
      Topped (root2, TNode (root1, sons1, sons2))

  (* Incrementing a positive number. *)

  let rec increment (d1 : topped) (n2 : number) =
    match n2 with
    | Skip n2 ->
	Cons (d1, n2)
    | Cons (d2, n2) ->
	Skip (increment (carry d1 d2) n2)
    | One d2 ->
	Skip (One (carry d1 d2))

  (* [insert x q] is the queue obtained by inserting the element [x]
     into the queue [q]. It runs in time $O(\log n)$, where $n$ is the
     size of the queue [q], and in constant amortized time. *)

  let insert x q =
    match q with
    | QZero ->
	singleton x
    | QPositive n ->
	QPositive (increment (Topped (x, TEmpty)) n)

  (* Adding two positive numbers, without and with a carry. *)

  let rec add (n1 : number) (n2 : number) =
    match n1, n2 with
    | Skip n1, Skip n2 ->
	Skip (add n1 n2)
    | Skip n1, Cons (d2, n2)
    | Cons (d2, n2), Skip n1 ->
	Cons (d2, add n1 n2)
    | Skip n1, One d2
    | One d2, Skip n1 ->
	Cons (d2, n1)
    | Cons (d1, n1), Cons (d2, n2) ->
	Skip (addc (carry d1 d2) n1 n2)
    | Cons (d1, n1), One d2
    | One d2, Cons (d1, n1) ->
	Skip (increment (carry d1 d2) n1)
    | One d1, One d2 ->
	Skip (One (carry d1 d2))

  and addc (d : topped) (n1 : number) (n2 : number) =
    match n1, n2 with
    | Skip n1, Skip n2 ->
	Cons (d, add n1 n2)
    | Skip n1, Cons (d2, n2)
    | Cons (d2, n2), Skip n1 ->
	Skip (addc (carry d d2) n1 n2)
    | Skip n1, One d2
    | One d2, Skip n1 ->
	Skip (increment (carry d d2) n1)
    | Cons (d1, n1), Cons (d2, n2) ->
	Cons (d, addc (carry d1 d2) n1 n2)
    | Cons (d1, n1), One d2
    | One d2, Cons (d1, n1) ->
	Cons (d, increment (carry d1 d2) n1)
    | One d1, One d2 ->
	Cons (d, One (carry d1 d2))

  (* [union q1 q2] is the union of the queues [q1] and [q2]. It runs
     in time $O(\log(n_1+n_2))$, where $n_1$ and $n_2$ are the sizes
     of the queues [q1] and [q2]. *)

  let union q1 q2 =
    match q1, q2 with
    | QZero, _ ->
	q2
    | _, QZero ->
	q1
    | QPositive n1, QPositive n2 ->
	QPositive (add n1 n2)

  (* [skip] prepends a 0 digit in front of a possibly null number. *)

  let skip (q : t) =
    match q with
    | QZero ->
	QZero
    | QPositive n ->
	QPositive (Skip n)

  (* [cons] prepends a 1 digit in front of a possibly null number. *)

  let cons (d : topped) (q : t) =
    match q with
    | QZero ->
	QPositive (One d)
    | QPositive n ->
	QPositive (Cons (d, n))

  (* [extractMin n] finds which 1 digit within the positive number
     [n] holds the minimum element. It returns a pair of that digit
     and of the number [n] where that digit has been replaced with
     a 0 digit (so the new number is possibly null). *)

  let rec extractMin (n : number) : topped * t =
    match n with
    | One d ->
	d, QZero
    | Skip n ->
	let d, q = extractMin n in
	d, skip q
    | Cons (d1, n) ->
	let d2, q = extractMin n in
	let Topped (root1, _) = d1
	and Topped (root2, _) = d2 in
	if X.compare root1 root2 < 0 then
	  d1, QPositive (Skip n)
	else
	  d2, cons d1 q

  (* [dismantle q f] prepends the full binomial forest [f] (encoded as
     a complete binary tree) in front of the binomial queue [q]. *)

  let rec dismantle (q : t) = function
    | TEmpty ->
	q
    | TNode (root, sons, siblings) ->
	dismantle (cons (Topped (root, sons)) q) siblings

  (* [extract q] raises [Not_found] if the queue [q] is
     empty. Otherwise, it extracts a minimal element out of the queue,
     and returns both that element and the queue formed by the
     remaining elements. Its time complexity is $O(\log n)$, where $n$
     is the size of the queue [q]. *)

  let extract = function
    | QZero ->
	raise Not_found
    | QPositive n ->
	let Topped (root, sons), q = extractMin n in
	root, union (dismantle QZero sons) q

  (* [elements q] returns a sorted list of all elements in [q]. Its
     time complexity is $O(n\log n)$, where $n$ is the size of the
     queue [q]. *)

  let rec elements q =
    try
      let x, q = extract q in
      x :: elements q
    with Not_found ->
      []

end
