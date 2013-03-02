open Stream

(* A data structure for queues, due to Chris Okasaki. All operations
   require worst-case constant time, except [extract], which requires
   amortized constant time. *)

(* The data structure has the following invariants: [lenf] is the length
   of [f]; [lenr] is the length of [r]; [lenf] is greater than or equal
   to [lenr]. That is, there are never too many elements in [r]. *)

type 'a queue = {
  lenf: int;
  f: 'a stream;
  lenr: int;
  r: 'a list;
}

(* Construction. *)

let empty : 'a queue = {
  lenf = 0;
  f = nil;
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
    f = q.f ++ rev q.r;
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

