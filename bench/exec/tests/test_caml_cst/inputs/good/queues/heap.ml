(* $Header: /home/yquem/cristal/fpottier/cvs/queues/heap.ml,v 1.2 2003/04/18 14:31:09 fpottier Exp $ *)

(* This module implements heaps, as suggested J. W. J. Williams in
   Communications of the ACM 7(6), June 1964. *)

(* A heap is a (resizable) array of elements. Elements must be equipped with
   a total order. Although they are arranged in an array, they form a
   logical tree, with the convention that every node is no greater than its
   sons. *)

type 'a t = {
    o: 'a -> 'a -> int;
    a: 'a ResizableArray.t
  } 

(* The index of the tree's root. *)

let root = 0

(* The index of a node's father. *)

let father i =
  (i + 1) / 2 - 1

(* The index of a node's left son. *)

let left i =
  2 * i + 1

(* The index of a node's right sibling. *)

let right i =
  i + 1

(* [make o x] creates a new heap, whose elements are ordered by [o]. The
   element [x] is a dummy value used to initialize the array, and will never
   be compared with actual elements of the heap. *)

let make o x = {
  o = o;
  a = ResizableArray.make x
} 

(* [size h] returns the number of elements in the heap [h]. It is a constant
   time operation. *)

let size h =
  ResizableArray.size h.a

(* [swop h x] inserts the element [x] into the heap [h], then extracts and
   returns the least element. Thus, the heap size does not change. The time
   complexity is $O(\log n)$, where $n$ is the heap's size.

   It is a top to bottom procedure. That is, by extracting the top element,
   we create a hole at the root of the tree. We then push the hole down
   until it becomes a suitable position for inserting [x]. *)

let swop h x =
  let a = h.a in
  let n = ResizableArray.size a in

  (* If the heap is empty, then it remains empty and the result is [x]. *)

  if n = 0 then
    x
  else

  (* If [x] is no greater than the heap's current minimum element, then the
     heap is unaffected and the result is [x]. *)

    let minimum = ResizableArray.get a root in
    if h.o x minimum <= 0 then
      x
    else

      (* Otherwise, the result is the heap's current minimum element.  We
	 remove it, creating a hole, and sift elements down until the hole
	 becomes a suitable position for inserting [x]. When [scan i] is
	 invoked, the hole is at index [i]. *)

      let rec scan i =
	let j = left i in

	(* In the most common case, there are two nodes below the hole.  We
	   first determine which of the two is smaller; we call its index
	   [k] and its value [ak]. Then, we compare it with [x].  If it is
	   smaller than [x], then the hole is not a suitable position for
	   inserting [x], so we swap the hole with [ak], and
	   continue. Otherwise, we insert [x] into the current hole. *)

	if j < n-1 then begin
	  let j' = right j in
	  let aj = ResizableArray.get a j
	  and aj' = ResizableArray.get a j' in
	  let k, ak = if h.o aj aj' < 0 then j, aj else j', aj' in
	  if h.o ak x < 0 then begin
	    ResizableArray.set a i ak;
	    scan k
	  end
	  else
	    ResizableArray.set a i x
	end

	(* There is a special case where there is only one node below the
	   hole. The principle remains the same as above. *)

	else if j = n-1 then begin
	  let aj = ResizableArray.get a j in
	  if h.o aj x < 0 then begin
	    ResizableArray.set a i aj;
	    ResizableArray.set a j x
	  end
	  else
	    ResizableArray.set a i x
	end

	(* If there are no nodes below the hole, then it is a suitable
	   position for inserting [x]. *)

	else (* $j\geq n$ *)
	  ResizableArray.set a i x

      in
      scan root;
      minimum

(* [insert h x] inserts the element [x] into the heap [h]. Its time
   complexity is $O(\log n)$, where $n$ is the heap's size.

   It is a bottom to top procedure. That is, we first insert [x] at the
   fringe of the tree. Then, we restore the heap condition by pushing [x]
   down as far as necessary. *)

let insert h x =
  let a = h.a in
  let n = ResizableArray.size a in
  ResizableArray.resize a (n + 1);

  (* When [scan i] is invoked, the hole is at index [i] and all elements
     below the hole are greater than [x]. *)

  let rec scan i =

    (* If the hole is now at the root, it is a suitable position
       for inserting [x]. *)

    if i = root then
      ResizableArray.set a i x

    (* Otherwise, we look at the node above the hole, whose index is [j] and
       value is [aj]. If [x] is smaller than it, then the current hole is
       not a suitable position for inserting [x], so we swap the hole with
       [aj], and continue. Otherwise, we insert [x] into the current
       hole. *)

    else
      let j = father i in
      let aj = ResizableArray.get a j in
      if h.o x aj < 0 then begin
	ResizableArray.set a i aj;
	scan j
      end
      else
	ResizableArray.set a i x

  in
  scan n

(* [extractmin h] extracts and returns the minimum element of the heap
   [h]. It raises [Not_found] if the heap is empty. Its time
   complexity is $O(\log n)$, where $n$ is the heap's size. *)

let extractmin h =
  let a = h.a in
  let n = ResizableArray.size a in
  if n = 0 then
    raise Not_found
  else
    let last = ResizableArray.get a (n - 1) in
    ResizableArray.resize a (n - 1);
    swop h last

(* [heapsort o l] sorts the list [l] according to the total ordering
   [o]. Its time complexity is $O(n\log n)$. *)

let heapsort o = function
  | [] ->
      []
  | (x :: _) as l ->
      let h = make (fun x1 x2 -> -(o x1 x2)) x in
      List.iter (insert h) l;
      let rec build l n =
	if n = 0 then l else build (extractmin h :: l) (n - 1) in
      build [] (size h)

(* Note that it is possible to turn an array into a heap in linear
   time, using Floyd's construction (a bottom-up construction). The
   code above does not exploit this fact. Also, when sorting an array,
   it is possible to allocate the heap inside the array, so no extra
   storage is required -- this is what one usually calls heapsort.
   Using Floyd's construction, it is also possible to merge two heaps
   in linear time. *)

