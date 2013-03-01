(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/gromit/ordering.ml,v 1.2 2000/02/11 16:15:36 fpottier Exp $ *)

(*i-----------------------------------------------------------------------------------------------------------------i*)
(*s \mysection{Orderings} *)

(* A binary relation over a set of $n$ elements is represented as a $n\times n$ square Boolean matrix. Elements are
   numbered from $0$ to $n-1$. An ordering is a reflexive, transitive, anti-symmetric relation. *)

type ordering = {
    ordering_n : int;
    ordering_matrix : bool array array
  } 

(* The function [ordering] expects an arbitrary binary relation, and computes the smallest ordering containing it, if
   there is one.  The computation is performed in place. An exception of the form [Cycle(i, j)] is raised if the
   relation contains a cycle. Then, [i] and [j] are two distinct, unspecified elements of such a cycle. *)

exception Cycle of int * int

let ordering relation =
  let nm1 = relation.ordering_n - 1
  and m = relation.ordering_matrix in

  (* Mark each element as related with itself. *)

  for i = 0 to nm1 do
    m.(i).(i) <- true
  done;

  (* Apply Warshall's algorithm. Given a boolean matrix $A$, this algorithm computes $A^*$ as the ``limit'' of a series
     of matrices $A(-1) = I\vee A, A(0), A(1), \ldots$ with $a_{ij}(s)=1$ if and only if there is a directed path from
     $i$ to $j$ passing through intermediate nodes from $\{ 0, \ldots, s \}$ only. $A(s)$ can be computed in $O(n^2)$
     time from $A(s-1)$ by observing that
     $$a_{ij}(s) = a_{ij}(s-1) \vee a_{is}(s-1)a_{sj}(s-1)$$
     As $A^*=A(n-1)$, Warshall's algorithm computes transitive closures in $O(n^3)$ time. *)

  for s = 0 to nm1 do
    for i = 0 to nm1 do
      for j = 0 to nm1 do
	if m.(i).(s) & m.(s).(j) then
	  m.(i).(j) <- true
      done
    done
  done;

  (* At this point, the binary relation described by our matrix is necessarily reflexive and transitive. There remains
     to make sure that it is also anti-symmetric, i.e. that no cycle exists. This is done in a very straightforward
     way, in time $O(n^2)$. *)

  for i = 0 to nm1 do
    for j = 0 to nm1 do
      if (i <> j) & m.(i).(j) & m.(j).(i) then
	raise (Cycle(i, j))
    done
  done

(*i-----------------------------------------------------------------------------------------------------------------i*)
(*s \mysection{Lattices} *)

(* A lattice is an ordering which verifies a few additional properties. It must have a least element $\bot$ and a
   greatest element $\top$. Furthermore, any two elements $i$ and $j$ must have a least upper bound $i\sqcup j$ and
   a greatest lower bound $i\sqcap j$. *)

type lattice = {
    lattice_n : int;
    lattice_matrix : bool array array;
    lattice_bot : int;
    lattice_top : int;
    lattice_lub : int array array;
    lattice_glb : int array array
  } 

(*i-----------------------------------------------------------------------------------------------------------------i*)
(*s \mysubsection{A data structure for quick column lookup} *)

(* As part of the process of making sure that the ordering is a lattice, we shall need to quickly determine
   whether a given column appears in the matrix, and if so, at which index. This may be done in time
   $O(n)$, provided an appropriate structure is built beforehand. (This algorithm was suggested by Alexandre
   Frey.)

   We shall need a datatype for trees with binary nodes, leaves carrying an integer index, and holes. It is
   defined as follows: *)

type tree =
  | Node of tree * tree
  | Leaf of int
  | Hole

(* The set of all columns which appear in the matrix can be described as a tree, with the following property:
   a column whose contents is $b_0b_1\ldots b_{n-1}$ (i.e. a given sequence of $n$ booleans) appears in the matrix
   at index $j$ if and only if following the path $b_0b_1\ldots b_{n-1}$ in the tree leads to a leaf tagged $j$.
   Such a tree takes space $O(n^2)$, and may be built in time $O(n^2)$. *)

(*i-----------------------------------------------------------------------------------------------------------------i*)
(*s \mysubsection{Building the data structure} *)

(* The auxiliary function [build_tree] accepts an ordering and builds the tree described above. *)

let build_tree ordering =
  let nm1 = ordering.ordering_n - 1
  and m = ordering.ordering_matrix in

  (* The auxiliary function [insert_column] accepts a tree which supposedly already describes a number of columns,
     and adds a description of column [j] to it. *)

  let insert_column j tree =

    (* The auxiliary function [insert_partial_column] accepts a tree, and adds a description of the partial column
       obtained by starting at $(i, j)$ and moving down. *)

    let rec insert_partial_column i tree =

      (* If we have reached the bottom of the column, then we produce a leaf carrying the integer [j]. The old tree
	 is disregarded; it must have been a hole anyway, because if it was a leaf, then the matrix would contain two
	 identical columns. This is impossible if the matrix describes an ordering. *)
      
      if i > nm1 then Leaf (j)

      (* Otherwise, we look at the current element in the column, i.e. $(i, j)$. Its value tells whether we should
	 move left or right within the tree. (Recall that the contents of the column shall be read as a path into
	 the final tree.) *)

      else if m.(i).(j) then begin

	(* This bit of the path is $1$, so we should move down to the current node's right son. If the current node
	   is a hole, we proceed as if it were a binary node with two holes as its sons. It cannot be a leaf,
	   because all leaves are at depth $n$ in the tree. *)
	
	match tree with
	| Node (left, right) ->
	    Node (left, insert_partial_column (i + 1) right)
	| Leaf _ ->
	    assert false
	| Hole ->
	    Node (Hole, insert_partial_column (i + 1) Hole)

      end
      else begin

	match tree with
	| Node (left, right) ->
	    Node (insert_partial_column (i + 1) left, right)
	| Leaf _ ->
	    assert false
	| Hole ->
	    Node (insert_partial_column (i + 1) Hole, Hole)

      end in

    insert_partial_column 0 tree in

  (* The whole tree is obtained by applying [insert_column] to each column, in turn, starting with an empty tree. *)

  let rec iterate j tree =
    if j > nm1 then tree
    else iterate (j + 1) (insert_column j tree) in

  iterate 0 Hole

(*i-----------------------------------------------------------------------------------------------------------------i*)
(*s \mysubsection{Checking whether greatest lower bounds exist} *)

(* The auxiliary function [build_glb] accepts an ordering and makes sure that any two elements have a greatest lower
   bound. It returns a $n\times n$ square integer matrix which describes $\sqcap$ extensively. The parameter [failure]
   is a function of two integer arguments, which is invoked if two elements do not have a greatest lower bound. *)

let build_glb failure ordering =
  let n = ordering.ordering_n
  and m = ordering.ordering_matrix in
  let nm1 = n - 1 in

  (* First, build the tree described above. *)

  let tree = build_tree ordering in

  (* We are now ready to make sure that $\sqcap$ is defined. For each pair of elements [j1] and [j2], the element
     $[j1]\sqcap [j2]$, if it exists, is characterized by the fact that its column is exactly
     the product (i.e. the set intersection) of the $[j1]^{\text{th}}$ and $[j2]^{\text{th}}$ columns. For fixed [j1]
     and [j2], we may compute this product in time $O(n)$, and check whether it appears in the tree at the same
     time. So, checking that $\sqcap$ is defined, and computing it extensively, takes time $O(n^3)$. *)

  let glb = Array.make_matrix n n (-1) in

  for j1 = 0 to nm1 do
    glb.(j1).(j1) <- j1;
    for j2 = j1 + 1 to nm1 do

      (* We must compute the product of the two columns, and look for it in the tree. The search consists
	 of a loop, where [i] ranges from $0$ to $n-1$. We do not check for the condition $i=n$, because the search
	 shall necessarily end when we reach a leaf or a hole.

	 If the search reaches a leaf, then the column we were looking for exists, and the leaf carries its number.
	 If the search reaches a hole, then the column does not exist, which means that [j1] and [j2] do not have
	 a greatest lower bound. If the search reaches a node, then necessarily $i<n$, and we go down to the left
	 or to the right, according to the $i^{\text{th}}$ element of the columns' product. *)

      let rec look i = function
	| Leaf j ->
	    glb.(j1).(j2) <- j;
	    glb.(j2).(j1) <- j
	| Hole ->
	    failure j1 j2
	| Node (left, right) ->
	    look (i + 1) (if m.(i).(j1) & m.(i).(j2) then right else left) in

      look 0 tree

    done
  done;

  glb

(*i-----------------------------------------------------------------------------------------------------------------i*)
(*s \mysubsection{Finding the least element} *)

(* The auxiliary function [bottom] accepts a non-empty ordering whose $\sqcap$ function is defined, and computes its
   least element. It works in time $O(n)$, by computing the greatest lower bound of all elements of the set. This
   element is defined, and is a least element; it is necessarily unique, since an ordering is anti-symmetric. *)

let bottom glb =
  let n = Array.length glb in
  let rec loop i accu =
    if i = n then accu
    else loop (i + 1) glb.(i).(accu) in
  loop 1 0

(*i-----------------------------------------------------------------------------------------------------------------i*)
(*s \mysubsection{Reversing a partial order} *)

(* The auxiliary function [transpose] transposes a square matrix, whose dimension is at least $1$. *)

let transpose matrix =
  let n = Array.length matrix in
  let nm1 = n - 1 in
  let matrix' = Array.make_matrix n n matrix.(0).(0) in
  for i = 0 to nm1 do
    for j = 0 to nm1 do
      matrix'.(i).(j) <- matrix.(j).(i)
    done
  done;
  matrix'

(* The auxiliary function [reverse] reverses a non-empty ordering. *)

let reverse ordering =
  {
    ordering_n = ordering.ordering_n;
    ordering_matrix = transpose ordering.ordering_matrix
  } 

(*i-----------------------------------------------------------------------------------------------------------------i*)
(*s \mysubsection{Putting it all together} *)

(* The function [lattice] expects an ordering, makes sure it is a lattice, and returns an appropriate [lattice]
   structure describing it. The following exceptions may be raised if the ordering isn't a lattice:
   \begin{itemize}
   \item [Empty]: the ordering has no elements;
   \item [NoLUB(i, j)]: $i\sqcup j$ is undefined;
   \item [NoGLB(i, j)]: $i\sqcap j$ is undefined.
   \end{itemize} *)

exception Empty
exception NoLUB of int * int
exception NoGLB of int * int

let lattice ordering =
  let n = ordering.ordering_n in

  (* First, we check whether the ordering is non-empty. *)

  if n < 1 then
    raise Empty;

  (* Then, we compute $\sqcap$ and $\bot$. *)

  let glb = build_glb (fun j1 j2 -> raise (NoGLB(j1, j2))) ordering in
  let bot = bottom glb in

  (* Then, we transpose the ordering's matrix, so as to work on the reverse ordering, and repeat the same steps.
     Transposing the matrix incurs a cost of $O(n^2)$, but has the advantage of avoiding code duplication. *)

  let lub = build_glb (fun j1 j2 -> raise (NoLUB(j1, j2))) (reverse ordering) in
  let top = bottom lub in

  (* At this point, the ordering is known to be a lattice. There only remains to build the associated structure. *)

  {
    lattice_n = n;
    lattice_matrix = ordering.ordering_matrix;
    lattice_bot = bot;
    lattice_top = top;
    lattice_lub = lub;
    lattice_glb = glb
  } 

