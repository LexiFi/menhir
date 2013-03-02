(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/markedList.ml,v 1.2 2000/10/23 09:32:04 fpottier Exp $ *)

(* This module provides linear-time operations on (unsorted) lists. It requires the ability to physically mark
   list elements.

   Every operation provided by the module implicitly assumes that no elements are initially marked, and uses
   marks internally. *)

module type S = sig

  type element
  type t = element list

  (* [clean list] returns [list] deprived of any (physical) duplicates. *)

  val clean: t -> t

  (* [diff list1 list2] returns [list1] deprived from any elements which (physically) occur in [list2].
     The result list contains no (physical) duplicates. *)

  val diff: t -> t -> t

  (* [concat list1 list2] concatenates [list1] in front of [list2], and ensures that the result list
     contains no (physical) duplicates. *)

  val concat: t -> t -> t

  (* [concat_diff list1 list2 list3] concatenates [list1] in front of [list2], removes any elements
     which (physically) occur in [list3], and ensures that the result list contains no (physical) duplicates. *)

  val concat_diff: t -> t -> t -> t

  (* [sub list1 list2] tells whether every element of [list1] is (physically) present in [list2]. *)

  val sub: t -> t -> bool

end

module Make (X : sig

  type t

  (* [mark x] marks the element [x]. [x] may be marked already; in that case, [mark] has no effect. *)

  val mark: t -> unit

  (* [unmark x] unmarks the element [x]. [x] may be unmarked already; in that case, [unmark] has no effect. *)

  val unmark: t -> unit
      
  (* [marked x] tells whether the element [x] is marked. *)
      
  val marked: t -> bool

end) = struct

  type element = X.t
  type t = element list

  (* [mark_and_sweep root list1] walks [list1], marking elements as it goes. Then, it computes [root()], which must
     yield a list of unmarked elements. (That is, any elements of which happen to be marked, possibly because they
     occur in [list1], must be unmarked if returned by [root].) Then, it walks back [list1], appending any unmarked
     elements to the result list, and unmarking elements as it goes. At the end of the day, all elements of [list1]
     are unmarked.

     In the simplest case, all elements of [list1] are initially unmarked, and unmarked again when the function
     returns. However, more complex cases are possible (see [concat] below). *)

  let mark_and_sweep root list1 =
    let rec walk = function
      |	[] ->

	  (* We have reached the end of [list1]. Compute some list. Any elements which appear in [list1] are currently
	     marked. Any elements placed by [root] in the result list must be unmarked in the process, so that we will
	     not add them redundantly when walking back [list1]. *)

	  root()

      |	(elem1 :: rest1) as list1 ->

	  (* Mark the element. Notice that it may be marked already if it appears twice in [list1]. *)

	  X.mark elem1;
	  
	  (* Go on walking [list1]. *)

	  let rest1' = walk rest1 in

	  (* We're back and building a new list. Check whether this element is marked. If it is, then this is
	     the first time we encounter it on our way back, so we unmark it and add it to the result list. Otherwise,
	     we simply drop it. *)

	  if X.marked elem1 then begin
	    X.unmark elem1;

	    (* This physical equality check allows avoiding allocation whenever the result list turns out to be
	       equal to the input list. *)

	    if rest1' == rest1 then
	      list1
	    else
	      elem1 :: rest1'
	  end
	  else
	    rest1 in

    walk list1

  (* [clean list] returns [list] deprived of any (physical) duplicates. 

     It would be possible to define [clean list] as [diff list []], but this definition is slightly more
     efficient. *)

  let clean =
    mark_and_sweep (function () -> [])

  (* [diff list1 list2] returns [list1] deprived from any elements which (physically) occur in [list2].
     The result list contains no (physical) duplicates.

     The [root] computation consists in unmarking all of [list2]'s elements, then returning the empty
     list. Thus, the elements which remain marked after the transform are exactly those which occur in
     [list1], but not in [list2]. They will be added to this empty list, so the end result will be the
     difference [list1] minus [list2], as desired. *)

  let diff list1 list2 =
    mark_and_sweep (function () ->
      List.iter X.unmark list2;
      []
    ) list1

  (* [concat list1 list2] concatenates [list1] in front of [list2], and ensures that the result list
     contains no (physical) duplicates. *)

  let concat list1 list2 =
    mark_and_sweep (function () ->
      clean list2
    ) list1

  (* [concat_diff list1 list2 list3] concatenates [list1] in front of [list2], removes any elements
     which (physically) occur in [list3], and ensures that the result list contains no (physical) duplicates. *)

  let concat_diff list1 list2 list3 =
    mark_and_sweep (function () ->
      diff list2 list3
    ) list1

  (* [sub list1 list2] tells whether every element of [list1] is (physically) present in [list2]. *)

  let sub list2 list1 =
    let rec walk = function
      |	[] ->

	  (* We have reached the end of [list1]. Any elements which appear in [list1] are currently marked. Check
	     whether all elements of [list2] are marked as well. *)

	  List.for_all X.marked list2

      |	elem1 :: rest1 ->

	  (* Mark the element. Notice that it may be marked already if it appears twice in [list1]. *)

	  X.mark elem1;
	  
	  (* Go on walking [list1]. *)

	  let result = walk rest1 in

	  (* Unmark [list1] on our way back. *)

	  X.unmark elem1;
	  result in

    walk list1

end

