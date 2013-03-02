(* This module provides a very naive, list-based implementation of
   dictionaries. *)

open Sigs

module Make (Goal : GOAL) = struct

  module Goal = Goal
  open Goal

  type dictionary =
      goal list

  let empty =
    []

  let add g d =
    g :: d

  module Index =
    Word.Left

  (* Naive check for an occurrence of the [i]-suffix of the pattern
     [p] at offset [j] within the word [w]. The check either fails or
     returns the end index. *)

  let rec check p w i j =
    try
      let i' = Index.next p i in
      try
	let j' = Index.next w j in
	if Word.Symbol.equal (Index.get p i) (Index.get w j) then
	  check p w i' j'
	else
	  raise Not_found (* failure: two symbols differ *)
      with Index.AtBoundary ->
	raise Not_found (* failure: pattern is non-empty, word is empty *)
    with Index.AtBoundary ->
      j (* success: pattern is empty *)

  (* Naive check for an occurrence of the pattern [p] at offset [j]
     within the word [w]. The check either fails and returns an
     unchanged accumulator, or succeeds and prepends a pair of the
     start and end indices where the pattern was found to the
     accumulator. *)

  let check p w j accu =
    try
      (j, check p w (Index.start p) j) :: accu
    with Not_found ->
      accu

  (* Naive search for all occurrences of the pattern [p] within the
     word [w]. *)

  let rec search p w j =
    try
      let j' = Index.next w j in
      check p w j (search p w j')
    with Index.AtBoundary ->
      check p w j []

  let search p w =
    search p w (Index.start w)

  (* Naive search for all occurrences of left-hand sides of all
     rules in the dictionary [d] within the word [w]. *)

  let lookup (d : dictionary) (w : Word.word) =
    List.fold_left (fun accu rule ->
      let lhs = Normal.left rule
      and rhs = Normal.right rule in
      let occurrences = search lhs w in
      List.fold_right (fun (j, j') accu ->
	(rule, Index.prefix w j, rhs, Index.suffix w j') :: accu
      ) occurrences accu
    ) [] d

end
