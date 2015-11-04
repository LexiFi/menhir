open Keyword
open UnparameterizedSyntax
open ListMonad

(* This exception will be raised when a branch does not need inlining. *)
exception NoInlining

(* Color are used to detect cycles. *)
type 'a color = 
  | BeingExpanded
  | Expanded of 'a

(* [index2id] converts a 0-based index (into a list of producers) to
   an identifier (the name of the producer). *)

let index2id producers i =
  try
    let (_, x) = List.nth producers i in
    x
  with Failure _ ->
    assert false (* should not happen *)

(* [rename_sw_outer] transforms the keywords in the outer production (the
   caller) during inlining. It looks for [$startpos(x)] and [$endpos(x)], where
   [x] is the name of the callee, and replaces them with [startp] and [endp],
   respectively. *)

(* It does not modify [$startpos] or [$endpos], of course, nor [$startpos(y)]
   and [$endpos(y)] for some other [y]. It does not modify [$endpos($0)]. *)

let rename_sw_outer (x, startp, endp) (subject, where) : (subject * where) option =
  match subject with
  | Before
  | Left ->
      None
  | RightNamed x' ->
      if x' = x then
        match where with
        | WhereStart -> Some startp
        | WhereEnd   -> Some endp
      else
        None

(* [rename_sw_inner] transforms the keywords in the inner production (the callee)
   during inlining. It looks for [$endpos($0)], [$startpos], and [$endpos], and
   replaces them with [beforeendp], [startp], and [endp], respectively. *)

(* It does not modify any [$startpos(x)], of course. *)

let rename_sw_inner (beforeendp, startp, endp) (subject, where) : (subject * where) option =
  match subject, where with
  | Before, _ ->
      assert (where = WhereEnd);
      Some beforeendp
  | Left, WhereStart ->
      Some startp
  | Left, WhereEnd ->
      Some endp
  | RightNamed _, _ ->
      None

(* Inline a grammar. The resulting grammar does not contain any definitions
   that can be inlined. *)
let inline grammar = 

  let names producers = 
    List.fold_left (fun s (_, x) -> StringSet.add x s) 
      StringSet.empty producers 
  in

  (* This function returns a fresh name beginning with [prefix] and 
     that is not in the set of names [names]. *)
  let rec fresh ?(c=0) names prefix =
    let name = prefix^string_of_int c in
      if StringSet.mem name names then
	fresh ~c:(c+1) names prefix
      else 
	name
  in

  let use_inline = ref false in

  (* This table associates a color to each non terminal that can be expanded. *)
  let expanded_non_terminals = 
    Hashtbl.create 13 
  in

  let expanded_state k = 
    Hashtbl.find expanded_non_terminals k 
  in
      
  let mark_as_being_expanded k = 
    Hashtbl.add expanded_non_terminals k BeingExpanded
  in

  let mark_as_expanded k r =
    Hashtbl.replace expanded_non_terminals  k (Expanded r);
    r
  in

  (* This function traverses the producers of the branch [b] and find
     the first non terminal that can be inlined. If it finds one, it
     inlines its branches into [b], that's why this function can return
     several branches. If it does not find one non terminal to be 
     inlined, it raises [NoInlining]. *)
  let rec find_inline_producer b = 
    let prefix, nt, p, psym, suffix = 
      let rec chop_inline i (prefix, suffix) =
	match suffix with
	  | [] -> 
	      raise NoInlining

	  | ((nt, id) as x) :: xs ->
	      try
		let r = StringMap.find nt grammar.rules in
		if r.inline_flag then 
		    (* We have to inline the rule [r] into [b] between
		       [prefix] and [xs]. *)
		  List.rev prefix, nt, r, id, xs
		else 
		  chop_inline (i + 1) (x :: prefix, xs) 
	      with Not_found -> 
		chop_inline (i + 1) (x :: prefix, xs) 
      in
	chop_inline 1 ([], b.producers)
    in
      prefix, expand_rule nt p, nt, psym, suffix

  (* We have to rename producers' names of the inlined production 
     if they clash with the producers' names of the branch into 
     which we do the inlining. *)
  and rename_if_necessary b producers =

    (* First we compute the set of names already in use. *)
    let producers_names = names (b.producers @ producers) in

    (* Compute a renaming and the new inlined producers' names. *)
    let phi, producers' =
      List.fold_left (fun (phi, producers) (p, x) -> 
	if StringSet.mem x producers_names then
	  let x' = fresh producers_names x in
	  ((x, x') :: phi, (p, x') :: producers)
	else 
	  (phi, (p, x) :: producers)
      ) ([], []) producers
    in
      phi, List.rev producers'
	
  (* Inline the non terminals that can be inlined in [b]. We use the 
     ListMonad to combine the results. *)
  and expand_branch (b : branch) : branch ListMonad.m =
    try
      (* [c] is the identifier under which the callee is known. *)
      let prefix, p, _nt, c, suffix = find_inline_producer b in
      use_inline := true;
      (* Inline a branch of [nt] at position [prefix] ... [suffix] in
	 the branch [b]. *)
      let inline_branch pb =
	(* Rename the producers of this branch is they conflict with
	   the name of the host's producers. *)
	let phi, inlined_producers = rename_if_necessary b pb.producers in

        (* After inlining, the producers are as follows. *)
        let producers = prefix @ inlined_producers @ suffix in
        let index2id = index2id producers in

        let prefix = List.length prefix
        and inlined_producers = List.length inlined_producers in

        (* Define how the start and end positions of the inner production should
           be computed once it is inlined into the outer production. These
           definitions of [startp] and [endp] are then used to transform
           [$startpos] and [$endpos] in the inner production and to transform
           [$startpos(x)] and [$endpos(x)] in the outer production. *)

        (* 2015/11/04. We ensure that positions are computed in the same manner,
           regardless of whether inlining is performed. *)

        let startp =
          if inlined_producers > 0 then
            (* If the inner production is non-epsilon, things are easy. The start
               position of the inner production is the start position of its first
               element. *)
            RightNamed (index2id prefix), WhereStart
          else if prefix > 0 then
            (* If the inner production is epsilon, we are supposed to compute the
               end position of whatever comes in front of it. If the prefix is
               nonempty, then this is the end position of the last symbol in the
               prefix. *)
            RightNamed (index2id (prefix - 1)), WhereEnd
          else
            (* If the inner production is epsilon and the prefix is empty, then
               we need to look up the end position stored in the top stack cell.
               This is the reason why we need the keyword [$endpos($0)]. It is
               required in this case to preserve the semantics of $startpos and
               $endpos. *)
            Before, WhereEnd

          (* Note that, to contrary to intuition perhaps, we do NOT have that
             if the prefix is empty, then the start position of the inner
             production is the start production of the outer production.
             This is true only if the inner production is non-epsilon. *)

	in

	let endp =
          if inlined_producers > 0 then
            (* If the inner production is non-epsilon, things are easy, then its end
               position is the end position of its last element. *)
            RightNamed (index2id (prefix + inlined_producers - 1)), WhereEnd
          else
            (* If the inner production is epsilon, then its end position is equal
               to its start position. *)
            startp

        in

        (* We must also transform [$endpos($0)] if it used by the inner
           production. It refers to the end position of the stack cell
           that comes before the inner production. So, if the prefix is
           non-empty, then it translates to the end position of the last
           element of the prefix. Otherwise, it translates to [$endpos($0)]. *)

        let beforeendp =
          if prefix > 0 then
            RightNamed (index2id (prefix - 1)), WhereEnd
          else
            Before, WhereEnd
        in

	(* Rename the outer and inner semantic action. *)
	let outer_action =
	  Action.rename (rename_sw_outer (c, startp, endp)) [] b.action
	and action' =
	  Action.rename (rename_sw_inner (beforeendp, startp, endp)) phi pb.action
	in

	{ b with
	  producers = producers;
	  action = Action.compose c action' outer_action
	}
      in
      List.map inline_branch p.branches >>= expand_branch

    with NoInlining ->
      return b

  (* Expand a rule if necessary. *)
  and expand_rule k r = 
    try 
      (match expanded_state k with
	 | BeingExpanded ->
    	     Error.error
	       r.positions
	       "there is a cycle in the definition of %s." k
	 | Expanded r ->
	     r)
    with Not_found ->
      mark_as_being_expanded k;
      mark_as_expanded k { r with branches = r.branches >>= expand_branch }
  in

  (* If we are in Coq mode, %inline is forbidden. *)
  let _ =
    if Settings.coq then
      StringMap.iter 
        (fun _ r -> 
	   if r.inline_flag then
             Error.error r.positions
               "%%inline is not supported by the Coq back-end.")
        grammar.rules
  in

    (* To expand a grammar, we expand all its rules and remove 
       the %inline rules. *)
  let expanded_rules = 
    StringMap.mapi expand_rule grammar.rules
  and useful_types = 
      StringMap.filter 
	(fun k _ -> try not (StringMap.find k grammar.rules).inline_flag
	 with Not_found -> true)
	grammar.types
  in

    { grammar with 
	rules = StringMap.filter (fun _ r -> not r.inline_flag) expanded_rules;
	types = useful_types
    }, !use_inline
      
