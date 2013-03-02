open Sigs
open Printf

module Make (Proof : PROOF) = struct

  open Proof

  (* ------------------------------------------------------------------------ *)

  (* Textual display of a proof net. *)

  let print b net =

    (* Display nodes breadth-first. *)

    let module G = struct
      type node = Goal.goal
      let n = n ()
      let index = index
      let successors f goal = successors (fun _ goal -> f goal) goal
      let iter = iter_initial
    end in

    let module B = Bfs.Run (G) (struct

      let print_edge b edge =
	Buffer.add_string b
	  begin match edge with
	  | EWordDecreasing ->
	      "w"
	  | EDerivationDecreasing ->
	      "d"
	  | ENeutral ->
	      ""
	  | EDangerous ->
	      "!"
	  end

      let print_subgoal b (edge, goal) =
	bprintf b "(%d)%a"
	  (index goal)
	  print_edge edge

      let print_structure b structure =
	match structure with
	| Absent ->
	    bprintf b "This goal is presently open.\n"
	| Disjunction steps ->
	    List.iter (function step ->
	      match step.subgoals with
	      | [] ->
		  bprintf b "Proof: %s\n" step.reason
	      | _ :: _ ->
		  bprintf b "Proof: by %a (%s)\n"
		    (Print.seplist Print.comma print_subgoal) step.subgoals
		    step.reason
	    ) steps

      let print_goal b goal =
	bprintf b "[%d]\nGoal: %a\nStatus: %a\n%a\n"
	  (index goal)
	  Goal.print goal
	  Bool3.print (status goal)
	  print_structure (structure goal)

      let discover depth goal =
	assert (depth >= 0);
	print_goal b goal

    end) in

    ()

  (* ------------------------------------------------------------------------ *)

  (* Dot output, with goals ranked by depth. *)

  let bdot b () =

    (* Set the graph's global attributes. *)

    bprintf b "digraph \"%s\" {\n" Settings.dotname;
    bprintf b "size=\"8.0, 11.0\";\n";
    bprintf b "orientation = portrait;\n";
    bprintf b "rankdir = TB;\n";
    bprintf b "ratio = auto;\n";
    bprintf b "ranksep = \".75 equally\";\n";
    bprintf b "node [ shape = record ];\n";

    (* Define a breadth-first iterator over all accessible nodes. *)

    let module G = struct
      type node = Goal.goal
      let n = n ()
      let index = index
      let successors f goal = successors (fun _ goal -> f goal) goal
      let iter = iter_initial
    end in

    let iter_accessible f =
      let module B = Bfs.Run (G) (struct
	let discover =
	  f
      end) in
      ()
    in

    (* Declare all (accessible) nodes. Goals that are considered obvious
       (that is, have no subgoals) are drawn in bold style. *)

    let internal b goal =
      bprintf b "g%d" (index goal)
    in

    let obvious goal =
      match structure goal with
      | Disjunction [ { subgoals = [] } ] ->
	  true
      | _ ->
	  false
    in

    iter_accessible (fun _ goal ->
      bprintf b "%a [ label=\"%d| %a\"%s ];\n"
	internal goal
	(index goal)
	Goal.eprint goal
	(if (obvious goal) then ", style=bold" else "")
    );
    
    (* Impose same-rank constraints. *)

    let pending =
      ref false

    and current =
      ref (-1)
    in

    iter_accessible (fun depth goal ->
      if depth > !current then begin  (* goal at new depth *)
	if !pending then    (* close previous rank if open *)
	  bprintf b "}\n";
	bprintf b "{ rank = same; ";      (* open new rank *)
	pending := true;
	current := depth
      end;
      bprintf b "%a; " internal goal (* goal at this depth *)
    );

    if !pending then                      (* close last rank *)
      bprintf b "}\n";

    (* Declare all edges. The edge style reflects the nature
       of the edge with respect to induction. *)

    (* TEMPORARY display visual warning when goal has several proofs *)

    iter_accessible (fun _ source ->
      successors (fun edge destination ->
	let style =
	  match edge with
	  | EWordDecreasing ->
	      "style=bold"
	  | EDerivationDecreasing ->
	      "style=solid"
	  | ENeutral ->
	      "style=dashed"
	  | EDangerous ->
	      "style=dashed,color=red"
	in
	bprintf b "%a -> %a [ %s ];\n"
	  internal source
	  internal destination
	  style
      ) source
    );

    (* Done. *)

    bprintf b "}\n"

end

