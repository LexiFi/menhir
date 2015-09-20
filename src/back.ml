(* Driver for the back-end. *)

module I = Interpret (* artificial dependency; ensures that [Interpret] runs first *)

(* If [--list-errors] is set, produce a list of erroneous input sentences, then stop. *)

let () =
  if Settings.list_errors then begin
    let module L = LRijkstra.Run(struct end) in
    exit 0
  end

(* Define an .ml file writer . *)

let write program =
  let module P = Printer.Make (struct
    let filename = Settings.base ^ ".ml"
    let f = open_out filename
    let locate_stretches =
      if Settings.infer then
	(* Typechecking should not fail at this stage. Omit #line directives. *)
	None
      else
	(* 2011/10/19: do not use [Filename.basename]. The [#] annotations that
	   we insert in the [.ml] file must retain their full path. This does
	   mean that the [#] annotations depend on how menhir is invoked -- e.g.
	   [menhir foo/bar.mly] and [cd foo && menhir bar.mly] will produce
	   different files. Nevertheless, this seems useful/reasonable. *)
	Some filename
    let raw_stretch_action = false
  end) in
  P.program program

(* Construct the code, using either the table-based or the code-based
   back-end, and pass it on to the printer. (This continuation-passing
   style is imposed by the fact that there is no conditional in ocaml's
   module language.) *)

let () =
  if Settings.coq then
    let module B = CoqBackend.Run (struct end) in
    let filename = Settings.base ^ ".v" in
    let f = open_out filename in
    B.write_all f;
    exit 0
  else
    if Settings.table then
      let module B = TableBackend.Run (struct end) in
      write B.program
    else
      let module B = CodeBackend.Run (struct end) in
      write (Inliner.inline B.program)

(* Write the interface file. *)

let () =
  Interface.write Front.grammar ()

let () =
  Time.tick "Printing"

