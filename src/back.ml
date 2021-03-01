(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* Driver for the back-end. *)

(* The automaton is now frozen and will no longer be modified. It is
   time to dump a new description of it, if requested by the user. *)
let () =
  match Settings.provide_example with
  | None -> ()
  | Some name ->
      match Settings.provide_example_seed with
      | None -> Random.self_init () 
      | Some s -> Random.init s ;
      let budget = Settings.example_size in
      let nt = 
        let (_, node) = Grammar.ProductionMap.choose Lr1.entry in
        Lr1.nt_of_entry node
      in
      let s = SentenceGenerator.sentence 
                ~log:Settings.example_log 
                nt
                budget 
      in
      let file = open_out name in
      Array.iter ( fun terminal -> 
        Printf.fprintf 
          file 
          "%s\n" 
          (Grammar.Terminal.print terminal) ) s ;
      close_out file;
      exit 0
      
let () =
  if Settings.dump_resolved then
    let module D = Dump.Make (Default) in
    D.dump (Settings.base ^ ".automaton.resolved")

let () =
  if Settings.automaton_graph then AutomatonGraph.print_automaton_graph ()

(* Let [Interpret] handle the command line options [--interpret],
   [--interpret-error], [--compile-errors], [--compare-errors]. *)

let () = Interpret.run ()

(* If [--list-errors] is set, produce a list of erroneous input sentences, then stop. *)

let () =
  if Settings.list_errors then
    let module L = LRijkstra.Run (struct
      (* Undocumented: if [--log-automaton 2] is set, be verbose. *)
      let verbose = Settings.logA >= 2

      (* For my own purposes, LRijkstra can print one line of statistics to a .csv file. *)
      let statistics = if false then Some "lr.csv" else None
    end) in
    exit 0

(* Define an .ml file writer . *)

let write program =
  let module P = Printer.Make (struct
    let filename = Settings.base ^ ".ml"

    let f = open_out filename

    let locate_stretches =
      (* 2017/05/09: always include line number directives in generated .ml
         files. Indeed, they affect the semantics of [assert] instructions
         in the semantic actions. *)
      (* 2011/10/19: do not use [Filename.basename]. The line number
         directives that we insert in the [.ml] file must retain their full
         path. This does mean that the line number directives depend on how
         menhir is invoked -- e.g. [menhir foo/bar.mly] and [cd foo && menhir
         bar.mly] will produce different files. Nevertheless, this seems
         useful/reasonable. *)
      Some filename 
  end) in
  P.program program

(* If requested, generate a .cmly file. *)

let () = if Settings.cmly then Cmly_write.write (Settings.base ^ ".cmly")

(* Construct and print the code using an appropriate back-end. *)

let () =
  if
    Settings.stacklang_dump || Settings.stacklang_graph
    || Settings.stacklang_test
  then (
    let module SL = EmitStackLang.Run () in
    let program = SL.program in
   StackLangTraverse.wf program;
    let program = StackLangTraverse.inline program in
    StackLangTraverse.wf program;
    if Settings.stacklang_dump then (
      StackLangPrinter.print stdout program;
      StackLangTraverse.(print (measure program)) );
    if Settings.stacklang_graph then StackLangGraph.print program;
    if Settings.stacklang_test then StackLangTester.test program )

let () =
  if Settings.table then (
    let module B = TableBackend.Run () in
    write B.program;
    Interface.write Front.grammar () )
  else if Settings.coq then
    let module B = CoqBackend.Run () in
    let filename = Settings.base ^ ".v" in
    let f = open_out filename in
    B.write_all f
  else if Settings.old_code_backend then
    ( write ( let module C = CodeBackend.Run() in
              CodeInliner.inline C.program ) ;
    Interface.write Front.grammar () )
  else
    let module SL = EmitStackLang.Run () in
    let program = SL.program in
   StackLangTraverse.wf program ;
    let program = StackLangTraverse.inline program in
    StackLangTraverse.wf program ;
    let program = StackLangTraverse.optimize program in
    (*StackLangTraverse.wf program ;*)
    if Settings.stacklang_dump then (
      StackLangPrinter.print stdout program ;
      StackLangTraverse.(print (measure program)) ) ;
    if Settings.stacklang_graph then 
      StackLangGraph.print program ;
    if Settings.stacklang_test then 
      StackLangTester.test program ;
    let program = ILofStackLang.compile program in
    write program ;
    Interface.write Front.grammar ()

let () = Time.tick "Printing"

