open StackLang
open StackLangUtils
open Infix

let max_degree = 5

(* -------------------------------------------------------------------------- *)

let lookup_degree label degree =
  match LabelMap.find label degree with exception Not_found -> 0 | d -> d


(* [in_degree program] computes the in-degree of every label in the program
   [program]. It returns a table that maps every reachable label to its
   in-degree. Unreachable labels do not appear in the table. The entry labels
   artifically receive an in-degree of at least 2; this ensures that they
   cannot be inlined by the function [inline] that follows. *)

let in_degree program =
  (* Initialize a queue and a map of labels to degrees. *)
  let queue : label Queue.t = Queue.create ()
  and degree : int LabelMap.t ref = ref LabelMap.empty in
  (* [tick label] increments the degree associated with [label]. If its
     previous degree was zero, then [label] is enqueued for exploration. *)
  let tick label =
    let d =
      try LabelMap.find label !degree with
      | Not_found ->
          Queue.add label queue;
          0
    in
    degree @:= LabelMap.add label (d + 1)
  in
  (* [visit () label] examines the block at address [label]. *)
  let visit () label = Block.successors tick (lookup label program.cfg).block in
  (* Initialize the queue with the entry labels. Process the queue until it
     becomes empty. Return the final table. *)
  StringMap.iter
    (fun _s label ->
      Queue.add label queue;
      degree @:= LabelMap.add label max_int )
    program.entry;
  Misc.qfold visit () queue;
  !degree


(* -------------------------------------------------------------------------- *)

(** [inline degree program] transforms the program [program] by removing every
   unreachable block and by inlining every block whose in-degree is 1. It is
   assumed that every entry label has an in-degree of at least 2. *)
let rec inline_block cfg degree labels block =
  match block with
  | IJump label
    when (not @@ LabelSet.mem label labels)
         && lookup_degree label degree <= max_degree ->
      (* If the target label's in-degree is 1, follow the indirection;
         otherwise, keep the [jump] instruction. *)
      let typed_block = lookup label cfg in
      let labels = LabelSet.add label labels in
      ITypedBlock
        { typed_block with
          block = inline_block cfg degree labels typed_block.block
        ; name = Some ("inlined_" ^ label)
        }
  | block ->
      Block.map (inline_block cfg degree labels) block


let inline_t_block cfg degree label t_block =
  let { block } = t_block in
  { t_block with
    block = inline_block cfg degree (LabelSet.singleton label) block
  }


let remove_unused program =
  let degree = in_degree program in
  Program.filter (fun label _ -> lookup_degree label degree > 0) program


let inline degree ({ cfg } as program) : program =
  if Settings.code_inlining
  then remove_unused (Program.mapi (inline_t_block cfg degree) program)
  else program


(* [inline program] transforms the program [program] by removing every
   unreachable block and by inlining away every (non-entry) label whose
   in-degree is 1. *)

let inline program =
  let program = inline (in_degree program) program in
  Time.tick "Inlining in StackLang";
  program
