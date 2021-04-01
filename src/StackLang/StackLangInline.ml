open StackLang
open StackLangUtils
open Infix
module Subst = Substitution

(* -------------------------------------------------------------------------- *)

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
      try LabelMap.find label !degree
      with Not_found -> Queue.add label queue ; 0
    in
    degree @:= LabelMap.add label (d + 1)
  in
  (* [visit () label] examines the block at address [label]. *)
  let visit () label = Block.successors tick (lookup label program.cfg).block in
  (* Initialize the queue with the entry labels. Process the queue until it
     becomes empty. Return the final table. *)
  StringMap.iter
    (fun _s label ->
      Queue.add label queue ;
      degree @:= LabelMap.add label 2)
    program.entry ;
  Misc.qfold visit () queue ;
  !degree

(* -------------------------------------------------------------------------- *)

(* [inline degree program] transforms the program [program] by removing every
   unreachable block and by inlining every block whose in-degree is 1. It is
   assumed that every entry label has an in-degree of at least 2. *)

let rec inline_block cfg degree block =
  match block with
  | IJump label ->
      (* If the target label's in-degree is 1, follow the indirection;
         otherwise, keep the [jump] instruction. *)
      if lookup label degree = 1 then
        let typed_block = lookup label cfg in
        ITypedBlock
          { typed_block with
            block= inline_block cfg degree typed_block.block
          ; name= Some ("inlined_" ^ label) }
      else IJump label
  | ISubstitutedJump (label, substitution) ->
      (* If the target label's in-degree is 1, follow the indirection;
         otherwise, keep the [jump] instruction. *)
      if lookup label degree = 1 then
        let typed_block = lookup label cfg in
        Subst.tight_restore_defs substitution (needed typed_block)
          (ITypedBlock
             { typed_block with
               block= inline_block cfg degree typed_block.block
             ; name= Some ("inlined_" ^ label) })
      else ISubstitutedJump (label, substitution)
  | block ->
      Block.map (inline_block cfg degree) block

let inline_cfg degree (cfg : typed_block RegisterMap.t) : cfg =
  LabelMap.fold
    (fun label ({block} as t_block) accu ->
      match LabelMap.find label degree with
      | exception Not_found ->
          (* An unreachable label. *)
          accu
      | d ->
          assert (d > 0) ;
          if d = 1 then accu
          else
            LabelMap.add label
              {t_block with block= inline_block cfg degree block}
              accu)
    cfg LabelMap.empty

let inline degree ({cfg; entry; states} as program) : program =
  if Settings.code_inlining then {cfg= inline_cfg degree cfg; entry; states}
  else program

(* [inline program] transforms the program [program] by removing every
   unreachable block and by inlining away every (non-entry) label whose
   in-degree is 1. *)

let inline program =
  let program = inline (in_degree program) program in
  Time.tick "Inlining in StackLang" ;
  program
