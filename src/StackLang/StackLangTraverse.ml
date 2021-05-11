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

open Printf
open StackLang

let branch_iter f (_pat, block) = f block

let branch_map f (pat, block) = (pat, f block)

(* -------------------------------------------------------------------------- *)

(* Checking that a StackLang program contains no references to undefined
   registers. *)

let wf_regs label rs rs' =
  (* Check that [rs'] is a subset of [rs]. *)
  let stray = RegisterSet.diff rs' rs in
  if not (RegisterSet.is_empty stray)
  then begin
    eprintf
      "StackLang: in block %s, reference to undefined register%s:\n  %s\n"
      label
      (if RegisterSet.cardinal stray > 1 then "s" else "")
      (RegisterSet.print stray);
    eprintf
      "StackLang: the following registers are defined:\n  %s\n"
      (RegisterSet.print rs);
    exit 1
  end


let wf_reg label rs r = wf_regs label rs (RegisterSet.singleton r)

let rec wf_value label rs v =
  match v with
  | VTag _ ->
      ()
  | VReg r ->
      wf_reg label rs r
  | VTuple vs ->
      List.iter (wf_value label rs) vs


let rec def rs p =
  match p with
  | PWildcard ->
      rs
  | PReg r ->
      (* Check that no name is bound twice by a pattern. *)
      assert (not (RegisterSet.mem r rs));
      RegisterSet.add r rs
  | PTuple ps ->
      List.fold_left def rs ps


let def rs p =
  (* The newly defined registers are the previously defined registers
     plus the registers defined by the pattern [p]. *)
  RegisterSet.union rs (def RegisterSet.empty p)


let wf_prim label rs p =
  match p with
  | PrimOCamlCall (_, args) ->
      List.iter (wf_reg label rs) args
  | PrimOCamlFieldAccess (r, _) ->
      wf_reg label rs r
  | PrimOCamlDummyPos ->
      ()
  | PrimOCamlAction _ ->
      ()


(* [wf_block cfg label rs block] checks that the block [block] does not refer
   to an undefined register, under the assumption that the registers [rs] are
   initially defined. The control flow graph [cfg] is used to map labels to
   blocks. [label] is the label of the current block and is used only as part
   of error messages. *)

let rec wf_block cfg label rs block =
  match block with
  | INeed (rs', block) ->
      wf_regs label rs rs';
      (* A [need] instruction undefines the registers that it does not
         mention, so we continue with [rs']. *)
      let rs = rs' in
      wf_block cfg label rs block
  | IPush (v, block) ->
      wf_value label rs v;
      wf_block cfg label rs block
  | IPop (p, block) ->
      let rs = def rs p in
      wf_block cfg label rs block
  | IDef (p, v, block) ->
      wf_value label rs v;
      let rs = def rs p in
      wf_block cfg label rs block
  | IPrim (r, p, block) ->
      wf_prim label rs p;
      let rs = def rs (PReg r) in
      wf_block cfg label rs block
  | ITrace (_, block) | IComment (_, block) ->
      wf_block cfg label rs block
  | IDie ->
      ()
  | IReturn r ->
      wf_reg label rs r
  | IJump label' ->
      (* Check that every register that is needed at the destination label
         is defined here. *)
      wf_regs label rs (needed (lookup label' cfg))
  | ICaseToken (r, branches, odefault) ->
      wf_reg label rs r;
      List.iter (wf_branch cfg label rs) branches;
      Option.iter (wf_block cfg label rs) odefault
  | ICaseTag (r, branches) ->
      wf_reg label rs r;
      List.iter (branch_iter (wf_block cfg label rs)) branches


and wf_branch cfg label rs (tokpat, block) =
  let rs =
    match tokpat with
    | TokSingle (_, r) ->
        def rs (PReg r)
    | TokMultiple _ ->
        rs
  in
  wf_block cfg label rs block


(* [wf_block cfg label block] checks that the block [block] at address [label]
   does not refer to an undefined register. We assume that the block begins
   with an [INeed] instruction and use this instruction serves as a reference
   to find out which registers are initially defined. *)

let wf_block cfg label block = wf_block cfg label (needed block) block

(* [wf program] checks that the program [program] contains no references to
   undefined registers. *)

let wf program =
  LabelMap.iter (wf_block program.cfg) program.cfg;
  Time.tick "Checking the StackLang code for well-formedness"


(* -------------------------------------------------------------------------- *)

(* [successors yield block] applies the function [yield] in turn to every
   label that is the target of a [jump] instruction in the block [block]. *)

let rec successors yield block =
  match block with
  | INeed (_, block)
  | IPush (_, block)
  | IPop (_, block)
  | IDef (_, _, block)
  | IPrim (_, _, block)
  | ITrace (_, block)
  | IComment (_, block) ->
      successors yield block
  | IDie | IReturn _ ->
      ()
  | IJump label ->
      yield label
  | ICaseToken (_, branches, oblock) ->
      List.iter (branch_iter (successors yield)) branches;
      Option.iter (successors yield) oblock
  | ICaseTag (_, branches) ->
      List.iter (branch_iter (successors yield)) branches


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
      try LabelMap.find label !degree with
      | Not_found ->
          Queue.add label queue;
          0
    in
    degree := LabelMap.add label (d + 1) !degree
  in

  (* [visit () label] examines the block at address [label]. *)
  let visit () label = lookup label program.cfg |> successors tick in

  (* Initialize the queue with the entry labels. Process the queue until it
     becomes empty. Return the final table. *)
  Lr1.NodeMap.iter
    (fun _s label ->
      Queue.add label queue;
      degree := LabelMap.add label 2 !degree )
    program.entry;
  Misc.qfold visit () queue;
  !degree


(* -------------------------------------------------------------------------- *)

(* [inline degree program] transforms the program [program] by removing every
   unreachable block and by inlining every block whose in-degree is 1. It is
   assumed that every entry label has an in-degree of at least 2. *)

let rec inline_block cfg degree block =
  match block with
  | INeed (rs, block) ->
      INeed (rs, inline_block cfg degree block)
  | IPush (v, block) ->
      IPush (v, inline_block cfg degree block)
  | IPop (p, block) ->
      IPop (p, inline_block cfg degree block)
  | IDef (p, v, block) ->
      IDef (p, v, inline_block cfg degree block)
  | IPrim (r, p, block) ->
      IPrim (r, p, inline_block cfg degree block)
  | ITrace (s, block) ->
      ITrace (s, inline_block cfg degree block)
  | IComment (s, block) ->
      IComment (s, inline_block cfg degree block)
  | IDie ->
      IDie
  | IReturn r ->
      IReturn r
  | IJump label ->
      (* If the target label's in-degree is 1, follow the indirection;
         otherwise, keep the [jump] instruction. *)
      if lookup label degree = 1
      then inline_block cfg degree (lookup label cfg)
      else IJump label
  | ICaseToken (r, branches, odefault) ->
      ICaseToken
        ( r
        , List.map (branch_map (inline_block cfg degree)) branches
        , Option.map (inline_block cfg degree) odefault )
  | ICaseTag (r, branches) ->
      ICaseTag (r, List.map (branch_map (inline_block cfg degree)) branches)


let inline_cfg degree cfg =
  LabelMap.fold
    (fun label block accu ->
      match LabelMap.find label degree with
      | exception Not_found ->
          (* An unreachable label. *)
          accu
      | d ->
          assert (d > 0);
          if d = 1
          then accu
          else LabelMap.add label (inline_block cfg degree block) accu )
    cfg
    LabelMap.empty


let inline degree { cfg; entry } = { cfg = inline_cfg degree cfg; entry }

(* [inline program] transforms the program [program] by removing every
   unreachable block and by inlining away every (non-entry) label whose
   in-degree is 1. *)

let inline program =
  let program = inline (in_degree program) program in
  Time.tick "Inlining in StackLang";
  program


(* -------------------------------------------------------------------------- *)

(* Measuring the size of a StackLang program. *)

type measure =
  { mutable push : int
  ; mutable pop : int
  ; mutable def : int
  ; mutable prim : int
  ; mutable trace : int
  ; mutable die : int
  ; mutable return : int
  ; mutable jump : int
  ; mutable casetoken : int
  ; mutable casetag : int
  ; mutable total : int
  }

let zero () =
  { push = 0
  ; pop = 0
  ; def = 0
  ; prim = 0
  ; trace = 0
  ; die = 0
  ; return = 0
  ; jump = 0
  ; casetoken = 0
  ; casetag = 0
  ; total = 0
  }


let print m =
  let pad i = Misc.padded_index m.total i in
  printf "PUSH    %s\n" (pad m.push);
  printf "POP     %s\n" (pad m.pop);
  printf "DEF     %s\n" (pad m.def);
  printf "PRIM    %s\n" (pad m.prim);
  printf "TRCE    %s\n" (pad m.trace);
  printf "DIE     %s\n" (pad m.die);
  printf "RET     %s\n" (pad m.return);
  printf "JUMP    %s\n" (pad m.jump);
  printf "CASEtok %s\n" (pad m.casetoken);
  printf "CASEtag %s\n" (pad m.casetag);
  printf "total   %s\n" (pad m.total);
  ()


let rec measure_block m block =
  match block with
  | INeed (_, block) ->
      measure_block m block
  | IPush (_, block) ->
      m.total <- m.total + 1;
      m.push <- m.push + 1;
      measure_block m block
  | IPop (_, block) ->
      m.total <- m.total + 1;
      m.pop <- m.pop + 1;
      measure_block m block
  | IDef (_, _, block) ->
      m.total <- m.total + 1;
      m.def <- m.def + 1;
      measure_block m block
  | IPrim (_, _, block) ->
      m.total <- m.total + 1;
      m.prim <- m.prim + 1;
      measure_block m block
  | ITrace (_, block) ->
      m.total <- m.total + 1;
      m.trace <- m.trace + 1;
      measure_block m block
  | IComment (_, block) ->
      measure_block m block
  | IDie ->
      m.total <- m.total + 1;
      m.die <- m.die + 1
  | IReturn _ ->
      m.total <- m.total + 1;
      m.return <- m.return + 1
  | IJump _ ->
      m.total <- m.total + 1;
      m.jump <- m.jump + 1
  | ICaseToken (_, branches, odefault) ->
      m.total <- m.total + 1;
      m.casetoken <- m.casetoken + 1;
      List.iter (branch_iter (measure_block m)) branches;
      Option.iter (measure_block m) odefault
  | ICaseTag (_, branches) ->
      m.total <- m.total + 1;
      m.casetag <- m.casetag + 1;
      List.iter (branch_iter (measure_block m)) branches


let measure program =
  let m = zero () in
  LabelMap.iter (fun _ block -> measure_block m block) program.cfg;
  m
