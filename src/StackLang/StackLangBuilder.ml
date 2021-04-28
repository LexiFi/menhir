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

module TerminalSet = Grammar.TerminalSet
open StackLang

let identity x =
  x

let compose f g x =
  f (g x)

type state =
  | Idle
  | Open of (block -> block)
  | Closed of block

let current : state ref =
  ref Idle
let current_stack_type : cell_info array option ref =
  ref None
let current_final_type : Stretch.ocamltype option ref =
  ref None
let current_needed : RegisterSet.t option ref =
  ref None

let current_has_case_tag : bool ref =
  ref false

let typed_exec (body : unit -> unit) =
  current := Open identity ;
  current_stack_type := None ;
  current_final_type := None ;
  current_needed := None ;
  current_has_case_tag := false ;
  body () ;
  let has_case_tag = !current_has_case_tag in
  match !current, !current_stack_type, !current_needed  with
  | Idle, _, _ ->
      (* This cannot happen, I think. *)
      assert false
  | Open _, _, _ ->
      (* The user has misused our API: a block has been opened and has not
         been properly ended by calling [die], [return], [jump], or a case
         analysis construction. *)
      assert false
  | _, None, _ | _, _, None   ->
    (* The user must specifiy the type of a block
    *)
    assert false
  | Closed block, Some stack_type, Some needed_registers ->
      let final_type = !current_final_type in
      current := Idle ;
      current_stack_type := None ;
      current_final_type := None ;
      current_needed := None ;
      current_has_case_tag := false ;
      { block
      ; stack_type
      ; final_type
      ; needed_registers
      ; has_case_tag
      ; name= None}

let exec (body : unit -> unit) =
  current := Open identity ;
  body () ;
  match !current with
  | Idle ->
      (* This cannot happen, I think. *)
      assert false
  | Open _ ->
      (* The user has misused our API: a block has been opened and has not
         been properly ended by calling [die], [return], [jump], or a case
         analysis construction. *)
      assert false
  | Closed block ->
      current := Idle;
      block


let extend g =
  match !current with
  | Idle ->
      (* The user has misused our API: an instruction generation operation
         has been called while no block was in construction. *)
      assert false
  | Open f ->
      (* The current block is extended with [g] and remains open. *)
      current := Open (compose f g)
  | Closed _ ->
      (* The user has misused our API: an instruction generation operation
         has been called, but the block under construction has already been
         ended by calling [die], [return], [jump], or a case analysis
         construction. *)
      assert false

let close i =
  match !current with
  | Idle ->
      (* The user has misused our API: an instruction generation operation
         has been called while no block was in construction. *)
      assert false
  | Open f ->
      (* The instruction [i] is the final instruction in this block, which
         becomes closed. *)
      current := Closed (f i)
  | Closed _ ->
      (* The user has misused our API: an instruction generation operation
         has been called, but the block under construction has already been
         ended by calling [die], [return], [jump], or a case analysis
         construction. *)
      assert false

let set_stack_type typ =
  current_stack_type := Some typ

let set_final_type typ =
    current_final_type := Some typ

let set_needed needed =
  current_needed := Some (RegisterSet.of_list needed)
let need rs =
  extend (fun block -> INeed (rs, block))

let need_list rs =
  need (RegisterSet.of_list rs)

let push v cell =
  extend (fun block -> IPush (v, cell, block))

let pop p =
  extend (fun block -> IPop (p, block))

let def p v =
  (* In order to avoid unnecessary clutter, we eliminate a definition of
     the form [def x = x] on the fly. *)
  match p, v with
  | PReg dst, VReg src when dst = src ->
      ()
  | _, _ ->
      extend (fun block -> IDef (p, v, block))

let move dst src =
  def (PReg dst) (VReg src)

let prim r p =
  extend (fun block -> IPrim (r, p, block))

let trace s =
  extend (fun block -> ITrace (s, block))

let comment s =
  extend (fun block -> IComment (s, block))

let die () =
  close IDie

let return r =
  close (IReturn r)

let jump l =
  close (IJump l)

let tokens tokpat =
  match tokpat with
  | TokSingle (tok, _) ->
      TerminalSet.singleton tok
  | TokMultiple toks ->
      toks

let tokens branches =
  List.fold_left (fun accu (tokpat, _) ->
    TerminalSet.union accu (tokens tokpat)
  ) TerminalSet.empty branches


let exhaustive branches =
  TerminalSet.subset TerminalSet.universe (tokens branches)

let case_token r cases =
  (* Save the block under construction. *)
  let saved = !current in
  (* Create a growing list of branches. *)
  let branches, default = ref [], ref None in
  (* Define a function that creates a new branch. *)
  let def_branch pat body = branches := (pat, exec body) :: !branches in
  (* Define a function that creates the default branch. *)
  let def_default body = default := Some (exec body) in
  (* Give the user access to these functions. *)
  cases def_branch def_default;
  (* Retrieve the branches that have been constructed. *)
  let branches, default = List.rev !branches, !default in
  (* If the branches cover all terminal symbols, then there is no need
     for a default branch; drop it. *)
  let default = if exhaustive branches then None else default in
  (* Restore the block that was under construction and close it with an
     [ICaseToken] instruction. *)
  current := saved;
  close (ICaseToken (r, branches, default))

let case_tag r cases =
  current_has_case_tag := true;
  let saved = !current in
  let branches = ref [] in
  let def_branch pat body = branches := (pat, exec body) :: !branches in
  cases def_branch;
  let branches = List.rev !branches in
  current := saved;
  (* There used to be an optimisation that does not perform the match if there
     is only one branch. However this does not work : we need to uncover type
     information here *)
  close (ICaseTag (r, branches))

module Build (L : sig
  type label
  val print: label -> string
  val iter: (label -> unit) -> unit
  val code: label -> unit
  val entry: string StringMap.t
  val states: state_info TagMap.t
end) =
struct
  open L

  let code (label : label) =
    typed_exec (fun () -> code label)


  let cfg =
    ref LabelMap.empty

  let () =
    iter (fun label ->
      cfg := LabelMap.add (print label) (code label) !cfg
    )


  let cfg =
    !cfg

  let entry =
    L.entry

  let program =
    {cfg; entry; states}
end (* Build *)
