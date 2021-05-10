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

let length = List.length

let map = List.map

let mem = List.mem

open Lexing
open Printf
open Grammar
open StackLang
open StackLangUtils

(* -------------------------------------------------------------------------- *)

(* The exception [RuntimeError] is raised when the interpreter encounters an
   unexpected failure at runtime. This indicates that the StackLang program is
   incorrect in some way. *)

(* Checking (prior to execution) that the program is well-formed eliminates a
   category of errors (namely, accesses to undefined registers), but other
   sources of errors remain, such as attempting to pop off an empty stack,
   using a tuple pattern to deconstruct a value that is not a tuple of
   suitable arity, etc. *)

exception RuntimeError of string

let error format = ksprintf (fun s -> raise (RuntimeError s)) format

(* -------------------------------------------------------------------------- *)

(* The registers and the stack contain ground values. A ground value can be a
   token (produced by the lexer), a tag, a dummy value, or a tuple of ground
   values. *)

(* We cannot execute actual semantic actions, because they are just text. We
   cannot even hope to build concrete syntax trees (as done in the reference
   interpreter), because an optimized StackLang program does not necessarily
   keep track of all semantic values. Thus, we represent both positions and
   semantic values as dummy (unit) values. This means that a StackLang program
   is interpreted as a recognizer: it either accepts or rejects the input, but
   does return any information beyond this single bit. *)

type gvalue =
  | GVToken of Terminal.t
  | GVTag of tag
  | GVDummy
  | GVTuple of gvalue list

let asToken = function GVToken tok -> tok | _ -> error "a token was expected"

let asTag = function GVTag tag -> tag | _ -> error "a tag was expected"

(* -------------------------------------------------------------------------- *)

(* The runtime environment maps registers to ground values. *)

module Env = RegisterMap

type env = gvalue Env.t

(* Evaluating a value [v] yields a ground value. *)

let rec eval (env : env) (v : value) : gvalue =
  match v with
  | VTag tag ->
      GVTag tag
  | VReg r ->
    begin
      try RegisterMap.find r env with
      | Not_found ->
          error "undefined register: %s" r
    end
  | VTuple vs ->
      GVTuple (map (eval env) vs)
  | VUnit ->
      GVDummy


(* Matching a ground value [gv] against a pattern [p] extends the environment
   with new bindings. *)

let rec bind p gv (env : env) : env =
  match (p, gv) with
  | PWildcard, _ ->
      env
  | PReg r, _ ->
      Env.add r gv env
  | PTuple ps, GVTuple gvs ->
      if length ps = length gvs
      then List.fold_right2 bind ps gvs env
      else
        error
          "tuple pattern of arity %d cannot match tuple value of arity %d"
          (length ps)
          (length gvs)
  | PTuple _, _ ->
      error "tuple pattern cannot match a value that is not a tuple"


let binds = Bindings.fold (fun r v env -> bind (PReg r) (eval env v) env)

(* -------------------------------------------------------------------------- *)

(* The interpreter's state is as follows. *)

type state =
  { (* Should trace instructions produce output on [stderr]? *)
    trace : bool
  ; (* A lexer that produces terminal symbols instead of actual tokens. *)
    lexer : lexbuf -> Terminal.t
  ; (* A lexing buffer, used by the lexer, and out of which positions are read. *)
    lexbuf : lexbuf
  ; (* The program. *)
    program : program
  ; (* The runtime environment. *)
    mutable env : env
  ; (* The stack. *)
    mutable stack : gvalue list
  }

(* -------------------------------------------------------------------------- *)

(* Executing a primitive operation. *)

let exec_prim state p =
  match p with
  | PrimOCamlCall (_, _) ->
      (* We assume that this is a call of the form [discard lexer lexbuf]. *)
      (* We do not look up the registers [lexer] and [lexbuf], so it is okay
         if they are not defined. *)
      let tok = state.lexer state.lexbuf in
      if state.trace
      then
        eprintf
          "Lookahead token is now %s (%d-%d)\n"
          (Terminal.print tok)
          state.lexbuf.lex_start_p.pos_cnum
          state.lexbuf.lex_curr_p.pos_cnum;
      GVToken tok
  | PrimOCamlFieldAccess (_, _) ->
      (* We assume that this is an access to a position field in [lexbuf]. *)
      (* A position is replaced with a dummy value. *)
      GVDummy
  | PrimOCamlDummyPos ->
      (* A position is replaced with a dummy value. *)
      GVDummy
  | PrimOCamlAction _ ->
      (* A semantic value is replaced with a dummy value. *)
      GVDummy


(* -------------------------------------------------------------------------- *)

(* Popping a ground value off the stack. *)

let pop state : gvalue =
  match state.stack with
  | gv :: stack ->
      state.stack <- stack;
      gv
  | [] ->
      error "attempt to pop something off an empty stack"


(* -------------------------------------------------------------------------- *)

(* Executing a block. *)

let rec exec state block =
  match block with
  | INeed (rs, block) ->
      let required = rs
      and available = Env.domain state.env in
      if not (StringSet.subset required available)
      then
        error
          "incorrect NEED annotation; have %s, need %s"
          (RegisterSet.print available)
          (RegisterSet.print required);
      state.env <- Env.restrict required state.env;
      exec state block
  | IPush (v, _, block) ->
      let gv = eval state.env v in
      state.stack <- gv :: state.stack;
      exec state block
  | IPop (p, block) ->
      let gv = pop state in
      state.env <- bind p gv state.env;
      exec state block
  | IDef (bindings, block) ->
      state.env <- binds bindings state.env;
      exec state block
  | IPrim (r, p, block) ->
      let gv = exec_prim state p in
      state.env <- bind (PReg r) gv state.env;
      exec state block
  | ITrace (s, block) ->
      if state.trace then prerr_string s;
      exec state block
  | IComment (_, block) ->
      exec state block
  | IDie ->
      None (* reject *)
  | IReturn v ->
      let _gv = eval state.env v in
      Some ()
      (* accept *)
  | IJump label ->
      let block = (lookup label state.program.cfg).block in
      exec state block
  | ICaseToken (r, branches, odefault) ->
      let tok = asToken (eval state.env (VReg r)) in
      exec_casetoken state tok branches odefault
  | ICaseTag (r, branches) ->
      let tag = asTag (eval state.env (VReg r)) in
      exec_casetag state tag branches
  | ITypedBlock { block; stack_type = _; final_type = _ } ->
      exec state block


and exec_casetoken state tok branches odefault =
  match (branches, odefault) with
  | (TokSingle (tok', r), block) :: _, _ when Terminal.equal tok tok' ->
      state.env <- bind (PReg r) GVDummy state.env;
      exec state block
  | (TokMultiple toks, block) :: _, _ when TerminalSet.mem tok toks ->
      exec state block
  | _ :: branches, _ ->
      exec_casetoken state tok branches odefault
  | [], Some block ->
      exec state block
  | [], None ->
      error "nonexhaustive case analysis on a token (%s)" (Terminal.print tok)


and exec_casetag state tag branches =
  match branches with
  | (TagMultiple tags, block) :: _ when mem tag tags ->
      exec state block
  | _ :: branches ->
      exec_casetag state tag branches
  | [] ->
      error "nonexhaustive case analysis on a tag (%s)" (string_of_tag tag)


(* -------------------------------------------------------------------------- *)

(* The interpretation of a program begins with an environment in which
   certain dummy bindings are made (in order to avoid runtime failures
   at NEED instructions) and with an empty stack. *)

let interpret program label trace lexer lexbuf =
  let env =
    List.fold_right
      (fun r env -> Env.add r GVDummy env)
      EmitStackLang.required
      Env.empty
  and stack = [] in
  let state = { trace; lexer; lexbuf; program; env; stack } in
  exec state (IJump label)
