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

let map = List.map
open PPrint
open Grammar
open StackLang

let register =
  string

let tag =
  OCaml.int

let label =
  string

let rec value v =
  match v with
  | VTag t ->
      tag t
  | VReg r ->
      register r
  | VTuple vs ->
      OCaml.tuple (map value vs)

let rec pattern p =
  match p with
  | PWildcard ->
      underscore
  | PReg r ->
      register r
  | PTuple ps ->
      OCaml.tuple (map pattern ps)

let primitive p =
  match p with
  | PrimOCamlCall (f, rs) ->
      string f ^^ concat (map (fun r -> space ^^ register r) rs)
  | PrimOCamlFieldAccess (r, f) ->
      utf8format "%s.%s" r f
  | PrimOCamlDummyPos ->
      utf8format "<dummy position>"
  | PrimOCamlAction _ ->
      utf8format "<semantic action>"

let tokpat pat =
  match pat with
  | TokSingle (tok, r) ->
      utf8format "%s %s" (Terminal.print tok) r
  | TokMultiple toks ->
      toks
      |> TerminalSet.elements
      |> map (fun tok -> utf8format "%s _" (Terminal.print tok))
      |> flow (break 1 ^^ bar ^^ space)
      |> group

let tagpat pat =
  match pat with
  | TagMultiple tags ->
      tags
      |> map tag
      |> flow (break 1 ^^ bar ^^ space)
      |> group

let nl =
  hardline

let branch (guard, body) =
  nl ^^ bar ^^ space ^^ group (guard ^^ string " ->" ^^ nest 4 body)

let rec block b =
  match b with
  | INeed (rs, b) ->
      let rs = RegisterSet.elements rs in
      nl ^^ string "NEED " ^^ separate (comma ^^ space) (map register rs) ^^
      block b
  | IPush (v, b) ->
      nl ^^ string "PUSH " ^^ value v ^^
      block b
  | IPop (p, b) ->
      nl ^^ string "POP  " ^^ pattern p ^^
      block b
  | IDef (p, v, b) ->
      nl ^^ string "DEF  " ^^ pattern p ^^ string " = " ^^ value v ^^
      block b
  | IPrim (r, p, b) ->
      nl ^^ string "PRIM " ^^ register r ^^ string " = " ^^ primitive p ^^
      block b
  | ITrace (s, b) ->
      nl ^^ string "TRCE " ^^ OCaml.string s ^^
      block b
  | IComment (s, b) ->
      nl ^^ string "#### " ^^ string s ^^
      block b
  | IDie ->
      nl ^^ string "DIE"
  | IReturn r ->
      nl ^^ string "RET  " ^^ register r
  | IJump l ->
      nl ^^ string "JUMP " ^^ label l
  | ICaseToken (r, branches, default) ->
      nl ^^ string "CASE " ^^ register r ^^ string " OF" ^^
      concat (map branch (
        map (fun (pat, b) -> (tokpat pat, block b)) branches @
        match default with Some b -> [(underscore, block b)] | None -> []
      ))
  | ICaseTag (r, branches) ->
      nl ^^ string "CASE " ^^ register r ^^ string " OF" ^^
      concat (map branch (
        map (fun (pat, b) -> (tagpat pat, block b)) branches
      ))
  | ITypedBlock ({block=b; stack_type=_; final_type=_}) -> 
      nl ^^ string "TYPED " ^^ block b

let entry_comment entry_labels label =
  if LabelSet.mem label entry_labels then
    string "## Entry point:" ^^ nl
  else
    empty

let labeled_block entry_labels (label, {block=b; stack_type=_}) =
  entry_comment entry_labels label ^^
  string label ^^ colon ^^ nest 2 (block b) ^^ nl ^^ nl

let program program =
  program.cfg
  |> LabelMap.bindings
  |> map (labeled_block (entry_labels program))
  |> concat

let print f prog =
  ToChannel.pretty 0.8 80 f (program prog)
