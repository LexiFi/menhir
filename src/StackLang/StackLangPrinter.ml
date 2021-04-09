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

let nl =
  hardline
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
  | VUnit -> label "UNIT"

let substitution substitution =
  Substitution.fold
    ( fun reg value' acc ->
        nl ^^ (register reg) ^^ string " := " ^^ (value value') ^^ acc )
    substitution
    empty

let ocamltype =
  function
  | Stretch.Inferred typ -> string typ
  | Stretch.(Declared {stretch_content}) -> string stretch_content

let cell_info {typ; hold_semv; hold_state; hold_startpos; hold_endpos} =
  string "["
  ^^ (match typ with None -> string "." | Some typ -> ocamltype typ  )
  ^^ tag (Bool.to_int hold_semv)
  ^^ tag (Bool.to_int hold_state)
  ^^ tag (Bool.to_int hold_startpos)
  ^^ tag (Bool.to_int hold_endpos)
  ^^ string "]"


let known_cells known_cells=
  Array.fold_left
    (fun doc cell -> doc ^^ cell_info cell)
    empty
    known_cells

let state_info {known_cells=kn; sfinal_type} =
  nl ^^ string "Known_cells: " ^^ known_cells kn ^^
  nl ^^ string "Final type: " ^^ PPrintOCaml.option ocamltype sfinal_type

let states states =
  string "States :"
  ^^ nest 2 ( TagMap.fold
                ( fun t si doc ->
                    nl
                    ^^ string "State "
                    ^^ tag t
                    ^^ nest 2 (state_info si)
                    ^^ doc )
                states empty )

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
      string f ^^ concat (map (fun r -> space ^^ value r) rs)
  | PrimOCamlFieldAccess (r, f) ->
      utf8format "%s.%s" r f
  | PrimOCamlDummyPos ->
      utf8format "<dummy position>"
  | PrimSubstOcamlAction (subst, _) ->
    string "("
    ^^ substitution subst
    ^^ utf8format "<semantic action>"
    ^^ string ")"
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

let branch (guard, body) =
  nl ^^ bar ^^ space ^^ group (guard ^^ string " ->" ^^ nest 4 body)

let rec block b =
  match b with
  | INeed (rs, b) ->
      let rs = RegisterSet.elements rs in
      nl ^^ string "NEED " ^^ separate (comma ^^ space) (map register rs) ^^
      block b
  | IPush (v, _, b) ->
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
  | IReturn v ->
      nl ^^ string "RET  " ^^ value v
  | IJump l ->
      nl ^^ string "JUMP " ^^ label l
  | ISubstitutedJump (l, sub) ->
      nl ^^ string "SUBST" ^^ substitution sub ^^
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
  | ITypedBlock ({block=b; stack_type; needed_registers=rs; final_type}) ->
      let rs = RegisterSet.elements rs in
      nl
      ^^ string "TYPED { "
      ^^ string "Final type :" ^^ optional ocamltype final_type
      ^^ nl
      ^^ string "  Known cells : "
      ^^ known_cells stack_type
      ^^ nl
      ^^ string "  Needed registers :"
      ^^ separate (comma ^^ space) (map register rs)
      ^^ nl
      ^^ string "}"
      ^^ block b

let entry_comment entry_labels label =
  if LabelSet.mem label entry_labels then
    string "## Entry point:" ^^ nl
  else
    empty

let labeled_block entry_labels (label, {block=b; stack_type=_}) =
  entry_comment entry_labels label ^^
  string label ^^ colon ^^ nest 2 (block b) ^^ nl ^^ nl

let program program =
  states program.states ^^
  (program.cfg
  |> LabelMap.bindings
  |> map (labeled_block (StringMap.domain program.entry))
  |> concat)

let to_channel =
  ToChannel.pretty 0.8 80

let to_string doc =
  let buffer = Buffer.create 10 in
  ToBuffer.pretty 0.8 80 buffer doc ;
  Bytes.to_string @@ Buffer.to_bytes buffer

let print f prog =
  to_channel f (program prog)

let print_value f v =
  to_channel f (value v)

let value_to_string v =
  to_string (value v)

let pattern_to_string p =
  to_string (pattern p)

let known_cells_to_string kc =
  to_string (known_cells kc)

let print_substitution f s =
  to_channel f (substitution s)

let print_block f b =
  to_channel f (block b)

let print_known_cells f ks =
  to_channel f (known_cells ks)

let print_states f s =
  to_channel f (states s)
