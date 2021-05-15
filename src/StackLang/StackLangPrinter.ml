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

let nl = hardline

let register = string

let tag t = string (string_of_tag t)

let label = string

let spaced_braces doc = braces (space ^^ doc ^^ space)

let rec value v =
  match v with
  | VTag t ->
      tag t
  | VReg r ->
      register r
  | VTuple vs ->
      OCaml.tuple (map value vs)
  | VUnit ->
      label "UNIT"


let binding (r, v) = register r ^^ string " <- " ^^ value v

let bindings bds =
  let bds = Bindings.to_list bds in
  group
  @@ align
  @@ spaced_braces (separate (break 1 ^^ semi ^^ space) (List.map binding bds))


let ocamltype = function
  | Stretch.Inferred typ ->
      string typ
  | Stretch.(Declared { stretch_content }) ->
      string stretch_content


let cell_info { typ; hold_semv; hold_state; hold_startpos; hold_endpos } =
  string "["
  ^^ (match typ with None -> string "." | Some typ -> ocamltype typ)
  ^^ OCaml.int (Bool.to_int hold_semv)
  ^^ OCaml.int (Bool.to_int hold_state)
  ^^ OCaml.int (Bool.to_int hold_startpos)
  ^^ OCaml.int (Bool.to_int hold_endpos)
  ^^ string "]"


let final_type ft = OCaml.option ocamltype ft

let known_cells known_cells =
  Array.fold_left (fun doc cell -> doc ^^ cell_info cell) empty known_cells


let state_info { known_cells = kn; sfinal_type } =
  nl
  ^^ string "Known_cells: "
  ^^ known_cells kn
  ^^ nl
  ^^ string "Final type: "
  ^^ final_type sfinal_type


let states states =
  string "States :"
  ^^ nest
       2
       (TagMap.fold
          (fun t si doc ->
            nl ^^ string "State " ^^ tag t ^^ nest 2 (state_info si) ^^ doc )
          states
          empty )


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
  | PrimOCamlFieldAccess (v, f) ->
      value v ^^ dot ^^ string f
  | PrimOCamlDummyPos ->
      utf8format "<dummy position>"
  | PrimOCamlAction (bs, _) ->
      spaced_braces (align (bindings bs ^/^ utf8format "<semantic action>"))


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
      tags |> map tag |> flow (break 1 ^^ bar ^^ space) |> group


let branch (guard, body) =
  nl ^^ bar ^^ space ^^ group (guard ^^ string " ->" ^^ nest 4 body)


let registers rs =
  let rs = RegisterSet.elements rs in
  align @@ group (separate (comma ^^ break 1) (map register rs)) ^^ dot


let rec typed_block
    { block = b; stack_type; needed_registers = rs; final_type = ft } =
  nl
  ^^ spaced_braces
  @@ group
  @@ align
       ( string "Final type :"
       ^^ final_type ft
       ^^ break 1
       ^^ string "  Known cells : "
       ^^ known_cells stack_type
       ^^ break 1
       ^^ string "  Needed registers :"
       ^^ registers rs )
  ^^ block b


and block b =
  match b with
  | INeed (rs, b) ->
      nl ^^ string "NEED " ^^ registers rs ^^ block b
  | IPush (v, _, b) ->
      nl ^^ string "PUSH " ^^ value v ^^ block b
  | IPop (p, b) ->
      nl ^^ string "POP " ^^ pattern p ^^ block b
  | IDef (bs, b) ->
      nl ^^ string "DEF " ^^ bindings bs ^^ block b
  | IPrim (r, p, b) ->
      nl
      ^^ string "PRIM "
      ^^ register r
      ^^ string " = "
      ^^ primitive p
      ^^ block b
  | ITrace (s, b) ->
      nl ^^ string "TRCE " ^^ OCaml.string s ^^ block b
  | IComment (s, b) ->
      nl ^^ string "#### " ^^ align (arbitrary_string s) ^^ block b
  | IDie ->
      nl ^^ string "DIE"
  | IReturn v ->
      nl ^^ string "RET  " ^^ value v
  | IJump l ->
      nl ^^ string "JUMP " ^^ label l
  | ICaseToken (r, branches, default) ->
      nl
      ^^ string "CASE "
      ^^ register r
      ^^ string " OF"
      ^^ concat
           (map
              branch
              ( map (fun (pat, b) -> (tokpat pat, block b)) branches
              @
              match default with
              | Some b ->
                  [ (underscore, block b) ]
              | None ->
                  [] ) )
  | ICaseTag (r, branches) ->
      nl
      ^^ string "CASE "
      ^^ register r
      ^^ string " OF"
      ^^ concat
           (map branch (map (fun (pat, b) -> (tagpat pat, block b)) branches))
  | ITypedBlock tb ->
      nl ^^ string "TYPED " ^^ typed_block tb ^^ block b


let entry_comment entry_labels label =
  if LabelSet.mem label entry_labels
  then string "## Entry point:" ^^ nl
  else empty


let labeled_block entry_labels (label, { block = b; stack_type = _ }) =
  entry_comment entry_labels label
  ^^ string label
  ^^ colon
  ^^ nest 2 (block b)
  ^^ nl
  ^^ nl


let program program =
  states program.states
  ^^ ( program.cfg
     |> LabelMap.bindings
     |> map (labeled_block (StringMap.domain program.entry))
     |> concat )


let to_channel f channel args =
  let doc = f args in
  ToChannel.pretty 0.8 80 channel doc


let to_string f arg =
  let doc = f arg in
  let buffer = Buffer.create 10 in
  ToBuffer.pretty 0.8 80 buffer doc;
  Bytes.to_string @@ Buffer.to_bytes buffer


let print = to_channel program

let print_value = to_channel value

let value_to_string = to_string value

let print_pattern = to_channel pattern

let pattern_to_string = to_string pattern

let print_bindings = to_channel bindings

let bindings_to_string = to_string bindings

let print_block = to_channel block

let block_to_string = to_string block

let print_tblock = to_channel typed_block

let tblock_to_string = to_string typed_block

let print_known_cells = to_channel known_cells

let known_cells_to_string = to_string known_cells

let print_states = to_channel states

let states_to_string = to_string states

let to_string = to_string program
