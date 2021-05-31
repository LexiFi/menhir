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

let rec grouped_separate sep docs =
  match docs with
  | [] ->
      empty
  | [ doc ] ->
      doc
  | doc :: docs ->
      group (doc ^^ sep ^^ grouped_separate sep docs)


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


let symbol s = string (Grammar.Symbol.print s)

let cell_info
    Invariant.{ symbol = s; holds_semv; holds_state; holds_startp; holds_endp }
    =
  symbol s
  ^^ OCaml.int (Bool.to_int holds_semv)
  ^^ OCaml.int (Bool.to_int holds_state)
  ^^ OCaml.int (Bool.to_int holds_startp)
  ^^ OCaml.int (Bool.to_int holds_endp)


let final_type ft = OCaml.option ocamltype ft

let cells cells =
  let cells = Array.to_list cells in
  brackets @@ separate (semi ^^ space) (List.map cell_info cells)


let state_info { known_cells = kn; sfinal_type } =
  nl
  ^^ string "Known_cells: "
  ^^ cells kn
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
  align @@ group (grouped_separate (comma ^^ break 1) (map register rs)) ^^ dot


let rec typed_block_instruction
    { stack_type; needed_registers = rs; final_type = ft } =
  nl
  ^^ spaced_braces
  @@ align
  @@ group
       ( string "Final type :"
       ^^ final_type ft
       ^^ break 1
       ^^ string "Known cells :"
       ^^ cells stack_type
       ^^ break 1
       ^^ string "Needed registers :"
       ^^ registers rs )


and typed_block tb = typed_block_instruction tb ^^ block tb.block

and partial_tblock culprit tb =
  typed_block_instruction tb ^^ partial_block culprit tb.block


and instruction b =
  match b with
  | IPush (v, c, _) ->
      nl ^^ string "PUSH " ^^ value v ^^ cell_info c
  | IPop (p, _) ->
      nl ^^ string "POP " ^^ pattern p
  | IDef (bs, _) ->
      nl ^^ string "DEF " ^^ bindings bs
  | IPrim (r, p, _) ->
      nl ^^ string "PRIM " ^^ register r ^^ string " = " ^^ primitive p
  | ITrace (s, _) ->
      nl ^^ string "TRACE " ^^ OCaml.string s
  | IComment (s, _) ->
      nl ^^ string "#### " ^^ align (arbitrary_string s)
  | IDie ->
      nl ^^ string "DIE"
  | IReturn v ->
      nl ^^ string "RET  " ^^ value v
  | IJump l ->
      nl ^^ string "JUMP " ^^ label l
  | ICaseToken (r, _, _) ->
      nl ^^ string "CASE " ^^ register r ^^ string " OF"
  | ICaseTag (r, _) ->
      nl ^^ string "CASE " ^^ register r ^^ string " OF"
  | ITypedBlock tb ->
      nl ^^ string "TYPED " ^^ typed_block_instruction tb


and block b =
  instruction b
  ^^
  match b with
  | IPush (_, _, b)
  | IPop (_, b)
  | IDef (_, b)
  | IPrim (_, _, b)
  | ITrace (_, b)
  | IComment (_, b) ->
      block b
  | IDie | IReturn _ | IJump _ ->
      empty
  | ICaseToken (_, branches, default) ->
      concat
        (map
           branch
           ( map (fun (pat, b) -> (tokpat pat, block b)) branches
           @
           match default with Some b -> [ (underscore, block b) ] | None -> []
           ) )
  | ICaseTag (_, branches) ->
      concat (map branch (map (fun (pat, b) -> (tagpat pat, block b)) branches))
  | ITypedBlock { block = b } ->
      block b


and partial_block culprit b =
  instruction b
  ^^
  if culprit == b
  then nl ^^ string "(...)"
  else
    match b with
    | IPush (_, _, b)
    | IPop (_, b)
    | IDef (_, b)
    | IPrim (_, _, b)
    | ITrace (_, b)
    | IComment (_, b) ->
        partial_block culprit b
    | IDie | IReturn _ | IJump _ ->
        empty
    | ICaseToken (_, branches, default) ->
        concat
          (map
             branch
             ( map
                 (fun (pat, b) ->
                   ( tokpat pat
                   , if Block.contains b culprit
                     then partial_block culprit b
                     else string " (...)" ) )
                 branches
             @
             match default with
             | Some b ->
                 [ ( underscore
                   , if Block.contains b culprit
                     then partial_block culprit b
                     else string " (...)" )
                 ]
             | None ->
                 [] ) )
    | ICaseTag (_, branches) ->
        concat
          (map
             branch
             (map
                (fun (pat, b) ->
                  ( tagpat pat
                  , if Block.contains b culprit
                    then partial_block culprit b
                    else string "(...)" ) )
                branches ) )
    | ITypedBlock { block = b } ->
        partial_block culprit b


let entry_comment entry_labels label =
  if LabelSet.mem label entry_labels
  then string "## Entry point:" ^^ nl
  else empty


let labeled_block entry_labels (label, tb) =
  entry_comment entry_labels label
  ^^ string label
  ^^ colon
  ^^ nest 2 (typed_block tb)
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

let print_known_cells = to_channel cells

let known_cells_to_string = to_string cells

let print_states = to_channel states

let states_to_string = to_string states

let instruction_to_string = to_string instruction

let print_instruction = to_channel instruction

let print_partial_block ~culprit = to_channel (partial_block culprit)

let partial_block_to_string ~culprit = to_string (partial_block culprit)

let print_partial_tblock ~culprit = to_channel (partial_tblock culprit)

let partial_tblock_to_string ~culprit = to_string (partial_tblock culprit)

let to_string = to_string program
