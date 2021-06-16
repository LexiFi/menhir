open StackLang
open StackLangUtils
open Sync

let state_reg = NamingConventions.state

type cell = Invariant.cell

type cells = Invariant.cell array

let rec pattern_shadow_state = function
  | PReg reg when reg = state_reg ->
      true
  | PTuple li ->
      List.exists pattern_shadow_state li
  | _ ->
      false


let push _program cells sync _value cell =
  let sync = match sync with Synced n -> Synced (n + 1) | sync -> sync in
  (MArray.push cells cell, sync)


let pop _program cells sync pattern =
  let cells = MArray.pop cells in
  (* If the state is shadowed, then it becomes unknown. *)
  let sync = if pattern_shadow_state pattern then Synced 0 else sync in

  (cells, sync)


let def _program cells sync bindings =
  (* If the state variable is redefined, then matching on the state will
     not give any new information. Therefore, [extra_known_cells]
     becomes empty. However there is an issue : it is possible to trick
     this function into not raising an error when it should. *)
  let sync = Sync.update_with_bindings bindings sync in
  (cells, sync)


let case_tag_branch program _cells sync (TagMultiple _taglist) block =
  let cells =
    match block with
    | ITypedBlock { stack_type } ->
        stack_type
    | IJump label ->
        (lookup label program.cfg).stack_type
    | b ->
        StackLangPrinter.print_block stderr b;
        failwith "cannot find t_block or jump"
  in
  (* We are matching on a state, therefore state is always needed,
     and we can discard these values. *)
  (cells, sync)


let typed_block _program _known_cells _sync { stack_type } =
  let cells = stack_type in
  let sync = Synced 0 in
  (cells, sync)


let jump program _known_cells _sync label =
  let target = lookup label program.cfg in
  (target.stack_type, Synced 0)


module Curry (P : sig
  val program : program

  val cells : cells

  val sync : sync
end) =
struct
  let program, cells, sync = P.(program, cells, sync)

  let push = push program cells sync

  let pop = pop program cells sync

  let def = def program cells sync

  let case_tag_branch = case_tag_branch program cells sync

  let typed_block = typed_block program cells sync

  let jump = jump program cells sync
end
