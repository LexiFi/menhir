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


let push _program known_cells sync _value cell =
  let sync = match sync with Synced n -> Synced (n + 1) | sync -> sync in
  (MArray.push known_cells cell, sync)


let pop _program known_cells sync pattern =
  let known_cells = MArray.pop known_cells in
  (* If the state is shadowed, then it becomes unknown. *)
  let sync = if pattern_shadow_state pattern then Synced 0 else sync in

  (known_cells, sync)


let def _program known_cells sync bindings =
  (* If the state variable is redefined, then matching on the state will
     not give any new information. Therefore, [extra_known_cells]
     becomes empty. However there is an issue : it is possible to trick
     this function into not raising an error when it should. *)
  let sync = Sync.update_with_bindings bindings sync in
  (known_cells, sync)


let case_tag program known_cells sync _reg branches =
  match sync with
  | Synced n ->
      let branch_aux (TagMultiple taglist, _block) =
        (* By matching on the state, we discover state information.
           We can enrich the known cells with this information. *)
        let known_cells =
          Array.append
            (state_info_intersection program.states taglist).known_cells
            (MArray.suffix known_cells n)
        in
        (* We are matching on a state, therefore state is always needed,
           and we can discard these values. *)
        (known_cells, sync)
      in
      List.map branch_aux branches
  | Unsynced _tag ->
      assert false


let typed_block _program _known_cells _sync { stack_type } =
  let known_cells = stack_type in
  let sync = Synced 0 in
  (known_cells, sync)

let jump program _known_cells _sync label =
  let target = lookup label program.cfg in
  (target.stack_type, Synced 0)


module Curry (P : sig
  val program : program
end) =
struct
  let program = P.program

  let push = push program

  let pop = pop program

  let def = def program

  let case_tag = case_tag program

  let typed_block = typed_block program

  let jump = jump program
end
