open StackLang
open StackLangUtils

module Variable = Fix.Glue.HashTablesAsImperativeMaps (struct
  include String

  let hash = Hashtbl.hash
end)

module Property = Fix.Prop.Set (RegisterSet)

open Fix.Make (Variable) (Property)

let ( + ) = RegisterSet.union

let ( - ) = RegisterSet.diff

let ( -^ ) regs reg = RegisterSet.remove reg regs

let ( +^ ) regs reg = RegisterSet.add reg regs

let empty = RegisterSet.empty

let unions = List.fold_left ( + ) empty

let defined_tokpat = function
  | TokSingle (_, reg) ->
      RegisterSet.singleton reg
  | TokMultiple _ ->
      RegisterSet.empty


(** This function computes the registers required by a block according to a
    valuation that maps labels to needed registers *)
let rec needed_block block valuation =
  let needed_block block = needed_block block valuation in
  match block with
  | IDef (bindings, block) ->
      let codomain = Bindings.codomain bindings in
      let domain = Bindings.domain bindings in
      codomain + (needed_block block - domain)
  | IPush (value, _, block) ->
      Value.registers value + needed_block block
  | IPop (pattern, block) ->
      needed_block block - Pattern.registers pattern
  | IPrim (reg, prim, block) ->
      Primitive.registers prim + (needed_block block -^ reg)
  | IReturn value ->
      Value.registers value
  | IDie ->
      empty
  | ITrace (register, block) ->
      needed_block block +^ register
  | IComment (_, block) ->
      needed_block block
  | ICaseTag (register, branches) ->
      (unions @@ List.map (branch_iter needed_block) branches) +^ register
  | ICaseToken (register, branches, default) ->
      unions
        (List.map
           (fun (tokpat, block) -> needed_block block - defined_tokpat tokpat)
           branches )
      + Option.value ~default:empty (Option.map needed_block default)
      +^ register
  | IJump label ->
      valuation label
  | ITypedBlock _ ->
      assert false


let equations program label =
  let routine = lookup label program.cfg in
  let block = routine.block in
  needed_block block


let needed program = lfp (equations program)

let update program =
  let needed = needed program in
  Program.mapi
    (fun label routine -> { routine with needed_registers = needed label })
    program
