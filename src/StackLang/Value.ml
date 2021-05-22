open StackLangBasics
open RegisterSet

module Accu = struct

  let rec registers accu v =
    match v with
    | VTag _
    | VUnit
      -> accu
    | VReg r ->
        add r accu
    | VTuple vs ->
        registers_of_list accu vs

  and registers_of_list accu vs =
    List.fold_left registers accu vs

end

let registers v =
  Accu.registers empty v

let registers_of_list vs =
  Accu.registers_of_list empty vs
