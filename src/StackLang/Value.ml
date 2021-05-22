open StackLangBasics
open RegisterSet

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

let registers v =
  registers empty v

let registers_of_list vs =
  registers_of_list empty vs
