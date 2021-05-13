open StackLangBasics

type t = value

let rec registers = function
  | VReg reg ->
      RegisterSet.singleton reg
  | VTuple li ->
      List.fold_left RegisterSet.union RegisterSet.empty (List.map registers li)
  | _ ->
      RegisterSet.empty
