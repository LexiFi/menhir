open StackLangBasics

type t = value

let rec registers = function
  | VReg reg ->
      RegisterSet.singleton reg
  | VTuple li ->
      List.fold_left RegisterSet.union RegisterSet.empty (List.map registers li)
  | _ ->
      RegisterSet.empty


let registers_of_list values =
  List.fold_left
    (fun regs v ->
      let regs' = registers v in
      RegisterSet.union regs regs' )
    RegisterSet.empty
    values
