open StackLang

type t = primitive

let action ?(bindings = Bindings.empty) action =
  PrimOCamlAction (bindings, action)

let registers = function
  | PrimOCamlAction (bindings, action) ->
      Bindings.apply_registers bindings
      @@ RegisterSet.union (Action.posvars action) (Action.semvars action)
  | PrimOCamlCall (_f, values) ->
      List.fold_left RegisterSet.union RegisterSet.empty
        (List.map Value.registers values)
  | PrimOCamlDummyPos ->
      RegisterSet.empty
  | PrimOCamlFieldAccess (value, _fields) ->
      Value.registers value
