open StackLang

type t = primitive

let action ?(bindings = Bindings.empty) action =
  PrimOCamlAction (bindings, action)


let registers = function
  | PrimOCamlAction (bindings, action) ->
      RegisterSet.union
        (RegisterSet.diff
           (RegisterSet.union (Action.posvars action) (Action.semvars action))
           (Bindings.codomain bindings) )
        (Bindings.domain bindings)
  | PrimOCamlCall (_f, values) ->
      Value.registers_of_list values
  | PrimOCamlDummyPos ->
      RegisterSet.empty
  | PrimOCamlFieldAccess (value, _fields) ->
      Value.registers value
