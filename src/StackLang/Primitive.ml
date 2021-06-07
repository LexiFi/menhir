open StackLang

type t = primitive

let action ?(bindings = Bindings.empty) action =
  PrimOCamlAction (bindings, action)


let registers = function
  | PrimOCamlAction (bindings, action) ->
      RegisterSet.(
        let registers = diff (Action.vars action) (Bindings.domain bindings) in
        union registers (Bindings.codomain bindings))
  | PrimOCamlCall (_f, values) ->
      Value.registers_of_list values
  | PrimOCamlDummyPos ->
      RegisterSet.empty
  | PrimOCamlFieldAccess (value, _fields) ->
      Value.registers value
