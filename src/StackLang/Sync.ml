open StackLang

let state_reg = NamingConventions.state

type sync =
  | Synced of int
  | Unsynced of tag


let update_with_bindings bindings sync =
  match Bindings.apply bindings (VReg state_reg) with
  | VTag tag ->
      Unsynced tag
  | VReg reg when reg = state_reg ->
      sync
  | _ ->
      assert false