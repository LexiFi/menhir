open StackLangBasics

type t = value RegisterMap.t

let empty = RegisterMap.empty

let singleton register value = RegisterMap.singleton register value

let rec apply substitution = function
  | VReg register ->
      Option.value
        (RegisterMap.find_opt register substitution)
        ~default:(VReg register)
  | VTuple li ->
      VTuple (List.map (apply substitution) li)
  | v ->
      v


let add reg value map =
  (*RegisterMap.add register value map*)
  match value with
  | VReg reg' when reg = reg' ->
      map
  | _ ->
      RegisterMap.add reg value map


let fold = RegisterMap.fold

let extend reg value map = add reg (apply map value) map

let rec extend_pattern map pattern value =
  match (pattern, value) with
  | PWildcard, _ ->
      map
  | PReg reg, value ->
      extend reg value map
  | PTuple pli, VTuple vli ->
      List.fold_left2 extend_pattern map pli vli
  | _ ->
      assert false


let compose s1 s2 = fold (fun reg value s -> extend reg value s) s2 s1

let simple = extend_pattern empty

let rec remove substitution pattern =
  match pattern with
  | PReg reg ->
      RegisterMap.remove reg substitution
  | PWildcard ->
      substitution
  | PTuple li ->
      List.fold_left remove substitution li


let remove_registers substitution registers =
  RegisterSet.fold RegisterMap.remove registers substitution


let rec remove_value substitution = function
  | VReg reg ->
      remove substitution (PReg reg)
  | VTuple li ->
      List.fold_left remove_value substitution li
  | _ ->
      substitution


let rec apply_pattern substitution = function
  | PReg register ->
    ( match RegisterMap.find_opt register substitution with
    | Some (VReg reg) ->
        PReg reg
    | Some _ ->
        failwith "Substitution : could not transform value into pattern"
    | None ->
        PReg register )
  | PTuple li ->
      PTuple (List.map (apply_pattern substitution) li)
  | v ->
      v


let apply_reg substitution reg =
  match RegisterMap.find_opt reg substitution with
  | None ->
      reg
  | Some (VReg reg) ->
      reg
  | Some _ ->
      raise (Invalid_argument "apply_reg")


let apply_registers substitution (registers : registers) =
  let rec add_value set = function
    | VUnit | VTag _ ->
        set
    | VReg reg ->
        RegisterSet.add reg set
    | VTuple li ->
        List.fold_left add_value set li
  in
  RegisterSet.fold
    (fun reg acc ->
      let v = RegisterMap.find_opt reg substitution in
      match v with None -> RegisterSet.add reg acc | Some v -> add_value acc v
      )
    registers
    RegisterSet.empty


let domain = RegisterMap.domain

let values bindings = fold (fun _ value li -> value :: li) bindings []

let codomain bindings =
  let values = values bindings in
  List.fold_left
    (fun regset value -> RegisterSet.union (Value.registers value) regset)
    RegisterSet.empty
    values


let restrict bindings registers =
  RegisterMap.filter (fun reg _ -> RegisterSet.mem reg registers) bindings
