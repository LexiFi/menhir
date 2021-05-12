open StackLangBasics

type t = value RegisterMap.t

let empty = RegisterMap.empty

let singleton register value = RegisterMap.singleton register value

let is_empty = RegisterMap.is_empty

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
  match value with
  (* We avoid adding identity bindings. *)
  | VReg reg' when reg = reg' ->
      map
  | _ ->
      RegisterMap.add reg value map


let fold = RegisterMap.fold

let extend map reg value = add reg (apply map value) map

let rec extend_pattern map pattern value =
  match (pattern, value) with
  | PWildcard, _ ->
      map
  | PReg reg, value ->
      extend map reg value
  | PTuple pli, VTuple vli ->
      List.fold_left2 extend_pattern map pli vli
  | _ ->
      assert false


let compose b1 b2 =
  (* We fold on b2 with an accumulator starting at b1 *)
  fold (fun reg value b -> add reg (apply b1 value) b) b2 b1


let singleton_pattern = extend_pattern empty

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


let to_list bds = fold (fun reg value acc -> (reg, value) :: acc) bds []
