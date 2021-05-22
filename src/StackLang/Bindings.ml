open StackLangBasics

(* In the following, we write [b] for a set of bindings. *)

type t =
  value RegisterMap.t

let empty, singleton, is_empty, domain, to_list, fold =
  RegisterMap.(empty, singleton, is_empty, domain, bindings, fold)

let rec apply b = function
  | VReg register ->
      Option.value
        (RegisterMap.find_opt register b)
        ~default:(VReg register)
  | VTuple li ->
      VTuple (List.map (apply b) li)
  | v ->
      v


let add reg value b =
  match value with
  (* We avoid adding identity bindings. *)
  | VReg reg' when reg = reg' ->
      b
  | _ ->
      RegisterMap.add reg value b


let extend b reg value = add reg (apply b value) b

let rec extend_pattern b pattern value =
  match (pattern, value) with
  | PWildcard, _ ->
      b
  | PReg reg, value ->
      extend b reg value
  | PTuple pli, VTuple vli ->
      List.fold_left2 extend_pattern b pli vli
  | _ ->
      assert false


let compose b1 b2 =
  (* We fold on b2 with an accumulator starting at b1 *)
  fold (fun reg value b -> add reg (apply b1 value) b) b2 b1


let singleton_pattern = extend_pattern empty

let rec remove b pattern =
  match pattern with
  | PReg reg ->
      RegisterMap.remove reg b
  | PWildcard ->
      b
  | PTuple li ->
      List.fold_left remove b li


let remove_registers b registers =
  RegisterSet.fold RegisterMap.remove registers b


let rec remove_value b = function
  | VReg reg ->
      remove b (PReg reg)
  | VTuple li ->
      List.fold_left remove_value b li
  | _ ->
      b


let codomain bindings =
  fold
    (fun _r value regset -> RegisterSet.union (Value.registers value) regset)
    bindings
    RegisterSet.empty


let restrict bindings registers =
  RegisterMap.filter (fun reg _ -> RegisterSet.mem reg registers) bindings
