open StackLangBasics

(* In the following, we write [bs] for a set of bindings. *)

type t =
  value RegisterMap.t

let empty, is_empty, domain, to_list, fold =
  RegisterMap.(empty, is_empty, domain, bindings, fold)

let rec apply bs v =
  match v with
  | VReg r ->
      (try RegisterMap.find r bs with Not_found -> v)
  | VTuple vs ->
      VTuple (List.map (apply bs) vs)
  | VTag _
  | VUnit
    -> v

(* [add] is an internal function. *)
let add r v bs =
  match v with
  (* We avoid adding identity bindings. This is not essential, but may help
     avoid a certain amount of noise. *)
  | VReg r' when r = r' ->
      bs
  | _ ->
      RegisterMap.add r v bs

let extend bs r v =
  add r (apply bs v) bs

let rec extend_pattern bs pattern value =
  match (pattern, value) with
  | PWildcard, _ ->
      bs
  | PReg reg, value ->
      extend bs reg value
  | PTuple ps, VTuple vs ->
      List.fold_left2 extend_pattern bs ps vs
  | _ ->
      assert false


let compose bs1 bs2 =
  (* We fold on bs2 with an accumulator starting at bs1 *)
  fold (fun reg value bs -> add reg (apply bs1 value) bs) bs2 bs1


let singleton_pattern = extend_pattern empty

let rec remove bs pattern =
  match pattern with
  | PReg reg ->
      RegisterMap.remove reg bs
  | PWildcard ->
      bs
  | PTuple ps ->
      List.fold_left remove bs ps


let remove_registers bs registers =
  RegisterSet.fold RegisterMap.remove registers bs


let rec remove_value bs = function
  | VReg reg ->
      remove bs (PReg reg)
  | VTuple vs ->
      List.fold_left remove_value bs vs
  | _ ->
      bs


let codomain bs =
  fold
    (fun _r value regset -> RegisterSet.union (Value.registers value) regset)
    bs
    RegisterSet.empty


let restrict bs registers =
  RegisterMap.filter (fun reg _ -> RegisterSet.mem reg registers) bs
