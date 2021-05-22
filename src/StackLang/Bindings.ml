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

(* [add] is an internal function; it is used in the definition of [assign]
   and [compose]. *)

let add r v bs =
  match v with
  (* We drop identity bindings. This is not essential, but may help
     avoid a certain amount of noise. *)
  | VReg r' when r = r' ->
      bs
  | _ ->
      RegisterMap.add r v bs

(* [assign] with an accumulator. *)

let rec assign bs p v =
  match p, v with
  | PWildcard, _ ->
      bs
  | PReg r, v ->
      add r v bs
  | PTuple ps, VTuple vs ->
      assert (List.length ps = List.length vs);
      List.fold_left2 assign bs ps vs
  | _ ->
      assert false

(* [assign] without an accumulator. We assume that no register is assigned
   twice by the pattern [p]. *)

let assign p v =
  assign empty p v

(* To compute the sequential composition of [bs1] and [bs2], we must apply
   [bs1] to each of the values in the codomain of [bs2]. We can then add
   each binding in [bs2] to [bs1], possibly overriding previous bindings. *)

let compose bs1 bs2 =
  (* We fold on [bs2] with an accumulator whose initial value is [bs1]. *)
  fold (fun r v bs -> add r (apply bs1 v) bs) bs2 bs1

let remove bs rs =
  RegisterSet.fold RegisterMap.remove rs bs

let codomain bs =
  fold
    (fun _r value regset -> RegisterSet.union (Value.registers value) regset)
    bs
    RegisterSet.empty


let restrict bs registers =
  RegisterMap.filter (fun reg _ -> RegisterSet.mem reg registers) bs
