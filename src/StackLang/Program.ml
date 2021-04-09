open StackLang

type t = program

let map f program = {program with cfg= RegisterMap.map f program.cfg}

let mapi f program = {program with cfg= RegisterMap.mapi f program.cfg}

let iter f program = RegisterMap.iter f program.cfg

let filter pred program = {program with cfg= RegisterMap.filter pred program.cfg}

(*
let f : type tail. tail -> tail state -> _ = fun stack state ->
  match state with
  | S0 ->
      let (b, stack) = stack in (* POP B *)
      let (a, stack) = stack in (* POP A *)
      ( ... )
  | S1 ->
      let (c, stack) = stack in (* POP C *)
      ( ... )
  | S2 -> ( ... )

let g : type tail. (a * tail) -> _ = fun stack ->
  let state = S0 in
  let b : b = (...) in
  let stack = (b, stack) in
  f stack state
*)
