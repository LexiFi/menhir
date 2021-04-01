open StackLang

type t = program

let map f program = {program with cfg= RegisterMap.map f program.cfg}

let iter f program = RegisterMap.iter (f) program.cfg