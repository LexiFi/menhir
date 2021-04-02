open StackLang

type t = program

let map f program = {program with cfg= RegisterMap.map f program.cfg}

let mapi f program = {program with cfg= RegisterMap.mapi f program.cfg}

let iter f program = RegisterMap.iter f program.cfg

let filter pred program = {program with cfg=RegisterMap.filter pred program.cfg}
