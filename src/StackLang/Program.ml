open StackLang

type t = program

let map f program = { program with cfg = LabelMap.map f program.cfg }

let mapi f program = { program with cfg = LabelMap.mapi f program.cfg }

let iter f program = LabelMap.iter f program.cfg

let filter pred program =
  { program with cfg = LabelMap.filter pred program.cfg }
