type diff = 
  | Correct of int
  | Incorrect of string

let htable = Hashtbl.create 1000

let add generatedgroup input diff = 
  Hashtbl.add htable (generatedgroup,input) diff
;;

