open StackLangBasics
open RegisterSet

let rec registers accu p =
  match p with
  | PWildcard ->
      accu
  | PReg r ->
      add r accu
  | PTuple ps ->
      List.fold_left registers accu ps

let registers p =
  registers empty p
