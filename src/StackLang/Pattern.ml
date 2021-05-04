open StackLangBasics

type t = pattern

let rec registers = function
  | PWildcard ->
      RegisterSet.empty
  | PReg r ->
      RegisterSet.singleton r
  | PTuple ps ->
      List.fold_left RegisterSet.union rs (List.map registers ps)