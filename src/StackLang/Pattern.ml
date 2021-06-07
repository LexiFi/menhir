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

let rec restrict registers p =
  match p with
  | PWildcard ->
      PWildcard
  | PReg r when not @@ RegisterSet.mem r registers ->
      PWildcard
  | PReg r ->
      PReg r
  | PTuple ps ->
      let ps = List.map (restrict registers) ps in
      (* we would like to simplify [PTuple [_; _; _]] to [_], but we cannot,
         because [Pop PTuple [_; _; _]] is compiled as ocaml code
         [let (stack, _, _, _) = stack in]. Therefore the number of wildcards in
         a PTuple is meaningful information, and we cannot lose it. *)
      PTuple ps
