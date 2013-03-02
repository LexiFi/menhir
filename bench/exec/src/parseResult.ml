(* TODO fix too much htable lookups *)

open BenchTypes;;

type t =
  {
    mutable return : return;
    mutable time : float option;
    mutable tracereturn : return;
    mutable trace : Traces.t option;
  }

let htable = Hashtbl.create 1000

let build generated input =
  let result =
    {
      return      = Error;
      time        = None;
      tracereturn = Error;
      trace       = None;
    } in
  assert (not (Hashtbl.mem htable (generated, input)));
  Hashtbl.add htable (generated, input) result;
  result
;;

let set_time generated input return time =
  let key = (generated, input) in
  let pres =
    if (Hashtbl.mem htable key) then
      Hashtbl.find htable key
    else
      build generated input in
  pres.return <- return;
  pres.time <- time
;;

let set_trace generated input return trace =
  let key = (generated, input) in
  let pres =
    if (Hashtbl.mem htable key) then
      Hashtbl.find htable key
    else
      build generated input in
  pres.tracereturn <- return;
  pres.trace <- trace
;;

let get_result generated input =
  let pres = Hashtbl.find htable (generated, input) in
  pres.return
;;

let get_time generated input =
  let pres = Hashtbl.find htable (generated, input) in
  pres.time
;;

let get_trace generated input =
  let pres = Hashtbl.find htable (generated, input) in
  pres.trace
;;
