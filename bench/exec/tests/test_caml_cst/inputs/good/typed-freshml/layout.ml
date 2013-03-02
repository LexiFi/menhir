open Typed

let rec iter f tuple =
  match tuple with
  | SComponent (_, _, component) ->
      f component
  | SInner tuple
  | SOuter tuple
  | SAbstraction tuple ->
      iter f tuple
  | STuple tuples ->
      List.iter (iter f) tuples

let rec fold f tuple accu =
  match tuple with
  | SComponent (_, _, component) ->
      f component accu
  | SInner tuple
  | SOuter tuple
  | SAbstraction tuple ->
      fold f tuple accu
  | STuple tuples ->
      List.fold_right (fold f) tuples accu

let zip tuple =
  let rec zip accu tuple =
    match tuple with
    | SComponent (Some x, _, v) ->
	Source.Var.AtomMap.add x v accu
    | SComponent (None, _, _) ->
	accu
    | SInner tuple
    | SOuter tuple
    | SAbstraction tuple ->
	zip accu tuple
    | STuple tuples ->
	List.fold_left zip accu tuples
  in
  zip Source.Var.AtomMap.empty tuple

let rec map f tuple =
  match tuple with
  | SComponent (x, typ, component) ->
      SComponent (x, typ, f component)
  | SInner tuple ->
      SInner (map f tuple)
  | SOuter tuple ->
      SOuter (map f tuple)
  | SAbstraction tuple ->
      SAbstraction (map f tuple)
  | STuple tuples ->
      STuple (List.map (map f) tuples)

let farm empty join f tuple =
  let rec farm tuple =
    match tuple with
    | SComponent (x, typ, component) ->
	let accu, component = f component in
	accu, SComponent (x, typ, component)
    | SInner tuple ->
	let accu, tuple = farm tuple in
	accu, SInner tuple
    | SOuter tuple ->
	let accu, tuple = farm tuple in
	accu, SOuter tuple
    | SAbstraction tuple ->
	let accu, tuple = farm tuple in
	accu, SAbstraction tuple
    | STuple tuples ->
	let accu, tuples =
	  List.fold_right (fun tuple (accus, tuples) ->
	    let accu, tuple = farm tuple in
	    join accu accus, tuple :: tuples
	  ) tuples (empty, [])
	in
	accu, STuple tuples
  in
  farm tuple

let rec fold2 f tuple1 tuple2 accu =
  match tuple1, tuple2 with
  | SComponent (_, _, component1), SComponent (_, _, component2) ->
      f component1 component2 accu
  | SInner tuple1, SInner tuple2
  | SOuter tuple1, SOuter tuple2
  | SAbstraction tuple1, SAbstraction tuple2 ->
      fold2 f tuple1 tuple2 accu
  | STuple tuple1s, STuple tuple2s ->
      List.fold_right2 (fold2 f) tuple1s tuple2s accu
  | _, _ ->
      assert false

let rec fold2_except_under_abstractions f tuple1 tuple2 accu =
  match tuple1, tuple2 with
  | SComponent (_, _, component1), SComponent (_, _, component2) ->
      f component1 component2 accu
  | SInner tuple1, SInner tuple2
  | SOuter tuple1, SOuter tuple2 ->
      fold2 f tuple1 tuple2 accu
  | SAbstraction _, SAbstraction _ ->
      accu
  | STuple tuple1s, STuple tuple2s ->
      List.fold_right2 (fold2 f) tuple1s tuple2s accu
  | _, _ ->
      assert false

