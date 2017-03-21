open Positions
open Syntax

let new_precedence_level =
  let c = ref 0 in
  fun pos1 pos2 ->
    incr c;
    PrecedenceLevel (InputFile.get_input_file (), !c, pos1, pos2)

let new_production_level =
  let c = ref 0 in
  fun () ->
    incr c;
    ProductionLevel (InputFile.get_input_file (), !c)

let new_on_error_reduce_level =
  new_production_level
    (* the counter is shared with [new_production_level],
       but this is irrelevant *)

module IdSet = Set.Make (struct
  type t = identifier located
  let compare id1 id2 =
    compare (value id1) (value id2)
end)

let defined_identifiers (_, ido, _) accu =
  Option.fold IdSet.add ido accu

let defined_identifiers producers =
  List.fold_right defined_identifiers producers IdSet.empty

let check_production_group right_hand_sides =
  match right_hand_sides with
  | [] ->
      (* A production group cannot be empty. *)
      assert false
  | (producers, _, _, _) :: right_hand_sides ->
      let ids = defined_identifiers producers in
      List.iter (fun (producers, _, _, _) ->
        let ids' = defined_identifiers producers in
        try
          let id =
            IdSet.choose (IdSet.union
                                (IdSet.diff ids ids')
                                (IdSet.diff ids' ids))
          in
          Error.error [Positions.position id]
            "two productions that share a semantic action must define exactly\n\
             the same identifiers. Here, \"%s\" is defined\n\
             in one production, but not in all of them."
            (Positions.value id)
        with Not_found ->
          ()
      ) right_hand_sides

(* [normalize_producer i p] assigns a name of the form [_i]
   to the unnamed producer [p]. *)
let normalize_producer i (pos, opt_identifier, parameter) =
  let id =
    match opt_identifier with
      | Some id -> id
      | None -> Positions.with_pos pos ("_" ^ string_of_int (i + 1))
  in
  (id, parameter)

let normalize_producers producers =
  List.mapi normalize_producer producers

let override pos o1 o2 =
  match o1, o2 with
  | Some _, Some _ ->
      Error.signal [ pos ] "this production carries two %%prec declarations.";
      o2
  | None, Some _ ->
      o2
  | _, None ->
      o1

(* Only unnamed producers can be referred to using positional identifiers.
   Besides, such positions must be taken in the interval [1
   .. List.length producers]. The output array [p] is such that
   [p.(idx) = Some x] if [idx] must be referred to using [x], not
   [$(idx + 1)]. *)
let producer_names producers =
  producers
  |> List.map (fun (_, oid, _) -> Option.map Positions.value oid)
  |> Array.of_list
