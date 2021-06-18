open StackLang

(* -------------------------------------------------------------------------- *)

(* A few constructors. *)

let vreg r = VReg r

let vregs rs = List.map vreg rs

(* A few accessors. *)

let lookup label map =
  try LabelMap.find label map with Not_found -> assert false


let lookup_tag tag states = TagMap.find tag states

let lookup_state state states = lookup_tag (tag_of_node state) states

let entry_labels program = StringMap.domain program.entry

(* We assume that every labeled block in a well-formed control flow graph
   begins with an [INeed] instruction that determines which registers are
   defined upon entry to this block. *)

let needed t_block = t_block.needed_registers

let branch_iter f (_pat, block) = f block

let branch_map f (pat, block) = (pat, f block)

let rec value_refers_to_register register value =
  match value with
  | VReg register' when register = register' ->
      true
  | VTuple li ->
      List.exists (value_refers_to_register register) li
  | _ ->
      false


let cells_similar cs1 cs2 =
  assert (Array.(length cs1 = length cs2));
  Array.for_all2 Invariant.similar cs1 cs2


let is_compatible cs1 cs2 =
  MArray.(
    let n1 = length cs1
    and n2 = length cs2 in
    let short, long = if n1 < n2 then (cs1, cs2) else (cs2, cs1) in
    let short_n = min n1 n2 in
    let new_long = suffix long short_n in
    cells_similar short new_long)


let final_type_intersection f1 f2 =
  match (f1, f2) with Some f1, Some f2 when f1 = f2 -> Some f1 | _ -> None


let final_type_intersection infos =
  match infos with
  | info :: infos ->
      List.fold_left final_type_intersection info infos
  | [] ->
      assert false


let final_type_intersection states taglist =
  final_type_intersection
    (List.map (fun tag -> (lookup_tag tag states).sfinal_type) taglist)


let filter_stack stack =
  Array.of_list
    (List.filter
       (function
         | Invariant.
             { holds_state = false
             ; holds_semv = false
             ; holds_startp = false
             ; holds_endp = false
             } ->
             false
         | _ ->
             true )
       (Array.to_list stack) )


let longest_known_cells stack_types =
  List.hd
  @@ List.sort
       (fun s1 s2 -> -compare (Array.length s1) (Array.length s2))
       stack_types


let rec is_pattern_equivalent_to_value pattern value =
  match (pattern, value) with
  | PWildcard, _ ->
      true
  | PReg regp, VReg regv when regp = regv ->
      true
  | PTuple li_pat, VTuple li_val when List.length li_pat = List.length li_val ->
      List.for_all2 is_pattern_equivalent_to_value li_pat li_val
  | _, _ ->
      false


(* -------------------------------------------------------------------------- *)
(* Testing *)

let t1 = tag_of_int 1

let test_is_pattern_equivalent_to_value () =
  assert (
    is_pattern_equivalent_to_value
      PWildcard
      (VTuple [ VTag t1; VReg "a"; VTuple [ VTag t1; VReg "b" ] ]) );
  assert (
    is_pattern_equivalent_to_value
      (PTuple [ PWildcard; PReg "a"; PTuple [ PWildcard; PReg "b" ] ])
      (VTuple [ VTag t1; VReg "a"; VTuple [ VTag t1; VReg "b" ] ]) );
  assert (
    not
    @@ is_pattern_equivalent_to_value
         (PTuple [ PWildcard; PReg "a"; PTuple [ PReg "c"; PReg "b" ] ])
         (VTuple [ VTag t1; VReg "a"; VTuple [ VTag t1; VReg "b" ] ]) );
  assert (
    not
    @@ is_pattern_equivalent_to_value
         (PTuple [ PWildcard; PReg "a"; PTuple [ PReg "b" ] ])
         (VTuple [ VTag t1; VReg "a"; VTuple [ VTag t1; VReg "b" ] ]) )


let cell = Invariant.dummy_cell

let empty_cell = Invariant.empty_cell

let test_filter_stack () =
  assert (filter_stack [| empty_cell |] = [||]);
  assert (filter_stack [| empty_cell; empty_cell; empty_cell |] = [||]);
  assert (
    filter_stack
      [| cell true false false false
       ; cell false false true false
       ; empty_cell
       ; cell false false true true
      |]
    = [| cell true false false false
       ; cell false false true false
       ; cell false false true true
      |] )


let test () =
  test_is_pattern_equivalent_to_value ();
  test_filter_stack ()
