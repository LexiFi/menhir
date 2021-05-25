open StackLang
open Infix

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


let cells_intersection cells1 cells2 =
  let len1 = Array.length cells1 in
  let len2 = Array.length cells2 in
  (* let cells1 = Array.rev cells1 in
     let cells2 = Array.rev cells2 in *)
  let i = ref 0 in
  while
    let i = !i in
    i < len1
    && i < len2
    && Invariant.similar cells1.(len1 - i - 1) cells2.(len2 - i - 1)
  do
    i += 1
  done;
  let i = !i in
  let cells1 = MArray.suffix cells1 i in
  let cells2 = MArray.suffix cells2 i in
  Array.map2 Invariant.cell_intersection cells1 cells2


let state_info_intersection s1 s2 =
  let { known_cells = cells1; sfinal_type = f1 } = s1 in
  let { known_cells = cells2; sfinal_type = f2 } = s2 in
  let sfinal_type =
    match (f1, f2) with Some f1, Some f2 when f1 = f2 -> Some f1 | _ -> None
  in
  let known_cells = cells_intersection cells1 cells2 in
  { known_cells; sfinal_type }


let state_info_list_intersection state_infos =
  match state_infos with
  | state_info :: state_infos ->
      List.fold_left state_info_intersection state_info state_infos
  | [] ->
      assert false


let state_info_intersection states taglist =
  state_info_list_intersection
    (List.map (fun tag -> lookup_tag tag states) taglist)


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

let test_cells_intersection () =
  (* check that [cells_intersection a a = a] *)
  assert (
    cells_intersection
      [| cell true false false false
       ; cell false false true false
       ; empty_cell
       ; cell false false true true
      |]
      [| cell true false false false
       ; cell false false true false
       ; empty_cell
       ; cell false false true true
      |]
    = [| cell true false false false
       ; cell false false true false
       ; empty_cell
       ; cell false false true true
      |] );
  (* check that if [a] is a suffix of [b] then [cells_intersection a b = a]*)
  assert (
    cells_intersection
      [| cell true false false false
       ; cell false false true false
       ; empty_cell
       ; cell false false true true
      |]
      [| empty_cell; cell false false true true |]
    = [| empty_cell; cell false false true true |] );
  (* check that if the biggest common suffix of [a] and [b] is [ [||] ] then,
     [cells_intersection a b = [||]]*)
  assert (
    cells_intersection
      [| cell true false false false
       ; cell false false true false
       ; empty_cell
       ; cell false false true true
       ; cell false false true true
      |]
      [| cell true false false false
       ; cell false false true false
       ; empty_cell
       ; cell false false true true
       ; cell true true false false
      |]
    = [||] )


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
  test_filter_stack ();
  test_cells_intersection ()
