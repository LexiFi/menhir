
module ElementSet = Set.Make(String)

module DateMap =
struct include (* TEMPORARY above et below peuvent être implantés plus efficacement dans le module Map *)
Map.Make (struct
  type t = float
  let compare (x : float) y = if x < y then -1 else if x = y then 0 else 1
end)
let above (key : float) map =
  fold (fun key' data accu ->
    if key' >= key then
      add key' data accu
    else
      accu
  ) map empty
let below (key : float) map =
  fold (fun key' data accu ->
    if key' <= key then
      add key' data accu
    else
      accu
  ) map empty
end

let set =
  ref ElementSet.empty

let map : string list DateMap.t ref =
  ref DateMap.empty

open Database

let add element =
  set := ElementSet.add element !set;
  let date = elementdate element in
  let elements = try
    DateMap.find date !map
  with Not_found ->
    [] in
  map := DateMap.add date (element :: elements) !map

let remove date element =
  let date = match date with
  | Some date ->
      date
  | None ->
      elementdate element in
  suppress element;
  set := ElementSet.remove element !set;
  let elements = DateMap.find date !map in
  let elements = List.filter ((<>) element) elements in
  map := DateMap.add date elements !map

let sync () =
  
  (* Check that every element in the cache still exists in the file system.
     If not, suppress it. *)

  DateMap.iter (fun date elements ->
    List.iter (fun element ->
      if not (Sys.file_exists (original element)) then
	remove (Some date) element
    ) elements
  ) !map;

  (* Conversely, check that every element in the file system is known to
     the cache. *)

  List.iter (fun element ->
    if not (ElementSet.mem element !set) then
      add element
  ) (all_elements())

let save () =
  let c = open_out_bin Config.cache in
  output_value c !set;
  output_value c !map;
  close_out c

let restore () =
  if Sys.file_exists Config.cache then
    let c = open_in_bin Config.cache in
    set := Obj.magic (input_value c);
    map := Obj.magic (input_value c);
    close_in c

let _ =
  restore();
  sync();
  at_exit save

