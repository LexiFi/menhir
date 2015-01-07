type 'a t =
  (* data: *)   'a array *
  (* entry: *) int array

let make (a : 'a array array) : 'a t =
  let n = Array.length a in
  (* Build the entry array. *)
  let size = ref 0 in
  let entry = Array.init n (fun i ->
    let s = !size in
    size := s + Array.length a.(i);
    s
  ) in
  (* Build the data array. *)
  let i = ref 0
  and j = ref 0 in
  let data = Array.init !size (fun _ ->
    while !j = Array.length a.(!i) do
      i := !i + 1;
      j := 0;
    done;
    let x = a.(!i).(!j) in
    j := !j + 1;
    x
  ) in
  data, entry

let length ((_, entry) : 'a t) : int =
  Array.length entry

(* This auxiliary function conceptually extends the array [entry]
   to the case where [i] is [n]. *)
let _entry (data, entry) i =
  let n = Array.length entry in
  assert (0 <= i && i <= n);
  if i < n then
    entry.(i)
  else
    Array.length data

let row_length ((_, entry) as la : 'a t) i : int =
  _entry la (i + 1) - entry.(i)

let read ((data, entry) as la : 'a t) i j : 'a =
  assert (0 <= j && j < row_length la i);
  data.(entry.(i) + j)

let write ((data, entry) as la : 'a t) i j (v : 'a) : unit =
  assert (0 <= j && j < row_length la i);
  data.(entry.(i) + j) <- v

let rec read_interval data i j =
  if i = j then
    []
  else
    data.(i) :: read_interval data (i + 1) j

let read_row ((data, entry) as la : 'a t) i : 'a list =
  read_interval data entry.(i) (_entry la (i + 1))

