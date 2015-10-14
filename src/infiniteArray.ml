(** This module implements infinite arrays, that is, arrays that grow
    transparently upon demand. *)

type 'a t = {
    default: 'a;
    mutable table: 'a array;
    mutable extent: int; (* the index of the greatest [set] ever, plus one *)
  } 

let default_size =
  16384 (* must be non-zero *)

let make x = {
  default = x;
  table = Array.make default_size x;
  extent = 0;
} 

let rec new_length length i =
  if i < length then
    length
  else
    new_length (2 * length) i

let ensure a i =
  assert (0 <= i);
  let table = a.table in
  let length = Array.length table in
  if i >= length then begin
    let table' = Array.make (new_length (2 * length) i) a.default in
    Array.blit table 0 table' 0 length;
    a.table <- table'
  end

let get a i =
  ensure a i;
  Array.unsafe_get a.table (i)

let set a i x =
  ensure a i;
  Array.unsafe_set a.table (i) x;
  if a.extent <= i then
    a.extent <- i + 1

let update a i f =
  ensure a i;
  let x = a.table.(i) in
  Array.unsafe_set a.table (i) (f x);
  if a.extent <= i then
    a.extent <- i + 1

let extent a =
  a.extent

let domain a =
  Array.sub a.table 0 a.extent

