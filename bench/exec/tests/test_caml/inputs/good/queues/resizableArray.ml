(* $Header: /home/yquem/cristal/fpottier/cvs/queues/resizableArray.ml,v 1.1 2003/04/16 09:05:48 fpottier Exp $ *)

(* This module implements resizable arrays. *)

type 'a t = {
    default: 'a;
    mutable size: int;
    mutable table: 'a array
  } 

let default_size =
  16 (* must be non-zero *)

let make x = {
  default = x;
  size = 0;
  table = Array.make default_size x
} 

let size a =
  a.size

let rec new_length length i =
  if i <= length then
    length
  else
    new_length (2 * length) i

let resize a i =
  assert (i >= 0);
  let table = a.table in
  let length = Array.length table in
  if i < length then begin
    for j = i to length - 1 do
      a.table.(j) <- a.default
    done;
    a.size <- i
  end
  else if i > length then begin
    let table' = Array.make (new_length (2 * length) i) a.default in
    Array.blit table 0 table' 0 length;
    a.table <- table';
    a.size <- i
  end

let get a i =
  assert (i >= 0 && i < a.size);
  a.table.(i)

let set a i x =
  assert (i >= 0 && i < a.size);
  a.table.(i) <- x

