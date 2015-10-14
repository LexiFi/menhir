(* This module implements resizable arrays, that is, arrays that can
   grow upon explicit request. *)

type 'a t = {
    (* The default element is used to fill empty slots. *)
    default: 'a;
    (* The logical size of this array. *)
    mutable size: int;
    (* The physical array, whose length is at least [size]. *)
    mutable table: 'a array
  } 

let make capacity default =
  (* [capacity] must be nonzero, so that doubling it actually
     enlarges the array. *)
  assert (capacity >= 0);
  let capacity = if capacity = 0 then 1 else capacity in
  let table = Array.make capacity default in
  { default; size = 0; table }

let length a =
  a.size

let get a i =
  assert (0 <= i && i < a.size);
  a.table.(i)

let set a i x =
  assert (0 <= i && i < a.size);
  a.table.(i) <- x

let resize a s =
  assert (s >= 0);
  if s < a.size then begin
    (* The logical size of the array decreases. *)
    Array.fill a.table s (a.size - s) a.default;
    a.size <- s
  end
  else if s > a.size then begin
    (* The logical size of the array increases. *)
    let n = Array.length a.table in
    if s > n then begin
      (* The physical size of the array must increase. The new size is at
         least double of the previous size, and larger if requested. *)
      let table = Array.make (max (2 * n) s) a.default in
      Array.blit a.table 0 table 0 n;
      a.table <- table
    end;
    a.size <- s
  end

