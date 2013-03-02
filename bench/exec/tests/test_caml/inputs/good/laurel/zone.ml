(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/zone.ml,v 1.9 2000/03/02 11:07:26 fpottier Exp $ *)

(* This module defines zones. These objects define an immutable zone in a larger string. They come equipped with a
   mutable ``read head'', which allows easily reading a sequence of elements from a zone. *)

type t = {

    (* The (supposedly immutable) area where the data is stored. *)

    string: string;
    
    (* The zone start offset, comprised between 0 and [string]'s length, inclusive. *)

    start: int;

    (* The zone's length, comprised between 0 and [string]'s length minus [start], inclusive. *)

    length: int;

    (* The zone's read head position, comprised between 0 and [length], inclusive. *)

    mutable head: int

  } 

(* This function creates a zone out of a string. The zone spans the whole string. Its read head is initially located
   at the beginning of the zone. *)

let create string = {
  string = string;
  start = 0;
  length = String.length string;
  head = 0
} 

(* This exception is raised when attempting to read past the end of a zone. *)

exception EOZ

(* This auxiliary function simulates the head's advance, and raises [EOZ] if it would go past the end of the zone.
   It returns the head's new position, but does not actually move it. *)

let peek_ahead zone offset =
  let position = zone.head + offset in
  if position > zone.length then
    raise EOZ;
  position

(* This auxiliary function simulates the head's advance, and raises [EOZ] if it would go past the end of the zone.
   Then, it calls its [action] function; when the function returns, it actually advances the read head, unless some
   exception occurred. *)

let advance zone offset action =
  let position = peek_ahead zone offset in
  let result = action() in
  zone.head <- position;
  result

(* This auxiliary function converts an unsigned integer into a signed one. The [mask] indicates the position of the
   sign bit. *)

let signed mask x =
  if x land mask = 0 then
    x
  else
    x - (mask lsl 1)

(* This function reads an unsigned 8-bit integer at the read head's current position. After reading, the read head
   advances by one byte. *)

let u1 zone =
  advance zone 1 (fun () ->
    Char.code zone.string.[zone.start + zone.head]
  )

(* This function reads a signed 8-bit integer at the read head's current position. After reading, the read head
   advances by one byte. *)

let s1 zone =
  signed 0x80 (u1 zone)

(* This function reads an unsigned 16-bit integer, in big-endian order, at the read head's current position. After
   reading, the read head advances by two bytes. *)

let u2 zone =
  let hi = u1 zone in
  let lo = u1 zone in
  (hi lsl 8) + lo

(* This function reads a signed 16-bit integer, in big-endian order, at the read head's current position. After
   reading, the read head advances by two bytes. *)

let s2 zone =
  signed 0x8000 (u2 zone)

(* This function reads an unsigned 16-bit integer, in big-endian order, at the read head's current position. The
   read head does not advance. *)

let peek2 zone =
  let _ = peek_ahead zone 2 in
  let hi = Char.code zone.string.[zone.start + zone.head] in
  let lo = Char.code zone.string.[zone.start + zone.head + 1] in
  (hi lsl 8) + lo

(* This function reads a signed 32-bit integer, in big-endian order, at the read head's current position. After
   reading, the read head advances by four bytes. TEMPORARY Because O'Caml integers are signed 31-bit, the result
   will be incorrect if the integer stored in the zone has more than 30 significant bits. *)

let s4 zone =
  let hi = u2 zone in
  let lo = u2 zone in
  (hi lsl 16) + lo

(* This function reads a sub-zone of length [length] at the read head's current position. After reading, the
   read head advances by [length] bytes. The newly created zone uses the same data buffer as its parent zone.
   Its read head is initially located at the beginning of the sub-zone. *)

let sub zone length =
  advance zone length (fun () -> {
    string = zone.string;
    start = zone.start + zone.head;
    length = length;
    head = 0
  })

(* This function converts a zone into a string. It is the sub-string of the original buffer string covered by
   the zone. The zone's read head position is irrelevant, and unaffected. *)

let string zone =
  String.sub zone.string zone.start zone.length

(* This function reads in a ``table'' consisting of an element count (to be read by [rc]), followed by as many
   (possibly variable-length) elements. [rc] should typically be [u1] or [u2], depending on whether the element count
   is stored using one or two bytes. The [element] auxiliary function is invoked to read each element. It finds the
   read head positioned at the beginning of the element, and must leave it at the end of the element. The table is
   returned as a list, where the first element found in the zone appears first. The read head is advanced to the end
   of the table. *)

let table zone rc element =
  let rec read count =
    if count = 0 then
      []
    else
      let x = element zone in
      x :: (read (count - 1)) in
  read (rc zone)

(* This function reads in a ``table'' consisting of an undetermined number of (possibly variable-length) elements. The
   end of the table is deemed to coincide with the end of the zone. The [element] auxiliary function is invoked to
   read each element. It finds the read head positioned at the beginning of the element, and must leave it at the end
   of the element. The table is returned as a list, where the first element found in the zone appears first. The read
   head is advanced to the end of the table. *)

let rec undetermined_table zone element =
  if zone.head = zone.length then
    []
  else
    let head = element zone in
    head :: (undetermined_table zone element)

(* This function reads in a ``table'' consisting of an undetermined number of (possibly variable-length) elements. The
   end of the table is deemed to coincide with the end of the zone. The [element] auxiliary function is invoked to
   read each element. It finds the read head positioned at the beginning of the element, and must leave it at the end
   of the element. The table is returned as a hash table which maps zone offsets to elements. The zone's read head is
   advanced to the end of the table. *)

let hash_table zone element =
  let table = Hashtbl.create 1023 in
  let rec read () =
    if zone.head = zone.length then
      ()
    else
      let offset = zone.head in
      Hashtbl.add table offset (element zone);
      read() in
  read();
  table

(* This function reads in a ``Pascal'' string, i.e. a string prefixed with its length in bytes. The length field is
   to be read by [rc]. The read head is advanced to the end of the string. *)

let pascal zone rc =
  string (sub zone (rc zone))

(* This function returns the zone's current read head position. *)

let head zone =
  zone.head

(* This function sets the zone's read head position. *)

let seek zone position =
  assert (position >= 0);
  if position > zone.length then
    raise EOZ;
  zone.head <- position

(* This function returns a zone's length. *)

let length zone =
  zone.length

