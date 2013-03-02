open Printf

type punctuation =
    Buffer.t -> unit

type 'a printer =
    Buffer.t -> 'a -> unit

type 'a iterator =
   unit -> 'a option

let comma buffer =
  bprintf buffer ", "

let preciterator delim elem =
  let rec loop buffer xs =
    match xs() with
    | None ->
	()
    | Some x ->
	bprintf buffer "%t%a%a" delim elem x loop xs in
  loop

let sepiterator sep elem buffer xs =
  match xs() with
  | None ->
      ()
  | Some x ->
      bprintf buffer "%a%a" elem x (preciterator sep elem) xs

