type location =
    (Lexing.position * Lexing.position) option

type 'a t =
    (location, 'a) Annotation.t

let none v =
  Annotation.make None v

let some pos1 pos2 v =
  Annotation.make (Some (pos1, pos2)) v

let dummy =
  none ()

let get loc v =
  match Annotation.get v with
  | Some _ as loc ->
      loc
  | None ->
      loc

let content =
  Annotation.content

let wrap f loc x =
  let loc = get loc x in
  f loc (content x)

