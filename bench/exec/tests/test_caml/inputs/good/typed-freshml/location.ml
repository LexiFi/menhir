type location =
    Lexing.position * Lexing.position

type 'a t =
    (location, 'a) Annotation.t

let dummy =
  (Lexing.dummy_pos, Lexing.dummy_pos)

let vdummy =
  Annotation.make dummy ()

