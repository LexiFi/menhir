module Self = struct

  type t =
      string Location.t

  let compare (id1 : t) (id2 : t) =
    compare (Annotation.content id1) (Annotation.content id2)

end

include Self

module Map = Map.Make(Self)

let basename id =
  Annotation.map AlphaLib.Atom.String.basename id

let combine id i =
  Annotation.map (fun s ->
    AlphaLib.Atom.String.combine s i
  ) id

let print b id =
  Buffer.add_string b (Annotation.content id)
