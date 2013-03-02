(* $Header: /home/yquem/cristal/fpottier/cvs/modulo/id.ml,v 1.2 2002/05/24 09:32:55 fpottier Exp $ *)

(* Identifiers. *)

type id =
    string

(* Maps from identifiers to arbitrary data, used to implement
   symbol tables, environments, etc. *)

module I = struct
  type t = id
  let compare : id -> id -> int = compare
end

module IdSet = Set.Make (I)

module IdMap = Map.Make (I)

