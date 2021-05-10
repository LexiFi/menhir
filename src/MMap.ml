module type S = sig
  include Map.S

  module Domain : Set.S with type elt = key

  val cardinal : 'a t -> int

  val filter : (key -> 'a -> bool) -> 'a t -> 'a t

  val restrict : Domain.t -> 'a t -> 'a t

  val domain : 'a t -> Domain.t

  val multiple_add : key -> 'a -> 'a list t -> 'a list t

  val of_list : (key * 'a) list -> 'a t
end


module Make (Ord : Map.OrderedType) = struct
  include Map.Make (Ord)

  module Domain = Set.Make(Ord)

  let cardinal s = fold (fun _ _ x -> x + 1) s 0

  let filter pred map =
    fold
      (fun key value map -> if pred key value then add key value map else map)
      map empty

  let restrict domain map = filter (fun k _ -> Domain.mem k domain) map

  let domain map =
    fold (fun key _ acu -> Domain.add key acu) map Domain.empty

  let multiple_add k v m =
    let vs = try find k m with Not_found -> [] in
    add k (v :: vs) m

  let of_list li =
    let r = empty in
    List.fold_left (fun map (key, value) -> add key value map) r li
end
