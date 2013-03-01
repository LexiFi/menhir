include Set.Make (String)

let of_list xs =
  List.fold_right add xs empty

