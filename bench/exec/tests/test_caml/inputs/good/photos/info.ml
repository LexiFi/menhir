module StringSet = struct
  include Set.Make(String)
  let of_list xs =
    List.fold_right add xs empty
end

module StringMap =
  Map.Make(String)

(* Information about an element. *)

type information = {

    (* Caption. *)

    caption: string;

    (* Properties. *)

    properties: StringSet.t

  }

