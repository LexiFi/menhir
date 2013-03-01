(* $Header: /net/pauillac/caml/repository/bigbro/cache.ml,v 1.3 2001/03/06 13:58:56 fpottier Exp $ *)

open Link_info

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Fragments are ignored in the cache. (This is important - otherwise identical URLs with different fragments
would cause duplicate checks.)

*)

type key = Url.url * bool

let normalize (url, flag) =
  (Url.drop_fragment url, flag)
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Create the lock.

*)

let lock = Mutex.create();;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

To implement the cache, we have the choice between a map (i.e. a balanced tree) and a hash table. For the waiting
room, a hash table is mandatory, because it must support multiple entries per key. So, two functors are provided,
and either of them can be used for the cache.

Fix (2001/03/01): for the cache, multiple bindings use up memory needlessly. The [MakeMonoHash] implementation avoids
creating them.

*)

module MakeMap (Param : sig type t end) = struct

  module MyMap = Map.Make (struct type t = key let compare = Pervasives.compare end)

  let table = ref (MyMap.empty : Param.t MyMap.t)

  let add key data =
    table := MyMap.add (normalize key) data !table
  ;;

  let find key =
    MyMap.find (normalize key) !table
  ;;

end

module MakeHash (Param : sig type t end) = struct

  let table = (Hashtbl.create 1023 : (key, Param.t) Hashtbl.t);;

  let add key data =
    Hashtbl.add table (normalize key) data
  ;;

  let find key =
    Hashtbl.find table (normalize key)
  ;;

  let remove key =
    Hashtbl.remove table (normalize key)
  ;;

end

module MakeMonoHash (Param : sig type t end) = struct

  let table = (Hashtbl.create 1023 : (key, Param.t) Hashtbl.t);;

  let add key data =
    Hashtbl.replace table (normalize key) data
  ;;

  let find key =
    Hashtbl.find table (normalize key)
  ;;

end

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Apply the functor twice.

*)

module Cache = MakeMonoHash (struct type t = outcome end)
module Room  = MakeHash (struct type t = full_link_info end)
