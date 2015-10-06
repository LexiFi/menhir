(* Experimental code to produce a rendition of the automaton as a
   directed graph. This code is currently not plugged in. *)

open Grammar

module G = struct

  type vertex =
      Lr1.node

  let number s : string =
    string_of_int (Lr1.number s)

  let name s =
    Printf.sprintf "s%s" (number s)

  let successors (action: ?style:Dot.style -> label:string -> vertex -> unit) s : unit =
    Lr1.transitions s |> SymbolMap.iter (fun symbol s' ->
      action ~label:(Symbol.print symbol) s'
    )
      
  let iter (action: ?style:Dot.style -> label:string -> vertex -> unit) : unit =
    Lr1.iter (fun s ->
      action ~label:(number s) s
    )

end

let filename =
  Printf.sprintf "%s.dot" Settings.base

let () =
  let c = open_out filename in
  let module P = Dot.Print(G) in
  P.print ~orientation:Dot.Portrait ~size:(8.,5.) c;
  close_out c

