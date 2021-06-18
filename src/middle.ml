(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

open Grammar

open Printf

module Run () = struct

(* -------------------------------------------------------------------------- *)

(* If [--random-sentence] was specified on the command line, obey it. *)
let print_sentence terminals : unit =
  List.iter (fun t ->
    printf "%s\n" (Terminal.print t)
  ) terminals


let () =
  Settings.random_sentence |> Option.iter begin fun (nt, goal, _style) ->
    match Nonterminal.lookup nt with
    | exception Not_found ->
        Error.error [] "the nonterminal symbol %s does not exist." nt
    | nt ->
        let sentence = RandomSentenceGenerator.nonterminal nt goal in
        print_sentence sentence;
        exit 0
  end

(* -------------------------------------------------------------------------- *)

end (* Run *)
