(* Source *)
module S = StackLang

(* Target *)
module T = IL

val compile : S.program -> T.program
(** Compile a StackLang program into an IL one. *)
