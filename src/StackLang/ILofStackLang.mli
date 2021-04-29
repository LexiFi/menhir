(* Source *)
module S = StackLang

(* Target *)
module T = IL

(* Compile a StackLang program into an IL one. *)
val compile : S.program -> T.program
