open StackLang

(** [inline program] transforms the program [program] by removing every
   unreachable block and by inlining away every (non-entry) label whose
   in-degree is 1. *)
   val inline: program -> program