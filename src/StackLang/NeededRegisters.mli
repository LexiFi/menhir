open StackLang

(** Update the needed register of the program with values computed from scratch.
    The program must not contain ITypedBlock instructions. *)
val update: program -> program