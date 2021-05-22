open StackLangBasics

val registers : value -> registers
(**[registers v] is the set of the registers that appear in the value [v]. *)

val registers_of_list : value list -> StringSet.t
(**[registers_of_list vs] is the set of the registers that appear in
   the values [vs]. *)
