open StackLangBasics

val registers : value -> registers
(**[registers v] is the set of the registers that appear in the value [v]. *)

val registers_of_list : value list -> registers
(**[registers_of_list vs] is the set of the registers that appear in
   the values [vs]. *)

module Accu : sig

  (**The union of [accu] with [registers v]. *)
  val registers : registers -> value -> registers

end
