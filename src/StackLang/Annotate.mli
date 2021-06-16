open StackLang
open Sync

type cell = Invariant.cell

type cells = Invariant.cell array

val push : program -> cells -> sync -> value -> cell -> cells * sync

val pop : program -> cells -> sync -> pattern -> cells * sync

val def : program -> cells -> sync -> Bindings.t -> cells * sync

val case_tag_branch :
  program -> cells -> sync -> tagpat -> block -> cell array * sync

val typed_block : program -> cells -> sync -> typed_block -> cells * sync

val jump : program -> cells -> sync -> label -> cells * sync

module Curry : functor
  (P : sig
     val program : program

     val cells : cells

     val sync : sync
   end)
  -> sig
  val push : value -> cell -> cells * sync

  val pop : pattern -> cells * sync

  val def : bindings -> cells * sync

  val case_tag_branch : tagpat -> block -> cell array * sync

  val typed_block : typed_block -> cells * sync

  val jump : label -> cells * sync
end
