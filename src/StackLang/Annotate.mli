open StackLang
open Sync

type cell = Invariant.cell

type cells = Invariant.cell array

val push : program -> cells -> sync -> value -> cell -> cells * sync

val pop : program -> cells -> sync -> pattern -> cells * sync

val def : program -> cells -> sync -> Bindings.t -> cells * sync

val case_tag :
     program
  -> cells
  -> sync
  -> register
  -> (tagpat * block) list
  -> (cells * sync) list

val typed_block : program -> cells -> sync -> typed_block -> cells * sync

val jump : program -> cells -> sync -> label -> cells * sync

module Curry : functor
  (P : sig
     val program : program
   end)
  -> sig
  val push : cells -> sync -> value -> cell -> cells * sync

  val pop : cells -> sync -> pattern -> cells * sync

  val def : cells -> sync -> bindings -> cells * sync

  val case_tag :
    cells -> sync -> register -> (tagpat * block) list -> (cells * sync) list

  val typed_block : cells -> sync -> typed_block -> cells * sync

  val jump : cells -> sync -> label -> cells * sync
end
