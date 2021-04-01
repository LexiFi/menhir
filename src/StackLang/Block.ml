open StackLang
open StackLangUtils

type t = block

let iter f aggregate terminate = function
  | INeed (_, block)
  | IPush (_, _, block)
  | IPop (_, block)
  | IDef (_, _, block)
  | IPrim (_, _, block)
  | ITrace (_, block)
  | IComment (_, block)
  | ITypedBlock {block} ->
      f block
  | IDie | IReturn _ | IJump _ | ISubstitutedJump _ ->
      terminate
  | ICaseToken (_reg, branches, odefault) ->
      aggregate
        ( List.map (branch_iter f) branches
        @ match odefault with None -> [] | Some block -> [f block] )
  | ICaseTag (_reg, branches) ->
      aggregate @@ List.map (branch_iter f) branches

let map f = function
  | INeed (regs, block) ->
      INeed (regs, f block)
  | IPush (value, cell, block) ->
      IPush (value, cell, f block)
  | IPop (reg, block) ->
      IPop (reg, f block)
  | IDef (pat, value, block) ->
      IDef (pat, value, f block)
  | IPrim (reg, prim, block) ->
      IPrim (reg, prim, f block)
  | ITrace (reg, block) ->
      ITrace (reg, f block)
  | IComment (comment, block) ->
      IComment (comment, f block)
  | (IDie | IReturn _ | IJump _ | ISubstitutedJump _) as end_block ->
      end_block
  | ICaseToken (reg, branches, odefault) ->
      ICaseToken (reg, List.map (branch_map f) branches, Option.map f odefault)
  | ICaseTag (reg, branches) ->
      ICaseTag (reg, List.map (branch_map f) branches)
  | ITypedBlock t_block ->
      ITypedBlock {t_block with block= f t_block.block}

(* -------------------------------------------------------------------------- *)

(* [successors yield block] applies the function [yield] in turn to every
   label that is the target of a [jump] instruction in the block [block]. *)

let rec successors yield block =
  match block with
  | INeed (_, block)
  | IPush (_, _, block)
  | IPop (_, block)
  | IDef (_, _, block)
  | IPrim (_, _, block)
  | ITrace (_, block)
  | IComment (_, block) ->
      successors yield block
  | IDie | IReturn _ ->
      ()
  | IJump label | ISubstitutedJump (label, _) ->
      yield label
  | ICaseToken (_, branches, oblock) ->
      List.iter (branch_iter (successors yield)) branches ;
      Option.iter (successors yield) oblock
  | ICaseTag (_, branches) ->
      List.iter (branch_iter (successors yield)) branches
  | ITypedBlock {block; stack_type= _; final_type= _} ->
      successors yield block
