open StackLang
open StackLangUtils

type t = block

let reduce f aggregate = function
  | INeed (_, block)
  | IPush (_, _, block)
  | IPop (_, block)
  | IDef (_, block)
  | IPrim (_, _, block)
  | ITrace (_, block)
  | IComment (_, block)
  | ITypedBlock { block } ->
      aggregate ([f block])
  | IDie | IReturn _ | IJump _ ->
      aggregate []
  | ICaseToken (_reg, branches, odefault) ->
      aggregate
        ( List.map (branch_iter f) branches
        @ match odefault with None -> [] | Some block -> [ f block ] )
  | ICaseTag (_reg, branches) ->
      aggregate @@ List.map (branch_iter f) branches


let map
    f
    ?(need = fun registers b -> (registers, f b))
    ?(push = fun value cell b -> (value, cell, f b))
    ?(pop = fun pattern b -> (pattern, f b))
    ?(def = fun bindings b -> (bindings, f b))
    ?(prim = fun reg pr b -> (reg, pr, f b))
    ?(trace = fun reg b -> (reg, f b))
    ?(comment = fun content b -> (content, f b))
    ?(die = fun () -> ())
    ?(return = Fun.id)
    ?(jump = Fun.id)
    ?(case_token =
      fun reg branches odefault ->
        (reg, List.map (branch_map f) branches, Option.map f odefault))
    ?(case_tag = fun reg branches -> (reg, List.map (branch_map f) branches))
    ?(typed_block =
      fun ({ block } as tblock) -> { tblock with block = f block }) = function
  | INeed (regs, block) ->
      let regs, block = need regs block in
      INeed (regs, block)
  | IPush (value, cell, block) ->
      let value, cell, block = push value cell block in
      IPush (value, cell, block)
  | IPop (reg, block) ->
      let reg, block = pop reg block in
      IPop (reg, block)
  | IDef (bindings, block) ->
      let bindings, block = def bindings block in
      IDef (bindings, block)
  | IPrim (reg, pr, block) ->
      let reg, pr, block = prim reg pr block in
      IPrim (reg, pr, block)
  | ITrace (reg, block) ->
      let reg, block = trace reg block in
      ITrace (reg, block)
  | IComment (content, block) ->
      let content, block = comment content block in
      IComment (content, block)
  | IDie ->
      die ();
      IDie
  | IReturn value ->
      IReturn (return value)
  | IJump label ->
      IJump (jump label)
  | ICaseToken (reg, branches, odefault) ->
      let reg, branches, odefault = case_token reg branches odefault in
      ICaseToken (reg, branches, odefault)
  | ICaseTag (reg, branches) ->
      let reg, branches = case_tag reg branches in
      ICaseTag (reg, branches)
  | ITypedBlock t_block ->
      ITypedBlock (typed_block t_block)


let iter
    (f : t -> unit)
    ?(need = fun _ b -> f b)
    ?(push = fun _ _ b -> f b)
    ?(pop = fun _ b -> f b)
    ?(def = fun _ b -> f b)
    ?(prim = fun _ _ b -> f b)
    ?(trace = fun _ b -> f b)
    ?(comment = fun _ b -> f b)
    ?(die = ignore)
    ?(return = ignore)
    ?(jump = ignore)
    ?(case_token =
      fun _ branches odefault ->
        List.iter (branch_iter f) branches;
        Option.iter f odefault)
    ?(case_tag = fun _ branches -> List.iter (branch_iter f) branches)
    ?(typed_block = fun { block } -> f block) = function
  | INeed (regs, block) ->
      need regs block
  | IPush (value, cell, block) ->
      push value cell block
  | IPop (reg, block) ->
      pop reg block
  | IDef (bindings, block) ->
      def bindings block
  | IPrim (reg, pr, block) ->
      prim reg pr block
  | ITrace (reg, block) ->
      trace reg block
  | IComment (content, block) ->
      comment content block
  | IDie ->
      die ()
  | IReturn value ->
      return value
  | IJump label ->
      jump label
  | ICaseToken (reg, branches, odefault) ->
      case_token reg branches odefault
  | ICaseTag (reg, branches) ->
      case_tag reg branches
  | ITypedBlock t_block ->
      typed_block t_block


(* -------------------------------------------------------------------------- *)

(* [successors yield block] applies the function [yield] in turn to every
   label that is the target of a [jump] instruction in the block [block]. *)

let rec successors yield block =
  match block with
  | INeed (_, block)
  | IPush (_, _, block)
  | IPop (_, block)
  | IDef (_, block)
  | IPrim (_, _, block)
  | ITrace (_, block)
  | IComment (_, block) ->
      successors yield block
  | IDie | IReturn _ ->
      ()
  | IJump label ->
      yield label
  | ICaseToken (_, branches, oblock) ->
      List.iter (branch_iter (successors yield)) branches;
      Option.iter (successors yield) oblock
  | ICaseTag (_, branches) ->
      List.iter (branch_iter (successors yield)) branches
  | ITypedBlock { block; stack_type = _; final_type = _ } ->
      successors yield block


let need register block = INeed (register, block)

let push value cell block = IPush (value, cell, block)

let pop pattern block = IPop (pattern, block)

let def bindings block =
  if Bindings.is_empty bindings
  then block
  else
    match block with
    | IDef (bindings', block) ->
        IDef (Bindings.compose bindings bindings', block)
    | _ ->
        IDef (bindings, block)


let sdef pattern value block =
  let bindings = Bindings.assign pattern value in
  def bindings block


let prim register primitive block = IPrim (register, primitive, block)

let comment content block = IComment (content, block)

let die = IDie

let return value = IReturn value

let jump label = IJump label

let case_token ?default register branches =
  ICaseToken (register, branches, default)


let case_tag register branches = ICaseTag (register, branches)

let typed_block stack_type needed_registers has_case_tag ?final_type ?name block
    =
  ITypedBlock
    { stack_type; needed_registers; has_case_tag; final_type; name; block }
