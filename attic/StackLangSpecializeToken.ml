(* A naive approach to specializing the StackLang program for every token.    *)

(* -------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------- *)

module SpecializeToken (X : sig val program : program end) = struct open X

let print = Grammar.Terminal.print

let token = EmitStackLang.token

(* This register must be globally fresh. One fresh register suffices
   because we never analyze two tokens at the same time. *)
let tokv : register =
  Reg.import "_tokv"

let sensitive =
  Reg.Set.of_list [ token; tokv ]

type env =

  (* If [env] is [Some (t, true)] then the current token corresponds to
     the terminal symbol [t] and its semantic value is currently stored in
     the register [tokv]. If [env] is [Some (t, false)] then the current
     token corresponds to the terminal symbol [t] and this symbol has no
     semantic value. If [env] is [None] then nothing is known about the
     current token. *) (* TODO update comment *)
  terminal option

(* A debugging printer. *)

let print_env ot =
  match ot with
  | None ->
      sprintf "token: <unknown>"
  | Some t ->
      sprintf "token: %s" (print t)

(* A block can be specialized if and only if the [token] register appears
   among its needed registers. *)

let can_specialize_tblock tblock =
  Reg.Set.mem token tblock.needed

let can_specialize label =
  let tblock = lookup program label in
  can_specialize_tblock tblock

module Data = struct
  include Order.Option(Grammar.Terminal)
  let default = None
end

module S = struct

type label' = label * env

let spec_label (label, env) : label =
  match env with
  | None ->
      label
  | Some t ->
      sprintf "%s_spec_%s" (Label.export label) (print t)
      |> Label.import

let assert_known_token =
  Option.force

let analyze_token (k : env -> block) : block =
  let branches =
    Grammar.Terminal.fold_real (fun t branches ->
      let tokpat = TokSingle (t, tokv) in
      let env = Some t in
      let block = k env in
      let branch = tokpat, block in
      branch :: branches
    ) []
  in
  ICaseToken (token, branches, None)

let rec spec_block jump env block =
  match block with

  | IJump label ->
      (* Decide whether we want to forget information at this point. *)
      let env =
        if env <> None && can_specialize label then env else None
      in
      (* Emit a jump to the specialized block. *)
      info "(spec) Specializing %s (%s)" (Label.export label) (print_env env)
      (jump (label, env))
        (* TODO decide whether inlining is needed/useful *)

  | IPrim (p, PrimLexerCall vs, block) ->
      (* A lexer call overwrites the [token] register. *)
      assert (p = PReg token);
      (* The lexer call is preserved by the program transformation.
         Immediately after this call, we choose to eagerly analyze
         the new token and to specialize the code that follows. *)
      IPrim (p, PrimLexerCall vs,
      analyze_token (fun env -> spec_block jump env block))

  | ICaseToken (r, branches, odefault) ->
      (* [r] must be the [token] register. *)
      assert (r = token);
      (* The current token must be known, because we insert a CASEtok
         instruction immediately after every lexer call. *)
      let t = assert_known_token env in
      (* Thus, this CASEtok instruction can be simplified. *)
      begin match find_tokbranch branches t, odefault with
      | Some (TokSingle (_, r), block), _ ->
          (* Emit a [DEF] instruction to copy the semantic value of the token
             from the register [tokv] to the register [r], where this branch
             expects to find it. *)
          let bs = Bindings.assign [PReg r] [VReg tokv] in
          IDef (bs, spec_block jump env block)
      | Some (TokMultiple _, block), _
      | None, Some block ->
          spec_block jump env block
      | None, None ->
          (* No branch is taken, and there is no default branch.
             This should not happen. *)
          assert false
      end

  (* The instructions that remain do not write the registers [token] or
     [tokv], so the environment need not be updated. These instructions
     are not transformed. *)
  | IPush _
  | IPop _
  | IPeek _
  | IDef _
  | IPrim _
  | ITrace _
  | IComment _
  | IDead _
  | IStop _
  | IReturn _
  | ICaseTag _
    ->
      assert (Reg.Set.disjoint sensitive (written block));
      Block.map (spec_block jump env) block

let spec_tblock jump (_label, env) tblock =
  (* Transform the block. *)
  let block = spec_block jump env tblock.block in
  (* The type of the block is unaffected. *)
  let needed = tblock.needed in
  let needed =
    match env with
    | None ->
        needed
    | Some _ ->
        assert (Reg.Set.mem token tblock.needed);
        (* The register [token] is removed from the needed registers,
           and the register [tokv] is added instead. *)
        Reg.Set.(add tokv (remove token needed))
  in
  (* Done. *)
  { tblock with block; needed }

end (* S *)

include StackLangSpecialize.Make(Data)(S)(X)

end (* SpecializeToken *)

let specialize_token program =
  let module S = SpecializeToken(struct let program = program end) in
  Time.tick "StackLang: specialization for token";
  S.program
