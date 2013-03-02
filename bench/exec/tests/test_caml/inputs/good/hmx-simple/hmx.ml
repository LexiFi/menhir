(* $Header: /home/pauillac/formel1/fpottier/cvs/hmx/hmx.ml,v 1.12 2001/03/28 16:12:35 fpottier Exp $ *)

module type S = sig

  (* Names of primitive operations. *)

  type primitive

  (* Type nodes. *)

  type node

  (* Type schemes. *)

  type scheme

  (* Type environments. *)

  type environment =
      (string * scheme) list

  (* Program terms. A phrase is a single toplevel \texttt{let} definition. *)

  type expression =
    | PrimApp of primitive * expression list
    | Var of string
    | Lambda of string * expression
    | App of expression * expression
    | Let of string * expression * expression

  type phrase =
      string * expression

  (* Type inference. This function accepts an environment and a phrase. It infers a type, generalizes it, and returns
     it, together with the name of the variable being defined. *)

  val run: environment -> phrase -> string * scheme

end

(* This signature specifies the operations which allow building type terms. *)

module type TermSig = sig

  type node

  val arrow: node -> node -> node

end

module Make
    (T : TermSig)
    (X : ConstraintSystem.S with type node = T.node)
    (P : Primitives.S with type node = X.node)
= struct

  (* Names of primitive operations. *)

  type primitive = P.name

  (* Type nodes. *)

  type node = X.node

  (* Type schemes. *)

  type scheme = X.scheme

  (* Type environments. *)

  type environment =
      (string * scheme) list

  (* Program terms. A phrase is a single toplevel \texttt{let} definition. *)

  type expression =
    | PrimApp of primitive * expression list
    | Var of string
    | Lambda of string * expression
    | App of expression * expression
    | Let of string * expression * expression

  type phrase =
      string * expression

  (* Type inference.

     The parameter [env] represents the type environment.

     The parameter [mono] is a function which, given a type node, lowers its level appropriately. It is used to
     prevent type variables potentially associated with mutable storage locations from being generalized. *)

  let rec infer env mono = function
    | PrimApp (name, args) ->

	(* Look up the named primitive, and obtain its typing rule. Apply it. The call to [List.iter2] cannot fail
	   if the syntax tree (i.e. the parser) and the primitives module [P] agree on the arity of every primitive. *)

	let premises, conclusion = P.rule mono name in
	List.iter2 X.unify premises (List.map (infer env mono) args);
	conclusion

    | Var x ->

	(* Find the named entry in the current typing environment. *)

	let scheme = try
	  List.assoc x env
	with Not_found ->
	  failwith ("Unbound program variable: " ^ x) in

	(* Instantiate the type scheme. This returns the body of the type scheme's instance, and implicitly affects
	   the global constraint set. *)

	X.instantiate scheme

    | Lambda (x, e) ->

	(* Because $\lambda$-abstraction defers side effects, the type of any mutable storage cells allocated within
	   the abstraction's body may safely be created at the current level. We reflect this by providing a new
	   definition of [mono], obtained by applying [X.capture], into the recursive call to [infer]. *)

	let domain = X.fresh() in
	let codomain = infer ((x, X.inject domain) :: env) (X.capture()) e in
	T.arrow domain codomain

    | App (e1, e2) ->

	(* Because the result of an application may potentially expand the store, we cannot generalize it. Make it
	   monomorphic. *)

	let codomain = X.fresh() in
	mono codomain;

        (* Unify actual and expected types. *)

	X.unify (infer env mono e1) (T.arrow (infer env mono e2) codomain);
	codomain

    | Let (x, e1, e2) ->

	(* Infer a type for [e1] and generalize it. Infer a type for [e2] within an augmented type environment. *)

	infer ((x, infer_and_generalize env mono e1) :: env) mono e2

  and infer_and_generalize env mono e =

    (* Infer a type for [e], making sure that all variables freshly created in this sub-derivation are marked
       as such, i.e. would be quantified by a $(\exists Intro)$ rule. Generalize the type thus obtained. *)

    X.scope (fun () ->
      X.generalize (infer env mono e)
    )

  (* External interface. *)

  let run env (x, e) =
    x, infer_and_generalize env (X.capture()) e

end

