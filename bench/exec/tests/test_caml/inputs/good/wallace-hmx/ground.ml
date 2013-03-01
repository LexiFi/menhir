(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/ground.ml,v 1.13 2000/02/11 16:15:49 fpottier Exp $ *)

(* Here, we define the types and operations which must be provided by a ground signature. The constraint resolution
   engine is parameterized by such a ground signature.

   A ground signature must provide three abstract data types, equipped with appropriate operations: terms, symbols and
   kinds. The type of terms is parameterized: intuitively speaking, a value of type ['a Term.t] represents a term,
   whose head constructor is some symbol, and whose leaves are values of type ['a]. Symbols are partitioned into
   kinds; furthermore, there is a partial order over symbols, which makes each kind a lattice of symbols. Terms are
   also given kinds: a term's kind is its head constructor's kind. Furthermore, the partial order on symbols is
   extended, in a structural way, to a partial order on terms; each kind of terms also forms a lattice. *)

module type Signature = sig

  module Term : sig

    (* The type of terms. Its parameter, ['a], represents the type of leaves, and may be instantiated at will by the
       user. For instance, in an application where the ``small terms invariant'' is enforced, i.e. where all terms
       have height 1, ['a] should be instantiated with the type of type variables. In an application where arbitrary
       terms are allowed, one may instantiate ['a] with the type of terms itself, like this:
       \begin{center}
       [type term = Var of variable | Other of term Term.t]
       \end{center}
       In this module, no assumptions are made about leaves, thus leaving both possibilities open. *)

    type 'a t

    (* The functions [bottom] and [top] accept a term, and return the least and greatest elements of its kind. *)

    val bottom : 'a t -> 'b t
    val top : 'a t -> 'b t

    (* [arity] returns a term's arity. *)

    val arity: 'a t -> int

    (* The following two functions implement constraint propagation. An invocation [sbreak] [action] [term1] [term2]
       is valid only if [term1] and [term2] belong to the same kind. Its effect is to consider a subtyping constraint
       between [term1] and [term2]. If the constraint is inconsistent, i.e. if [term1]'s and [term2]'s head
       constructors aren't in their kind's ordering relation, then [Clash] is raised. Otherwise, the constraint is
       decomposed into a set of constraints between the two terms' leaves. This is done by repeatedly invoking
       [action] [sign] [leaf1] [leaf2], where [leaf1] and [leaf2] are the two leaves which form the sub-constraint,
       and [sign] equals [true] (resp. [false]) if and only if propagation was performed covariantly
       (resp. contravariantly).  [break] is a variant of [sbreak], whose action function does not expect sign
       information. *)

    exception Clash
    val break : ('a -> 'a -> unit) -> 'a t -> 'a t -> unit
    val sbreak : (bool -> 'a -> 'a -> unit) -> 'a t -> 'a t -> unit

    (* The following functions allow iterating over a term's leaves. The effect of [sfold] [action] [sign] [term]
       [accu] is to repeatedly invoke [action] [sign'] [leaf] [accu'], where [sign'] equals [sign] (resp. [not sign])
       if the leaf is a covariant (resp. contravariant) one, [leaf] ranges over [term]'s leaves, and [accu'] is the
       accumulator returned by the previous invocation, if there was one, and [accu] otherwise. [siter], [fold], and
       [iter] are simplified variants of [sfold], which remove the accumulator, the sign, or both, respectively. *)

    val iter : ('a -> unit) -> 'a t -> unit
    val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val siter : (bool -> 'a -> unit) -> bool -> 'a t -> unit
    val sfold : (bool -> 'a -> 'b -> 'b) -> bool -> 'a t -> 'b -> 'b

    (* The following functions allow modifying a term's leaves. The result of [map] [f] [term] is a term whose
       structure is that of [term], and whose leaves are the images of [term]'s leaves through the function [f].
       [endo_map] is a variant of [map], which attempts to preserve sharing, i.e. which returns the original term
       if all leaves are left unchanged by [f]. This eases the load on the garbage collector. [smap] is another
       variant, which provides [f] with the current leaf's sign. *)

    val map : ('a -> 'b) -> 'a t -> 'b t
    val endo_map : ('a -> 'a) -> 'a t -> 'a t
    val smap: (bool -> 'a -> 'b) -> bool -> 'a t -> 'b t

    (* The following functions allow computing greatest lower bounds and least upper bounds of terms. Invoking [glb]
       (resp. [lub]) [leaf_glb] [leaf_lub] [term1] [term2] is valid only if [term1] and [term2] belong to the same
       kind. The function [leaf_glb] (resp. [leaf_lub]) shall be invoked whenever computing the greatest lower bound
       (resp. least upper bound) of two leaves is needed. The result of the invocation is the greatest lower bound
       (resp. least upper bound) of [term1] and [term2]. *)

    val glb : ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    val lub : ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

    (* [expands term] tells whether [term] should also be expanded when a variable, related to it through some
       constraint, is expanded. In general, the answer should be ``yes'', with two exceptions:
       \begin{itemize}
       \item for term constructors which map row terms to regular ones, such as the record constructor or the
             variant constructor, the answer should be ``no''. Indeed, such constructors can only yield regular
             terms, and cannot be lifted to the level of rows. In other words, if some row variable $\rho$ is
             related to such a constructor, e.g. by the constraint $\{ \alpha \} \leq \rho$, then the only way
             of reading this constraint is really $\urow\{ \alpha \} \leq \rho$, meaning that the constructed
             term must be copied when expanding $\rho$. Usual term constructors such as $\rightarrow$, on the
             other hand, map regular terms to regular terms and row terms to row terms, so they should be
             expanded, not copied;
       \item for constant terms, expanding or copying makes no difference, except copying is slightly faster,
             so the answer should be ``no''.
       \end{itemize} *)

    val expands: 'a t -> bool

  end

  module Symbol : sig

    (* The type of symbols. *)

    type t

    (* Invoking [of_term] [term] produces [term]'s head constructor. *)

    val of_term : 'a Term.t -> t

    (* Invoking [ordered] [symbol] [term] is valid only if [symbol] and [term] belong the same kind. The call returns
       [true] if [symbol] is less than [term]'s head constructor, in the kind's ordering, and [false] otherwise. *)

    val ordered : t -> 'a Term.t -> bool

  end

  module Kind : sig

    (* The type of kinds. *)

    type t

    (* These functions map terms and symbols to kinds. *)

    val of_term : 'a Term.t -> t
    val of_symbol: Symbol.t -> t

    (* The functions [bottom] and [top] accept a kind, and return its least and greatest elements. *)

    val bottom : t -> 'a Term.t
    val top : t -> 'a Term.t

    (* [iter action term] allows iterating over a term's leaves. At every leaf, the [action] function is called; it is
       passed the leaf's expected kind, determined by looking at the term's head constructor, and the leaf itself. *)

    val iter: (t -> 'a -> unit) -> 'a Term.t -> unit

  end

  module Sort : sig

    (* [iter action term] allows iterating over a term's leaves. At every leaf, the [action] function is called; it is
       passed the leaf's expected sort, as a flag: [true] if the leaf must be a full row (this is the case, for
       instance, of the record constructor's argument), [false] if the leaf may have any sort. *)

    val iter: (bool -> 'a -> unit) -> 'a Term.t -> unit

  end

  (* Here comes a sub-module of secondary importance, which helps pretty-print type expressions. *)

  module Print : sig

    (* A token is a abstract way of referring to a concrete printing element, such as a punctuation sign, an
       arrow symbol, etc. *)

    type token

    (* [term print_token is_row print_leaf t] prints the term [t], that is, it issues an appropriate sequence of calls
       to [print_token] and [print_leaf]. The argument to [print_token] is a token. [is_row] is a flag telling whether
       the term is expected to be a row term. [print_leaf]'s first argument is a ``shield function'', i.e. a function
       which, when applied to a term, tells whether such a term, if it were to be printed at the current leaf, would
       require being shielded with parentheses. Its second argument is a flag telling whether the leaf is expected to
       be a row. The third argument is the term's leaf. *)

    val term: (token -> unit) -> bool -> (('a Term.t -> bool) -> bool -> 'a -> unit) -> 'a Term.t -> unit

    (* [symbol print_token s] prints the symbol [s], by issuing appropriate calls to [print_token]. *)

    val symbol: (token -> unit) -> Symbol.t -> unit

  end

  module Abstract : sig

    (* Here, we define the type of abstract type expressions, i.e. the structure of the abstract syntax trees produced
       by a type expression parser. The [info] field in each [expression] node, which has unspecified type, allows
       later decorating the tree with extra information, such as the expression's kind and sort. *)

    type 'a expression = {
	actual: 'a actual_expression;
	mutable info: 'a option
      }	

    and 'a actual_expression =
      | Variable of string
      | Term of ('a expression) Term.t
      |	RowExtension of string * 'a expression * 'a expression
      |	RowUniform of 'a expression

    type 'a coercion =
      |	Coercion of 'a expression * 'a expression
      |	Conditional of Symbol.t * 'a expression * 'a expression * 'a expression

  end

end

