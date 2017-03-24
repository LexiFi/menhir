/******************************************************************************/
/*                                                                            */
/*                                   Menhir                                   */
/*                                                                            */
/*                       François Pottier, Inria Paris                        */
/*              Yann Régis-Gianas, PPS, Université Paris Diderot              */
/*                                                                            */
/*  Copyright Inria. All rights reserved. This file is distributed under the  */
/*  terms of the GNU Library General Public License version 2, with a         */
/*  special exception on linking, as described in the file LICENSE.           */
/*                                                                            */
/******************************************************************************/

(* This is menhir's standard library. It offers a number of
   parameterized nonterminal definitions, such as options and lists,
   that should be useful in a number of circumstances. *)

%%

(* ------------------------------------------------------------------------- *)
(* The identity. *)

(* [anonymous(X)] is the same as [X]. *)

(* This allows placing an anonymous sub-rule in the middle of a rule, as in:

     foo
     anonymous(baz { action1 })
     bar
     { action2 }

   Because anonymous is marked %inline, everything is expanded away. So,
   this is equivalent to:

     foo baz bar { action1; action2 }

   Note that [action1] moves to the end of the rule. The anonymous sub-rule
   can even have several branches, as in:

     foo
     anonymous(baz { action1a } | quux { action1b })
     bar
     { action2 }

   This is expanded to:

     foo baz  bar { action1a; action2 }
   | foo quux bar { action1b; action2 }

*)

%public %inline anonymous(X):
x = X
    { x }

(* [embedded(X)] is the same as [X]. *)

(* This allows placing an anonymous sub-rule in the middle of a rule, as in:

     foo
     embedded(baz { action1 })
     bar
     { action2 }

   Because [embedded] is not marked %inline, this is equivalent to:

     foo xxx bar { action2 }

   where the fresh non-terminal symbol [xxx] is separately defined by:

     xxx: baz { action1 }

   In particular, if there is no [baz], what we get is a semantic action
   embedded in the middle of a rule. For instance,

     foo embedded({ action1 }) bar { action2 }

   is equivalent to:

     foo xxx bar { action2 }

   where [xxx] is separately defined by the rule:

     xxx: { action1 }

*)

%public embedded(X):
x = X
    { x }

(* ------------------------------------------------------------------------- *)
(* Options. *)

(* [option(X)] recognizes either nothing or [X]. It produces a value
   of type ['a option] if [X] produces a value of type ['a]. *)

%public option(X):
  /* nothing */
    { None }
| x = X
    { Some x }

(* [ioption(X)] is identical to [option(X)], except its definition is
   inlined. This has the effect of duplicating the production that
   refers to it, possibly eliminating an LR(1) conflict. *)

%public %inline ioption(X):
  /* nothing */
    { None }
| x = X
    { Some x }

(* [boption(X)] recognizes either nothing or [X]. It produces a value
   of type [bool]. *)

%public boption(X):
  /* nothing */
    { false }
| X
    { true }

(* [loption(X)] recognizes either nothing or [X]. It produces a value
   of type ['a list] if [X] produces a value of type ['a list]. *)

%public loption(X):
  /* nothing */
    { [] }
| x = X
    { x }

(* ------------------------------------------------------------------------- *)
(* Sequences. *)

(* [pair(X, Y)] recognizes the sequence [X Y]. It produces a value of
   type ['a * 'b] if [X] and [Y] produce values of type ['a] and ['b],
   respectively. *)

%public %inline pair(X, Y):
  x = X; y = Y
    { (x, y) }

(* [separated_pair(X, sep, Y)] recognizes the sequence [X sep Y]. It
   produces a value of type ['a * 'b] if [X] and [Y] produce values of
   type ['a] and ['b], respectively. *)

%public %inline separated_pair(X, sep, Y):
  x = X; sep; y = Y
    { (x, y) }

(* [preceded(opening, X)] recognizes the sequence [opening X]. It
   passes on the value produced by [X], so that it produces a value of
   type ['a] if [X] produces a value of type ['a]. *)

%public %inline preceded(opening, X):
  opening; x = X
    { x }

(* [terminated(X, closing)] recognizes the sequence [X closing]. It
   passes on the value produced by [X], so that it produces a value of
   type ['a] if [X] produces a value of type ['a]. *)

%public %inline terminated(X, closing):
  x = X; closing
    { x }

(* [delimited(opening, X, closing)] recognizes the sequence [opening X
   closing]. It passes on the value produced by [X], so that it
   produces a value of type ['a] if [X] produces a value of type
   ['a]. *)

%public %inline delimited(opening, X, closing):
  opening; x = X; closing
    { x }

(* ------------------------------------------------------------------------- *)
(* Lists. *)

(* [list(X)] recognizes a possibly empty list of [X]'s. It produces a
   value of type ['a list] if [X] produces a value of type ['a]. The
   front element of the list is the first element that was parsed. *)

%public list(X):
  /* nothing */
    { [] }
| x = X; xs = list(X)
    { x :: xs }

(* [nonempty_list(X)] recognizes a nonempty list of [X]'s. It produces
   a value of type ['a list] if [X] produces a value of type ['a]. The
   front element of the list is the first element that was parsed. *)

%public nonempty_list(X):
  x = X
    { [ x ] }
| x = X; xs = nonempty_list(X)
    { x :: xs }

(* [separated_list(separator, X)] recognizes a possibly empty list of
   [X]'s, separated with [separator]'s. It produces a value of type
   ['a list] if [X] produces a value of type ['a]. The front element
   of the list is the first element that was parsed. *)

%public %inline separated_list(separator, X):
  xs = loption(separated_nonempty_list(separator, X))
    { xs }

(* [separated_nonempty_list(separator, X)] recognizes a nonempty list
   of [X]'s, separated with [separator]'s. It produces a value of type
   ['a list] if [X] produces a value of type ['a]. The front element
   of the list is the first element that was parsed. *)

%public separated_nonempty_list(separator, X):
  x = X
    { [ x ] }
| x = X; separator; xs = separated_nonempty_list(separator, X)
    { x :: xs }

%%

