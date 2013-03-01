(* Abstract syntax for nominal type declarations. *)

type standard_type = string                     (* A standard O'Caml type expression *)

type locid =                                    (* An identifier with its location. *)
    Lexing.position * Lexing.position * string

type typeid =                                   (* A type identifier *)
    locid

type sortid =                                   (* A sort identifier *)
    locid

type dataid =                                   (* A data constructor identifier *)
    locid

type labelid =                                  (* A record label *)
    locid

type typevarid =                                (* An O'Caml type variable *)
    locid

type params =                                   (* A list of (distinct) O'Caml type variables *)
    typevarid list

type declaration =
  | DeclExpType of                              (* An expression type declaration *)
      params *                                  (* The type's parameters *)
      typeid *                                  (* The type's name *)
      expfactor summand list                    (* Its summands *)
  | DeclPatType of                              (* A pattern type declaration *)
      params *                                  (* The type's parameters *)
      typeid *                                  (* The type's name *)
      sortid list *                             (* The list of atom sorts that this type binds *)
      patfactor summand list                    (* Its summands *)
  | DeclSort of                                 (* A sort (of atoms) declaration *)
      sortid                                    (* The sort's name *)
  | DeclContainer of                            (* An external container declaration *)
      typeid *                                  (* The container's name *)
      locid *                                   (* The container's map function *)
      locid *                                   (* The container's fold function *)
      locid                                     (* The container's fold2 function *)
  | DeclIdentifier of                           (* A directive that determines the type of identifiers *)
      locid                                     (* The name of the desired identifier module *)

and 'factor summand =
  | Summand of
      dataid option *                           (* The data constructor name, void if this is a tuple or record type *)
      (labelid option * 'factor) list           (* Its factors; labels are void if this a tuple or sum type *)

and expfactor =
  | EAtom of sortid                             (* An atom, annotated with its sort *)
  | EEscape of standard_type                    (* A standard type *)
  | ETypRef of container * params * typeid      (* A named pattern or expression type *)
  | EAbstraction of params * typeid             (* An abstraction whose body is a named pattern type. *)

and patfactor =
  | PAtom of sortid                             (* An atom, annotated with its sort *)
  | PEscape of standard_type                    (* A standard type *)
  | PTypRef of patmodifier *                    (* A named pattern or expression type *)
               container * params * typeid

and patmodifier =
  | MRef                                        (* A regular reference *)
  | MInner                                      (* A reference in inner scope *)
  | MOuter                                      (* A reference in outer scope *)
  | MNeutral                                    (* A reference in neutral scope *)

and container =
  typeid option                                 (* None if no container, otherwise name of container *)

(* We require the body of an abstraction to be a named type. Indeed,
   we plan to translate abstractions down to abstract types, which
   must be boxed in O'Caml, so allowing the body of an abstraction to
   be a tuple type would introduce the (wrong) illusion that an extra
   indirection is avoided. Also, our use of abstract types will
   require assigning reasonable names to the view functions, and this
   is easier if the body of an abstraction is a named type. *)

(* Requiring the body of inner/outer/neutral to be a named type is
   not important, but does not sound very restrictive either. *)

(* Requiring every type to be annotated with the list of atom sorts
   that it binds is not absolutely essential. In principle, it would
   be sufficient for the user to declare whether a type does bind
   atoms (all of the atom sorts that it contains, not crossing inner,
   outer, or neutral) or doesn't. The set of atom sorts that are bound
   could then be computed automatically, and indeed we do so when
   checking that the user's declarations are consistent. We do require
   declarations because we believe that this improves readability and
   forces the user to think more carefully about lexical structure. *)

(* The above paragraph assumes that all atoms contained in a pattern
   (not crossing inner, outer, or neutral boundaries) are bound. Since
   we require the user to declare which sorts of atoms are bound, we
   could in principle remove this restriction. We do keep it, because
   it is not very restrictive and makes code generation simpler. *)

