(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/package.ml,v 1.3 2000/02/25 15:01:34 fpottier Exp $ *)

(* This module defines package-level operations, such as operations concerning the class hierarchy. *)

(* Still very TEMPORARY *)

module type S = sig

  type jclass (* TEMPORARY other packages assume jclass can be hashed, compared *)

  val arithmeticException: jclass
  val arrayIndexOutOfBoundsException: jclass
  val classCastException: jclass
  val negativeArraySizeException: jclass
  val nullPointerException: jclass
  val securityException: jclass
  val arrayStoreException: jclass

  val root: jclass (* root class, i.e. Object *)

  val subclass: jclass -> jclass -> bool
  val superclass: jclass -> jclass option
  val lcs: jclass -> jclass -> jclass (* least common superclass *)

  type jintf

  type jtype =
    | Prim of JavaCard.prim
    | Intf of jintf
    | Class of jclass
    | Array of jtype

  val classref: JavaCard.cpx -> jclass
  val fieldref: JavaCard.cpx -> jtype

end

