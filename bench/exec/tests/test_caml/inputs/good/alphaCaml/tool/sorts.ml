open Strings
open IL
open Bits

(* An ordered list of sorts. *)

type sort =
    string

let sorts : sort list =
  List.rev (
    StringSet.fold (fun sort sorts ->
      sort :: sorts
    ) Kind.sorts []
  )

(* Declaration of the heterogeneous product type of sorts. *)

let sortfield sort =
  sort

let sorttyvar sort =
  sort

let sortfields =
  List.map (fun sort ->
    sortfield sort, TypVar (sorttyvar sort)
  ) sorts

let sortparams =
  List.map sorttyvar sorts

let sortrecdef = StructTypeDefs [{
  typename = "sortrec";
  typeparams = sortparams;
  typerhs = Some (ADT.Record sortfields)
}]

let sortrec (params : typ list) : typ =
  tapp "sortrec" params

(* Declaration of the homogeneous product type of sorts. *)

let mapdef = StructTypeDefs [{
  typename = "map";
  typeparams = [ "a" ];
  typerhs = Some (ADT.Tuple [sortrec (List.map (fun _ -> TypVar "a") sorts)])
}]

let map (t : typ) : typ =
  TypApp (PathDot (PathVar "Sort", "map"), [ t ])

(* Declaration of the sum type of sorts. *)

let sortcon sort =
  String.capitalize sort

let dsortcon sort =
  EData (PathVar (sortcon sort), [])

let sortcons =
  List.map (fun sort ->
    sortcon sort, []
  ) sorts

let keydef = StructTypeDefs [{
  typename = "key";
  typeparams = [];
  typerhs = Some (ADT.Sum sortcons)
}]

let key : typ =
  tcon "key"

(* Declaration of a parameterized type of sort-tagged data. *)

let taggeddef = StructTypeDefs [{
  typename = "tagged";
  typeparams = [ "a" ];
  typerhs = Some (ADT.Tuple [ key; TypVar "a" ])
}]

let tagged (t : typ) : typ =
  TypApp (PathDot (PathVar "Sort", "tagged"), [ t ])

(* Building a [sortrec] value. *)

let mksortrec (f : sort -> expr) : expr =
  ERecord (List.map (fun sort -> (sortfield sort, f sort)) sorts)

(* [uniform : 'a -> 'a map]. *)

let uniform =
  mkfun "uniform" [ vvar "x" ] (mksortrec (fun _ -> vvar "x"))

(* [get : key -> 'a map -> 'a]. *)

let get =
  mkfun "get" [ vvar "key"; vvar "m" ] (
    EMatch (vvar "key",
      List.map (
	fun sort -> {
	  branchpat = dsortcon sort;
	  branchbody = ERecordAccess (vvar "m", sortfield sort)
	}
      ) sorts
    )
  )
				
(* [update : key -> 'a map -> ('a -> 'a) -> 'a map]. *)

let update =
  mkfun "update" [ vvar "key"; vvar "m"; vvar "k" ] (
    EMatch (vvar "key",
      List.map (
	fun sort -> {
	  branchpat = dsortcon sort;
	  branchbody =
	    ERecordUpdate (vvar "m", [ sortfield sort, EApp (vvar "k", ERecordAccess (vvar "m", sortfield sort))])
        }
      ) sorts
    )
  )
				
(* [update2 : key -> 'a map * 'a map -> ('a * 'a -> 'a * 'a) -> 'a map * 'a map]. *)

let update2 =
  mkfun "update2" [ vvar "key"; vpair "m1" "m2"; vvar "k" ] (
    EMatch (vvar "key",
      List.map (
	fun sort -> {
	  branchpat = dsortcon sort;
	  branchbody =
	    ELet (
	      vpair "x1" "x2",
	      EApp (
	        vvar "k",
	        pair
	          (ERecordAccess (vvar "m1", sortfield sort))
	          (ERecordAccess (vvar "m2", sortfield sort))
              ),
              pair
	        (ERecordUpdate (vvar "m1", [ sortfield sort, vvar "x1" ]))
	        (ERecordUpdate (vvar "m2", [ sortfield sort, vvar "x2" ]))
            )
        }
      ) sorts
    )
  )
				
(* [fold : ('a -> 'a -> 'b) -> 'a -> 'a -> 'b map]. *)

let fold =
  mkfun "fold" [ vvar "f"; mksortrec vvar; vvar "accu" ] (
    List.fold_right (fun sort code ->
      ELet (
        vvar "accu",
        EApp (EApp (vvar "f", vvar sort), vvar "accu"),
        code
      )
    ) sorts (vvar "accu")
  )

(* [map2 : ('a -> 'b -> 'c) -> 'a map -> 'b map -> 'c map]. *)

let map2 =
  mkfun "map2" [ vvar "f"; mksortrec vvar1; mksortrec vvar2 ] (
    mksortrec (fun sort ->
      EApp (EApp (vvar "f", vvar1 sort), vvar2 sort)
    )
  )

(* [pipe : key -> ('a tagged -> 'b tagged) -> 'a -> 'b]. *)

let pipe =
  mkfun "pipe" [ vvar "key"; vvar "f"; vvar "x" ] (
    ELet (
      vpair "_key" "y",
      EApp (vvar "f", vpair "key" "x"),
      vvar "y"
    )
  )

(* [select : ('a -> 'b) map -> 'a tagged -> 'b tagged]. *)

let select =
  mkfun "select" [ vvar "f"; vpair "key" "x" ] (
    pair
      (vvar "key")
      (EApp (EApp (EApp (vvar "get", vvar "key"), vvar "f"), vvar "x"))
  )

(* Group all of these in a module definition. *)

let sortmoduledef =
  StructModuleDef ("Sort", MEStruct [
    keydef;
    taggeddef;
    mapdef;
    uniform;
    get;
    update;
    update2;
    fold;
    map2;
    pipe;
    select;
  ])

(* Export our definitions. *)

let defs = [
  StructComment "Records whose fields are indexed by sorts.";
  sortrecdef;
  sortmoduledef;
]  

let sortcon sort =
  EData (PathDot (PathVar "Sort", sortcon sort), [])

