(* $Id: pprintExt.ml 57 2007-10-01 14:55:17Z yann.regisgianas $ *)

module B = Buffer

open Pprint
let beside	    = (^^)
let separate z x y  = x ^^ z ^^ y
let (++)            = separate space
let (//)            = separate softline
let (///)           = separate softbreak
let ($)             = separate line
let ($$)            = separate linebreak
let fold f docs     = List.fold_right f docs empty
let fillCat         = fold (///)
let hcat            = fold (^^)
let vcat            = fold ($$)
let cat docs        = group (vcat docs)

let encloseSep left right sep ds =
  match ds with
  | [] ->
      left ^^ right
  | [ d ] ->
      left ^^ d ^^ right
  | d :: ds ->
      left ^^ 
	align (
	  group (fold ($$) (d :: (List.map (beside sep) ds)))) ^^ right

let tupled s = 
  encloseSep lparen rparen (comma ^^ space) s

let tupled' f ts =
  tupled (List.map f ts)

let comma_sep s = 
  encloseSep empty empty (comma ^^ space) s

let sep x s = 
  encloseSep empty empty x s

let comma_sep' f ts =
  comma_sep (List.map f ts)

let paren_sep sep = 
  encloseSep lparen rparen sep

let brack_sep sep = 
  encloseSep lbracket rbracket sep

let space_sep ds =
  (* this should be that: encloseSep empty empty space *)
  sepmap space (fun x -> x) ds

let space_sep' f ds = 
  sepmap space f ds

let paren d = 
  lparen ^^ d ^^ rparen

let bracket d = 
  lbracket ^^ d ^^ rbracket

let as_string d = 
  let b = B.create 13 in
    Buffer.pretty 0.8 120 b d;
    B.contents b  
      
