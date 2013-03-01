
(*
 *	Copyright (c) 2002 by Laboratoire Spécification et Vérification (LSV),
 *	CNRS UMR 8643 & ENS Cachan.
 *	Written by Jean Goubault-Larrecq.  Not derived from licensed software.
 *
 *	Permission is granted to anyone to use this software for any
 *	purpose on any computer system, and to redistribute it freely,
 *	subject to the following restrictions:
 *
 *	1. Neither the author nor its employer is responsible for the consequences of use of
 *		this software, no matter how awful, even if they arise
 *		from defects in it.
 *
 *	2. The origin of this software must not be misrepresented, either
 *		by explicit claim or by omission.
 *
 *	3. Altered versions must be plainly marked as such, and must not
 *		be misrepresented as being the original software.
 *
 *	4. This software is restricted to non-commercial use only.  Commercial
 *		use is subject to a specific license, obtainable from LSV.
 *)

open Cparse
open Sizeof

type int_type = { it_signed : bool; (* true si type signe, false si unsigned. *)
		  it_nbits : int; (* nombre de bits dans les elements de ce type. *)
		  it_max : float; (* plus grand nombre du type: = 2^{it_nbits-1}-1
				   si type signe, 2^it_nbits-1 si unsigned. *)
		  it_min : float; (* plus petit nombre du type: = -2^{it_nbits-1}
				   si type signe, 0 si unsigned. *)
		  it_range : float (* nombre d'entiers representables dans ce type = 2^it_nbits. *)
		  }

let type_int =
    let n = sizeof_ctype ((false, false), TINT) * 8 in
	{ it_signed = true;
	  it_nbits = n;
	  it_max = ldexp 1.0 (n-1) -. 1.0;
	  it_min = -. ldexp 1.0 (n-1);
	  it_range = ldexp 1.0 n
	  }

let type_unsigned_int =
    let n = sizeof_ctype ((false, false), TUNSIGNED_INT) * 8 in
	{ it_signed = false;
	  it_nbits = n;
	  it_max = ldexp 1.0 n -. 1.0;
	  it_min = 0.0;
	  it_range = ldexp 1.0 n
	  }

let type_char =
    let n = sizeof_ctype ((false, false), TCHAR) * 8 in
	{ it_signed = true;
	  it_nbits = n;
	  it_max = ldexp 1.0 (n-1) -. 1.0;
	  it_min = -. ldexp 1.0 (n-1);
	  it_range = ldexp 1.0 n
	  }

let type_unsigned_char =
    let n = sizeof_ctype ((false, false), TUNSIGNED_CHAR) * 8 in
	{ it_signed = false;
	  it_nbits = n;
	  it_max = ldexp 1.0 n -. 1.0;
	  it_min = 0.0;
	  it_range = ldexp 1.0 n
	  }

let type_short =
    let n = sizeof_ctype ((false, false), TSHORT) * 8 in
	{ it_signed = true;
	  it_nbits = n;
	  it_max = ldexp 1.0 (n-1) -. 1.0;
	  it_min = -. ldexp 1.0 (n-1);
	  it_range = ldexp 1.0 n
	  }

let type_unsigned_short =
    let n = sizeof_ctype ((false, false), TUNSIGNED_SHORT) * 8 in
	{ it_signed = false;
	  it_nbits = n;
	  it_max = ldexp 1.0 n -. 1.0;
	  it_min = 0.0;
	  it_range = ldexp 1.0 n
	  }

let type_long =
    let n = sizeof_ctype ((false, false), TLONG) * 8 in
	{ it_signed = true;
	  it_nbits = n;
	  it_max = ldexp 1.0 (n-1) -. 1.0;
	  it_min = -. ldexp 1.0 (n-1);
	  it_range = ldexp 1.0 n
	  }

let type_unsigned_long =
    let n = sizeof_ctype ((false, false), TUNSIGNED_LONG) * 8 in
	{ it_signed = false;
	  it_nbits = n;
	  it_max = ldexp 1.0 n -. 1.0;
	  it_min = 0.0;
	  it_range = ldexp 1.0 n
	  }

let type_void_ptr =
    let n = sizeof_ctype ((false, false), TPOINTER ((false, false), TVOID)) * 8 in
	{ it_signed = false;
	  it_nbits = n;
	  it_max = ldexp 1.0 n -. 1.0;
	  it_min = 0.0;
	  it_range = ldexp 1.0 n
	  }

exception IntTypeInfo

let int_type_info (_, typ) =
    match typ with
        TINT -> type_int
      | TUNSIGNED_INT -> type_unsigned_int
      | TCHAR -> type_char
      | TUNSIGNED_CHAR -> type_unsigned_char
      | TSHORT -> type_short
      | TUNSIGNED_SHORT -> type_unsigned_short
      | TLONG -> type_long
      | TUNSIGNED_LONG -> type_unsigned_long
      | _ -> raise IntTypeInfo

let float_is_zero x =
    x=0.0 || x=(-0.0)
(*
        match classify_float x with
	    FP_zero -> true
          | _ -> false
*)

let float_plus_infini = 1.0/.0.0
let float_moins_infini = (-1.0)/.0.0
let float_epsilon =
    (* le plus petit nombre normalise. *)
    let eps = ref 1.0 in
	(while (1.0/.(!eps)<>float_plus_infini) do
	     eps := ldexp (!eps) (-1)
	 done;
	     ldexp (!eps) 1)

let float_is_nan (x:float) = not (x=x)

let round_to_int x = let y = if x<0.0 then ceil x else floor x in
                     if float_is_zero y then 0.0 else y

let round_to_int_range {it_signed=signed; it_max=max; it_min=min; it_range=range} x =
    if signed
	then let x' = mod_float (x -. min) range in
	    let x'' = if x'<0.0 then x' +. range +. min else x' +. min in
		round_to_int x''
    else let x' = mod_float x range in
	 let x'' = if x'<0.0 then x' +. range else x' in
	     floor x''

let round_to_int_type qtyp x = round_to_int_range (int_type_info qtyp) x

let mk_int qtyp x = (qtyp, INT (round_to_int_type qtyp x))

let mk_float qtyp x = (qtyp, if float_is_zero x then FLOAT 0.0 else FLOAT x)

exception WrongType

let rec convert ((_, typ) as qtyp) (_, cst) =
    match typ with
	TINT | TCHAR | TSHORT | TLONG |
	TUNSIGNED_INT | TUNSIGNED_CHAR |
	TUNSIGNED_SHORT | TUNSIGNED_LONG ->
	(match cst with
	     INT x -> mk_int qtyp x
           | FLOAT x -> mk_int qtyp x
	   | _ -> raise WrongType)
      | TFLOAT | TDOUBLE ->
	(match cst with
	     FLOAT _ -> (qtyp, cst)
	   | INT n -> (qtyp, FLOAT n)
           | STRING s -> raise WrongType)
      | TARRAY ((_, TCHAR), _) ->
        (match cst with
	     STRING _ -> (qtyp, cst)
           | _ -> raise WrongType)
      | _ -> raise WrongType

let calc_bnot (qtyp, c) =
    match qtyp, c with
        _, INT n -> mk_int qtyp (-.n -. 1.0)
	(* en complement a deux, ~n=-n-1 *)
      | _ -> raise WrongType

let calc_minus (qtyp, c) =
    match c with
        INT n -> mk_int qtyp (-. n)
      | FLOAT x -> mk_float qtyp (-. x)
      | _ -> raise WrongType

let calc_add c1 c2 =
    match c1, c2 with
	(qtyp, INT n1), (_, INT n2) -> mk_int qtyp (n1 +. n2)
      | (qtyp, FLOAT x1), (_, FLOAT x2) -> mk_float qtyp (x1 +. x2)
      | _, _ -> raise WrongType

let calc_sub c1 c2 =
    match c1, c2 with
	(qtyp, INT n1), (_, INT n2) -> mk_int qtyp (n1 -. n2)
      | (qtyp, FLOAT x1), (_, FLOAT x2) -> mk_float qtyp (x1 -. x2)
      | _, _ -> raise WrongType

let calc_mul c1 c2 =
    match c1, c2 with
	(qtyp, INT n1), (_, INT n2) -> mk_int qtyp (n1 *. n2)
      | (qtyp, FLOAT x1), (_, FLOAT x2) -> mk_float qtyp (x1 *. x2)
      | _, _ -> raise WrongType

let calc_div c1 c2 =
    match c1, c2 with
	(qtyp, INT n1), (_, INT n2) -> mk_int qtyp (n1 /. n2)
      | (qtyp, FLOAT x1), (_, FLOAT x2) -> mk_float qtyp (x1 /. x2)
      | _, _ -> raise WrongType

let calc_mod c1 c2 =
    match c1, c2 with
	(qtyp, INT n1), (_, INT n2) -> mk_int qtyp (mod_float n1 n2)
      | (qtyp, FLOAT x1), (_, FLOAT x2) -> mk_float qtyp (mod_float x1 x2)
      | _, _ -> raise WrongType

let calc_lt c1 c2 =
    match c1, c2 with
	(qtyp, INT n1), (_, INT n2) -> mk_int ((false, false), TINT) (if n1<n2 then 1.0 else 0.0)
      | (qtyp, FLOAT x1), (_, FLOAT x2) -> mk_int ((false, false), TINT) (if x1<x2 then 1.0 else 0.0)
      | _, _ -> raise WrongType

let calc_le c1 c2 = 
    match c1, c2 with
	(qtyp, INT n1), (_, INT n2) -> mk_int ((false, false), TINT) (if n1<=n2 then 1.0 else 0.0)
      | (qtyp, FLOAT x1), (_, FLOAT x2) -> mk_int ((false, false), TINT) (if x1<=x2 then 1.0 else 0.0)
      | _, _ -> raise WrongType

let calc_eq c1 c2 =
    match c1, c2 with
	(qtyp, INT n1), (_, INT n2) -> mk_int ((false, false), TINT) (if float_is_zero (n1-.n2) then 1.0 else 0.0)
      | (qtyp, FLOAT x1), (_, FLOAT x2) -> mk_int ((false, false), TINT) (if float_is_zero (x1-.x2) then 1.0 else 0.0)
      | (qtyp, STRING s1), (_, STRING s2) -> mk_int ((false, false), TINT) (if s1=s2 then 1.0 else 0.0)
      | _, _ -> raise WrongType

let calc_is_zero c =
    match c with
        (_, INT n) -> float_is_zero n
      | (_, FLOAT x) -> float_is_zero x
      | _ -> raise WrongType

let calc_mod_nbits {it_nbits=n} x =
    (* ramene x entre 0 et n-1, par modulo.
     On ne peut pas simplement utiliser mod_float, par exemple
     mod_float (-30.0) 32.0 donne -30.0, alors qu'on souhaite
     que ceci donne 2.0. *)
    let n' = float_of_int n in
    round_to_int_range {it_signed=false; it_nbits=0 (* bidon; OK car pas utilise
						     par round_to_int_range. *);
			 it_max=n'-.1.0; it_min=0.0; it_range=n'} x

let calc_left (qtyp, c) (_, c') =
    (* x<<y calcule x*2^{y mod n}, ou x fait n bits,
     et y est considere comme non signe, i.e., 0<=y mod n<n. *)
    let {it_min=c_minint; it_max=c_maxint} as it = int_type_info qtyp in
    match c, c' with
	INT n, INT n' ->
	let y = calc_mod_nbits it n' in
	    mk_int qtyp (ldexp n (int_of_float y))
      | _, _ -> raise WrongType

let calc_right (qtyp, c) (_, c') =
    (* x>>y calcule x asr y = floor (x/2^{y mod n}), ou x fait n bits,
     et y est considere comme non signe, i.e., 0<=y mod n<n.
     Par contre, on ne peut pas utiliser la division calc_div, car l'arrondi
     est fait vers le bas quel que soit le signe de x, contrairement a calc_div.
     Par exemple,
     31 / 4 = 7 et 31 asr 2 = 7 (pareil, car 31 est positif);
     -31 / 4 = -7 et -31 asr 2 = -8 (arrondi different). *)
    let {it_min=c_minint; it_max=c_maxint} as it = int_type_info qtyp in
    match c, c' with
	INT n, INT n' ->
	let y = calc_mod_nbits it n' in
	    mk_int qtyp (floor (ldexp n (-int_of_float n')))
      | _, _ -> raise WrongType

(* Il est malaise de calculer les operations bit a bit (and, xor, or)
 sur des flottants.  On coupe les flottants en octets et on effectue les
 operations sur chaque octet separement en convertissant en entier. *)
let rec calc_bit_a_bit_1 nbits f x y =
    (* x, y >= 0.0 *)
    if nbits<0
	then 0.0
    else let i = int_of_float (mod_float x 256.0) in
	 let x' = floor (ldexp x (-8)) in
	 let j = int_of_float (mod_float y 256.0) in
	 let y' = floor (ldexp y (-8)) in
	     256.0 *. calc_bit_a_bit_1 (nbits-8) f x' y'
	     +. float_of_int (f i j)

let calc_bit_a_bit {it_nbits=n; it_signed=s; it_max=m; it_range=r} f x y =
    if s
	then let x' = if x<0.0 then x+.r else x in
	     let y' = if y<0.0 then y+.r else y in
	     let z = calc_bit_a_bit_1 n f x' y' in
		 if z>m then z-.r else z
    else calc_bit_a_bit_1 n f x y

let calc_and (qtyp, c) (_, c') =
    match c, c' with
	INT n, INT n' ->
	mk_int qtyp (calc_bit_a_bit (int_type_info qtyp) (land) n n')
      | _ -> raise WrongType

let calc_xor (qtyp, c) (_, c') =
    match c, c' with
	INT n, INT n' ->
	mk_int qtyp (calc_bit_a_bit (int_type_info qtyp) (lxor) n n')
      | _ -> raise WrongType

let calc_or (qtyp, c) (_, c') =
    match c, c' with
	INT n, INT n' ->
	mk_int qtyp (calc_bit_a_bit (int_type_info qtyp) (lor) n n')
      | _ -> raise WrongType
