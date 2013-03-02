(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/world.ml,v 1.12 2000/03/04 07:07:26 fpottier Exp $ *)

(* This module allows loading a number of JavaCard ``export'' files, plus (at most) one ``CAP'' file, and managing the
   information contained therein. *)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{References} *)

(* As far as clients of this module are concerned, a reference to a class/interface/field/method is of an abstract
   type.

   This type is usually implemented as a disjoint sum. Entities defined by an export file are described by the export
   file itself, together with an appropriate auxiliary structure. Entities defined by a CAP file are described by the
   CAP file itself, together with appropriate auxiliary information. *)

type ('a, 'b) reference =
    | RefExport of Export.file * 'a
    | RefCAP of CAP.file * 'b

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Classes and interfaces} *)

module CIDesc = struct

  type t = (Export.class_info, CAP.internal_class_reference) reference

  (* This function allows comparing elements of type [t], so as to determine whether they refer to the same
     class/interface.

     Its implementation relies on the fact that a single package cannot be described by two distinct files. This means
     there is only one class description structure per class in memory; hence, physical equality may be used. *)

  let equal cidesc1 cidesc2 =
    match (cidesc1, cidesc2) with
    | RefExport (_, info1), RefExport (_, info2) ->
	info1 == info2
    | RefCAP (_, ref1), RefCAP (_, ref2) ->
	ref1 == ref2
    | _, _ ->
	false

  (* This function associates a hash code to an element of type [t]. It relies on the fact that classes/interfaces
     defined in export files have unique (fully qualified) names. It also uses the fact that only one CAP file is
     loaded. *)

  let hash = function
    | RefExport (file, info) ->
	Hashtbl.hash (Export.class_name file info)
    | RefCAP (_, ref) ->
	CAP.hash ref

  (* This auxiliary function prints a human-readable name for a class/interface. It is used for debugging. *)

  let human = function
    | RefExport (file, info) ->
	Export.class_name file info
    | RefCAP (_, ref) ->
	CAP.Human.icref ref

end

type jclass = CIDesc.t
type jintf  = CIDesc.t
type jci    = CIDesc.t

(* These functions maintain a mapping from fully qualified class/interface names to class/interface descriptions.
   Not all classes have known full names, because the names of the classes defined in CAP files are unavailable. *)

let class_table =
  Hashtbl.create 1023

let add_named_class =
  Hashtbl.add class_table

let find_named_class name =
  try
    Hashtbl.find class_table name
  with Not_found ->
    Printf.eprintf "Unknown class or interface: %s\n" name;
    flush stderr;
    assert false

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Packages} *)

(* Information about a package is provided either by an export file, or by a CAP file, but never both. *)

type package = (unit, unit) reference

(* This auxiliary function prints a human-readable description of a [package] structure. *)

let print_package package =
  Printf.sprintf "Package defined in %s\n"
    (match package with
    | RefExport (exportfile, ()) ->
	Export.filename exportfile
    | RefCAP (capfile, ()) ->
	CAP.filename capfile)

(* These functions maintain a mapping from package AIDs to package information structures. *)

let package_table =
  Hashtbl.create 1023

let add_package aid package =
  try
    let package' = Hashtbl.find package_table aid in
    prerr_endline "Two packages have conflicting AIDs, or a package has been loaded twice.";
    Printf.eprintf "AID: %s\n" (Aid.print aid);
    prerr_string (print_package package');
    prerr_string (print_package package);
    flush stderr;
    assert false
  with Not_found ->
    Hashtbl.add package_table aid package

let internal_find_package =
  Hashtbl.find package_table

let find_package aid =
  try
    internal_find_package aid
  with Not_found ->
    Printf.eprintf "Unknown package AID: %s\n" (Aid.print aid);
    flush stderr;
    assert false

(* [load_export exportname] loads the package described by the export file named [exportname]. *)

let load_export exportname =

  (* Load the export file. Determine the package's AID. Register the package. *)

  let exportfile = Export.load exportname in
  let aid = Export.package_aid exportfile in
  add_package aid (RefExport (exportfile, ()));

  (* Register the classes/interfaces defined by this package. *)

  List.iter (fun info ->
    let name = Export.class_name exportfile info in
    add_named_class name (RefExport (exportfile, info))
  ) exportfile.Export.classes

(* [load_cap capname packname] opens the CAP file named [capname] and loads the package named [packname] from it. *)

let load_cap =
  let called = ref false in

  let load_cap capname packname =

    (* Prevent the user from loading more than one CAP file. *)

    if !called then begin
	prerr_endline "At most one CAP file must be loaded.";
	assert false
    end;
    called := true;

    (* Load the CAP file. Determine the package's AID. Register the package. *)

    let capfile = CAP.load capname packname in
    let aid = CAP.package_aid capfile in
    add_package aid (RefCAP (capfile, ()));

    (* Make sure all required packages have been loaded first. *)

    CAP.imports (fun aid ->
      try
	let _ = internal_find_package aid in
	()
      with Not_found ->
	Printf.eprintf "Package %s requires package" packname;
	Printf.eprintf " %s to be loaded first.\n" (Aid.print aid);
	flush stderr;
	assert false
    ) capfile in

  load_cap

(* This functor returns a structure containing a series of standard class references. It must be called by the client
   \emph{after} the standard libraries have been loaded. This is a slightly fragile mechanism. Furthermore, it assumes
   that the standard libraries are described in export files, which prevents us from analyzing their CAP files. *)

module JavaLang (X : sig end) = struct

  let root =                           find_named_class "java/lang/Object"
  let arithmeticException =            find_named_class "java/lang/ArithmeticException"
  let arrayIndexOutOfBoundsException = find_named_class "java/lang/ArrayIndexOutOfBoundsException"
  let arrayStoreException =            find_named_class "java/lang/ArrayStoreException"
  let classCastException =             find_named_class "java/lang/ClassCastException"
  let negativeArraySizeException =     find_named_class "java/lang/NegativeArraySizeException"
  let nullPointerException =           find_named_class "java/lang/NullPointerException"
  let securityException =              find_named_class "java/lang/SecurityException"

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{More references} *)

type jfield =
    (Export.field_info, CAP.internal_field_reference) reference

type jmethod =
    (Export.method_info, CAP.internal_method_reference) reference

module Resolve = struct

  (* This auxiliary function looks for an export file defining package AID [aid], and resolves the given [class_token]
     in it. *)

  let aid_class_token aid class_token =
    match find_package aid with
    | RefExport (exportfile, ()) ->
	exportfile, Export.resolve exportfile class_token
    | RefCAP _ ->
	assert false

  (* This function converts [CAP.class_reference] values to [jci] format. *)

  let classref capfile = function
    | CAP.Internal cref ->
	RefCAP (capfile, cref)
    | CAP.External (aid, class_token, ()) ->

        (* The reference points to another package. Because we assume at most one CAP file is loaded at a time, this
	   package must be described by an export file. *)

	let exportfile, info = aid_class_token aid class_token in
	RefExport (exportfile, info)

  (* This function converts [CAP.field_reference] values to [jfield] format. *)

  let field_ref capfile = function
    | CAP.Internal ifref ->
	RefCAP (capfile, ifref)
    | CAP.External (aid, class_token, field_token) ->
	let exportfile, info = aid_class_token aid class_token in
	RefExport (exportfile, Export.resolve_field exportfile info field_token)

  (* This function converts [CAP.method_reference] values to [jmethod] format. *)

  let method_ref capfile = function
    | CAP.Internal imref ->
	RefCAP (capfile, imref)
    | CAP.External (aid, class_token, method_token) ->
	let exportfile, info = aid_class_token aid class_token in
	RefExport (exportfile, Export.resolve_method exportfile info method_token)

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Sub-class relationships} *)

(* [superclass] returns the (unique) superclass of the specified class, it it has one, and [None] otherwise. *)

let superclass = function
  | RefExport (file, info) ->
      Standard.map_option find_named_class (Export.superclass file info)
  | RefCAP (file, ref) ->
      Standard.map_option (Resolve.classref file) (CAP.superclass file ref)

(* [ancestors] behaves exactly as a [fold] on the specified class' ancestor list. Nearest ancestors are visited first;
   in particular, the specified class becomes the first argument to the [action] function. *)

let rec ancestors action cidesc accu =
  Standard.fold_option (ancestors action) (superclass cidesc) (action cidesc accu)

(* [subclass class1 class2] returns [true] if and only if [class1] is a sub-class of [class2]. This includes the case
   where both classes are equal.

   Its implementation is not particularly efficient, but that should do for the moment. *)

exception Yes

let subclass cidesc1 cidesc2 =
  try
    ancestors (fun ancestor () ->
      if CIDesc.equal ancestor cidesc2 then
	raise Yes
    ) cidesc1 ();
    false
  with Yes ->
    true

(* [lcs class1 class2] returns the least common superclass of [class1] and [class2]. *)

exception Got of jclass

let lcs cidesc1 cidesc2 =
  let ancestors1 = ancestors (fun x l -> x :: l) cidesc1 [] in
  try
    ancestors (fun ancestor () ->
      if List.exists (CIDesc.equal ancestor) ancestors1 then
	raise (Got ancestor)
    ) cidesc2 ();
    Printf.eprintf
      "These classes have no common ancestor: %s and %s\n"
      (CIDesc.human cidesc1) (CIDesc.human cidesc2);
    flush stderr;
    assert false
  with Got it ->
    it

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Methods} *)

(* These functions map a field/method to its class/interface. *)

let method_owner = function
  | RefExport (file, method_info) ->
      RefExport (file, method_info.Export.method_owner)
  | RefCAP (file, imref) ->
      RefCAP (file, CAP.method_owner imref)

let field_owner = function
  | RefExport (file, field_info) ->
      RefExport (file, field_info.Export.field_owner)
  | RefCAP (file, imref) ->
      RefCAP (file, CAP.field_owner imref)

(* These auxiliary functions print a human-readable name for a field/method. It is used for debugging/disassembling. *)

let method_simple_name = function
  | RefExport (file, method_info) ->
      Export.method_name file method_info
  | RefCAP (_, imref) ->
      CAP.Human.simple_imref imref

let field_simple_name = function
  | RefExport (file, field_info) ->
      Export.field_name file field_info
  | RefCAP (_, ifref) ->
      CAP.Human.simple_ifref ifref

let method_human jmethod =
  Printf.sprintf "%s.%s"
    (CIDesc.human (method_owner jmethod))
    (method_simple_name jmethod)

let field_human jfield =
  Printf.sprintf "%s.%s"
    (CIDesc.human (field_owner jfield))
    (field_simple_name jfield)

(* This function lists the methods of a class/interface. *)

let methods action = function
  | RefCAP (file, icref) ->
      CAP.methods (fun imref ->
	action (RefCAP (file, imref))
      ) icref
  | RefExport (file, info) ->
      List.iter (fun method_info ->
	action (RefExport (file, method_info))
      ) info.Export.methods

(* This functor returns a description of the specified method. The method must belong to a class (not an interface)
   defined in the current CAP file. It must be a concrete (i.e. not abstract) method. *)

module MethodCode (X : sig

  val jmethod: jmethod

end) = struct

  let capfile, imref =
    match X.jmethod with
    | RefCAP (capfile, imref) ->
	capfile, imref
    | RefExport _ ->
	assert false

  module Code = CAP.MethodCode (struct
    let capfile = capfile
    let desc = imref
  end)

  type node = Code.node
  let n = Code.n
  let index = Code.index
  let successors = Code.successors
  let iter = Code.iter
  let start = Code.start
  let locals = Code.locals

  (* Let us rewrite opcodes to further resolve references. *)

  let opcode node =
    let id branch =
      branch
    and resolve_jci =
      Resolve.classref capfile
    and resolve_field =
      Resolve.field_ref capfile
    and resolve_method =
      Resolve.method_ref capfile in

    CAP.map_opcode (id,
		    resolve_jci,
		    resolve_jci,
		    resolve_jci,
		    resolve_field,
		    resolve_method) (Code.opcode node)

  (* Disassembling. Used for debugging. *)

  open CAP

  let disassemble_prim = function
    | PrimBoolean ->
	"boolean"
    | PrimByte ->
	"byte"
    | PrimShort ->
	"short"
    | PrimInt ->
	"int"

  let disassemble_absi = function
    | AbsiA ->
	"a"
    | AbsiB ->
	"b"
    | AbsiS ->
	"s"
    | AbsiI ->
	"i"

  let disassemble_asi = function
    | AsiA ->
	"a"
    | AsiS ->
	"s"
    | AsiI ->
	"i"

  let disassemble_si = function
    | SiS ->
	"s"
    | SiI ->
	"i"

  let disassemble_jarray = function
    | ArrayPrim prim ->
	"[" ^ (disassemble_prim prim) ^ "]"
    | ArrayRef jci ->
	"[" ^ (CIDesc.human jci) ^ "]"

  let disassemble_check = function
    | CheckClass jci ->
	CIDesc.human jci
    | CheckArray jarray ->
	disassemble_jarray jarray

  let disassemble_this = function
    | UseThis ->
	"_this"
    | UseStack ->
	""

  let disassemble_condition = function
    | Acmpeq ->
	"acmpeq"
    | Acmpne ->
	"acmpne"
    | Scmpeq ->
	"scmpeq"
    | Scmpne ->
	"scmpne"
    | Scmplt ->
	"scmplt"
    | Scmpge ->
	"scmpge"
    | Scmpgt ->
	"scmpgt"
    | Scmple ->
	"scmple"
    | Eq ->
	"eq"
    | Ne ->
	"ne"
    | Lt ->
	"lt"
    | Ge ->
	"ge"
    | Gt ->
	"gt"
    | Le ->
	"le"
    | Nonnull ->
	"nonnull"
    | Null ->
	"null"

  let disassemble node =
    let opcode = match opcode node with
    | Aconst_null ->
	"aconst_null"
    | Arraylength ->
	"arraylength"
    | Arrayload absi ->
	(disassemble_absi absi) ^ "aload"
    | Arraystore absi ->
	(disassemble_absi absi) ^ "astore"
    | Athrow ->
	"athrow"
    | Checkcast check ->
	"checkcast " ^ (disassemble_check check)
    | Dup ->
	"dup"
    | Dup_x (m, n) ->
	Printf.sprintf "dup_x %d,%d" m n
    | Dup2 ->
	"dup2"
    | Getfield (absi, this, jfield) ->
	Printf.sprintf "getfield_%s%s %s"
	  (disassemble_absi absi)
	  (disassemble_this this)
	  (field_human jfield)
    | Getstatic (absi, jfield) ->
	Printf.sprintf "getstatic_%s %s"
	  (disassemble_absi absi)
	  (field_human jfield)
    | Goto node ->
	Printf.sprintf "goto %d" (index node)
    | I2b ->
	"i2b"
    | I2s ->
	"i2s"
    | Iadd ->
	"iadd"
    | Iand ->
	"iand"
    | Icmp ->
	"icmp"
    | Idiv ->
	"idiv"
    | If (condition, node) ->
	Printf.sprintf "if%s %d"
	  (disassemble_condition condition)
	  (index node)
    | Iinc (local_index, k) ->
	Printf.sprintf "iinc %d %d" local_index k
    | Imul ->
	"imul"
    | Ineg ->
	"ineg"
    | Instanceof check ->
	"instanceof " ^ (disassemble_check check)
    | Invokeinterface (nargs, jintf, method_token) ->
	Printf.sprintf "invokeinterface %d, %s, %d"
	  nargs
	  (CIDesc.human jintf)
	  method_token
    | Invokespecial ssm ->
	"invokespecial " ^ (method_human ssm)
    | Invokestatic sm ->
	"invokestatic " ^ (method_human sm)
    | Invokevirtual vm ->
	"invokevirtual " ^ (method_human vm)
    | Ior ->
	"ior"
    | Ipush k ->
	Printf.sprintf "ipush %d" k
    | Irem ->
	"irem"
    | Ishl ->
	"ishl"
    | Ishr ->
	"ishr"
    | Isub ->
	"isub"
    | Iushr ->
	"iushr"
    | Ixor ->
	"ixor"
    | Jsr node ->
	Printf.sprintf "jsr %d" (index node)
    | Load (asi, local_index) ->
	Printf.sprintf "%sload %d"
	  (disassemble_asi asi)
	  local_index
    | Lookupswitch (si, default, cases) ->
	let basic = Printf.sprintf "%slookupswitch\n       default: %d"
	    (disassemble_si si)
	    (index default) in
	List.fold_left (fun accu (key, target) ->
	  accu ^ (Printf.sprintf "\n       case %d: %d" key (index target))
        ) basic cases
    | New jclass ->
	"new " ^ (CIDesc.human jclass)
    | Newarray jarray ->
	"newarray " ^ (disassemble_jarray jarray)
    | Nop ->
	"nop"
    | Pop ->
	"pop"
    | Pop2 ->
	"pop2"
    | Putfield (absi, this, jfield) ->
	Printf.sprintf "putfield_%s%s %s"
	  (disassemble_absi absi)
	  (disassemble_this this)
	  (field_human jfield)
    | Putstatic (absi, jfield) ->
	Printf.sprintf "putstatic_%s %s"
	  (disassemble_absi absi)
	  (field_human jfield)
    | Ret local_index ->
	Printf.sprintf "ret %d" local_index
    | Return None ->
	"return"
    | Return (Some asi) ->
	(disassemble_asi asi) ^ "return"
    | S2b ->
	"s2b"
    | S2i ->
	"s2i"
    | Sadd ->
	"sadd"
    | Sand ->
	"sand"
    | Sdiv ->
	"sdiv"
    | Sinc (local_index, k) ->
	Printf.sprintf "sinc %d %d" local_index k
    | Smul ->
	"smul"
    | Sneg ->
	"sneg"
    | Sor ->
	"sor"
    | Spush k ->
	Printf.sprintf "spush %d" k	
    | Srem ->
	"srem"
    | Sshl ->
	"sshl"
    | Sshr ->
	"sshr"
    | Ssub ->
	"ssub"
    | Store (asi, local_index) ->
	Printf.sprintf "%sstore %d"
	  (disassemble_asi asi)
	  local_index
    | Sushr ->
	"sushr"
    | Swap_x (m, n) ->
	Printf.sprintf "swap_x %d,%d" m n
    | Sxor ->
	"sxor"
    | Tableswitch (si, default, lo, hi, table) ->
	let basic = Printf.sprintf "%stableswitch\n       default: %d"
	    (disassemble_si si)
	    (index default) in
	fst (Array.fold_left (fun (accu, key) target ->
	  accu ^ (Printf.sprintf "\n       case %d: %d"
		     key
		     (index target)),
	  key + 1
        ) (basic, lo) table) in

    Printf.sprintf "%3d: %s\n" (index node) opcode

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{TEMPORARY Testing} *)

let test () = (* TEMPORARY test *)

  (*Hashtbl.iter (fun name cidesc ->  
    Printf.printf "Class %s\n" name;
    methods (function RefExport (exportfile, method_info) ->
      Printf.printf "Method %s\n" (Export.method_name exportfile method_info)
    ) cidesc
  ) class_table;
  flush stdout;*)

  Hashtbl.iter (fun aid package ->
    match package with
    | RefExport _ ->
	()
    | RefCAP (capfile, ()) ->
	CAP.classes (fun icref ->
	  let cidesc = RefCAP (capfile, icref) in
	  Printf.printf "%s\n" (CIDesc.human cidesc);
	  flush stdout;
	  methods (fun jmethod ->
	    Printf.printf "    %s\n" (method_human jmethod);
	    flush stdout;
	    let module Code = MethodCode (struct let jmethod = jmethod end) in
	    Code.iter (fun node ->
	      print_string (Code.disassemble node);
	      flush stdout
            );
	    flush stdout
          ) cidesc
	) capfile
  ) package_table 

