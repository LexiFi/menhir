(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/cAP.ml,v 1.16 2000/03/04 07:07:26 fpottier Exp $ *)

(* This module allows reading in and parsing JavaCard ``CAP'' files. *)

(* Indices into several tables or components are given abstract types, so as to prevent errors. *)

type package_token =
    int

type class_token =
    int

type field_token =
    int

type method_token =
    int

type method_component_info_offset =
    int

type class_component_info_offset =
    int

type static_field_image_offset =
    int

type cpool_index =
    int

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Instruction set} *)

(* We give a definition of the instruction set which is parameterized over a large number of type variables. This
   allows several variants of the instruction set, where branch offsets, class references, etc. are implemented
   differently. *)

(* The type of local variable indices. *)

type local_index = int

(* Various instruction parameters. *)

type prim =
  | PrimBoolean
  | PrimByte
  | PrimShort
  | PrimInt

type 'jci jarray =
  | ArrayPrim of prim
  | ArrayRef of 'jci

type 'jci check =
  | CheckArray of 'jci jarray
  | CheckClass of 'jci

type this =
  | UseThis
  | UseStack

type absi =
  | AbsiA
  | AbsiB
  | AbsiS
  | AbsiI

type asi =
  | AsiA
  | AsiS
  | AsiI

type si =
  | SiS
  | SiI

type condition =
  | Acmpeq
  | Acmpne
  | Scmpeq
  | Scmpne
  | Scmplt
  | Scmpge
  | Scmpgt
  | Scmple
  | Eq
  | Ne
  | Lt
  | Ge
  | Gt
  | Le
  | Nonnull
  | Null

(* The type of instructions. The type variable ['branch] stands for the type of branch targets. ['jci] stands for the
   type of class/interface references. ['jclass] stands for the type of class references. ['jintf] stands for the type
   of interface references. ['jf] stands for the type of field references. ['jm] stands for the type of method
   references. *)

type ('branch, 'jci, 'jclass, 'jintf, 'jf, 'jm) opcode =
  | Aconst_null
  | Arraylength
  | Arrayload of absi
      (* includes \verb+aaload+, \verb+baload+, \verb+iaload+, \verb+saload+ *)
  | Arraystore of absi
      (* includes \verb+aastore+, \verb+bastore+, \verb+iastore+, \verb+sastore+ *)
  | Athrow
  | Checkcast of 'jci check
  | Dup
  | Dup_x of int * int
  | Dup2
  | Getfield of absi * this * 'jf
      (* includes \verb+getfield_<t>+, \verb+getfield_<t>_this+ and \verb+getfield_<t>_w+ *)
  | Getstatic of absi * 'jf
  | Goto of 'branch
      (* includes \verb+goto+ and \verb+goto_w+ *)
  | I2b
  | I2s
  | Iadd
  | Iand
  | Icmp
  | Idiv
  | If of condition * 'branch
      (* includes \verb+if_acmp<cond>+, \verb+if_scmp<cond>+, \verb+if<cond>+, \verb+ifnonull+, \verb+ifnull+
	 and their \verb+_w+ variants *)
  | Iinc of local_index * int
      (* includes \verb+iinc+ and \verb+iinc_w+ *)
  | Imul
  | Ineg
  | Instanceof of 'jci check
  | Invokeinterface of int * 'jintf * method_token
  | Invokespecial of 'jm
  | Invokestatic of 'jm
  | Invokevirtual of 'jm
  | Ior
  | Ipush of int
      (* includes \verb+bipush+, \verb+sipush+, \verb+iipush+, \verb+iconst_<i>+ *)
  | Irem
  | Ishl
  | Ishr
  | Isub
  | Iushr
  | Ixor
  | Jsr of 'branch
  | Load of asi * local_index
      (* includes \verb+aload+, \verb+iload+, \verb+sload+, and their \verb+_<n>+ variants *)
  | Lookupswitch of si * 'branch * (int * 'branch) list
      (* includes \verb+ilookupswitch+ and \verb+slookupswitch+ *)
  | New of 'jclass
  | Newarray of 'jci jarray
      (* includes \verb+anewarray+ and \verb+newarray+ *)
  | Nop
  | Pop
  | Pop2
  | Putfield of absi * this * 'jf
      (* includes \verb+putfield_<t>+, \verb+putfield_<t>_this+ and \verb+putfield_<t>_w+ *)
  | Putstatic of absi * 'jf
  | Ret of local_index
  | Return of asi option
      (* includes \verb+areturn+, \verb+ireturn+, \verb+return+, \verb+sreturn+ *)
  | S2b
  | S2i
  | Sadd
  | Sand
  | Sdiv
  | Sinc of local_index * int
      (* includes \verb+sinc+ and \verb+sinc_w+ *)
  | Smul
  | Sneg
  | Sor
  | Spush of int
      (* includes \verb+bspush+, \verb+sspush+, \verb+sconst_<s>+ *)
  | Srem
  | Sshl
  | Sshr
  | Ssub
  | Store of asi * local_index
      (* includes \verb+astore+, \verb+istore+, \verb+sstore+, and their \verb+_<n>+ variants *)
  | Sushr
  | Swap_x of int * int
  | Sxor
  | Tableswitch of si * 'branch * int * int * 'branch array
      (* includes \verb+itableswitch+ and \verb+stableswitch+ *)

(* To allow easily switching representations for the instruction parameters, we define a [map] primitive over
   opcodes. *)

let map_jarray map_jci = function
  | ArrayPrim prim ->
      ArrayPrim prim
  | ArrayRef jci ->
      ArrayRef (map_jci jci)

let map_check map_jci = function
  | CheckArray jarray ->
      CheckArray (map_jarray map_jci jarray)
  | CheckClass jci ->
      CheckClass (map_jci jci)

let map_opcode (map_branch, map_jci, map_jclass, map_jintf, map_field, map_method) = function
  | Aconst_null ->
      Aconst_null
  | Arraylength ->
      Arraylength
  | Arrayload absi ->
      Arrayload absi
  | Arraystore absi ->
      Arraystore absi
  | Athrow ->
      Athrow
  | Checkcast check ->
      Checkcast (map_check map_jci check)
  | Dup ->
      Dup
  | Dup_x (m, n) ->
      Dup_x (m, n)
  | Dup2 ->
      Dup2
  | Getfield (absi, this, ifld) ->
      Getfield (absi, this, map_field ifld)
  | Getstatic (absi, sf) ->
      Getstatic (absi, map_field sf)
  | Goto branch ->
      Goto (map_branch branch)
  | I2b ->
      I2b
  | I2s ->
      I2s
  | Iadd ->
      Iadd
  | Iand ->
      Iand
  | Icmp ->
      Icmp
  | Idiv ->
      Idiv
  | If (condition, branch) ->
      If (condition, map_branch branch)
  | Iinc (local_index, k) ->
      Iinc (local_index, k)
  | Imul ->
      Imul
  | Ineg ->
      Ineg
  | Instanceof check ->
      Instanceof (map_check map_jci check)
  | Invokeinterface (nargs, jintf, method_token) ->
      Invokeinterface (nargs, map_jintf jintf, method_token)
  | Invokespecial ssm ->
      Invokespecial (map_method ssm)
  | Invokestatic sm ->
      Invokestatic (map_method sm)
  | Invokevirtual vm ->
      Invokevirtual (map_method vm)
  | Ior ->
      Ior
  | Ipush k ->
      Ipush k
  | Irem ->
      Irem
  | Ishl ->
      Ishl
  | Ishr ->
      Ishr
  | Isub ->
      Isub
  | Iushr ->
      Iushr
  | Ixor ->
      Ixor
  | Jsr branch ->
      Jsr (map_branch branch)
  | Load (asi, local_index) ->
      Load (asi, local_index)
  | Lookupswitch (si, default, cases) ->
      Lookupswitch (si, map_branch default,
		    List.map (fun (key, branch) -> (key, map_branch branch)) cases)
  | New jclass ->
      New (map_jclass jclass)
  | Newarray jarray ->
      Newarray (map_jarray map_jci jarray)
  | Nop ->
      Nop
  | Pop ->
      Pop
  | Pop2 ->
      Pop2
  | Putfield (absi, this, ifld) ->
      Putfield (absi, this, map_field ifld)
  | Putstatic (absi, sf) ->
      Putstatic (absi, map_field sf)
  | Ret local_index ->
      Ret local_index
  | Return asio ->
      Return asio
  | S2b ->
      S2b
  | S2i ->
      S2i
  | Sadd ->
      Sadd
  | Sand ->
      Sand
  | Sdiv ->
      Sdiv
  | Sinc (local_index, k) ->
      Sinc (local_index, k)
  | Smul ->
      Smul
  | Sneg ->
      Sneg
  | Sor ->
      Sor
  | Spush k ->
      Spush k
  | Srem ->
      Srem
  | Sshl ->
      Sshl
  | Sshr ->
      Sshr
  | Ssub ->
      Ssub
  | Store (asi, local_index) ->
      Store (asi, local_index)
  | Sushr ->
      Sushr
  | Swap_x (m, n) ->
      Swap_x (m, n)
  | Sxor ->
      Sxor
  | Tableswitch (si, default, lo, hi, table) ->
      Tableswitch (si, map_branch default, lo, hi, Array.map map_branch table)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Error handling} *)

let fail capname =
  Printf.eprintf "Inconsistent CAP file: %s\n" capname;
  flush stderr

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Component utilities} *)

(* This function extracts a named component out of a named JAR archive. It relies on an external `unzip' program,
   which must be in the user's \verb+$PATH+.

   If at least one erroneous exit code is expected, then we redirect the command's standard error channel to
   [/dev/null]. This prevents the end user from seeing error messages which correspond, in fact, to normal
   execution. *)

let extract expected file component =
  let redirect =
    if expected = [] then "" else " 2>/dev/null" in
  try
    Io_utils.execute expected (Printf.sprintf "unzip -C -p %s %s%s" file component redirect)
  with Failure msg ->
    Printf.eprintf "%s\n" msg;
    assert false

(* Given names for a JAR archive, a package and a component, plus the component's expected tag, this function extracts
   the component, applies the [action] function to a zone containing its [info] array, and makes sure the function
   reads all of it. A flag tells whether the component is [optional], i.e. whether its absence constitutes an error.
   If an optional component is missing, the function raises [Not_found]. *)

let unzip_no_match_code =
  11

let component capname package (optional, tag, component) action =
  let expected = if optional then [ unzip_no_match_code ] else [] in
  let buffer =
    try
      extract expected capname (Printf.sprintf "%s/javacard/%s.cap" package component)
    with Io_utils.Exited unzip_no_match_code ->
      raise Not_found in
  let zone = Zone.create buffer in
  try
    try

      (* Check the component's tag. *)

      assert (Zone.u1 zone = tag);

      (* Check the component's size. *)

      let size = Zone.u2 zone in
      let subzone = Zone.sub zone size in
      assert (Zone.head zone = String.length buffer);

      (* Return a zone containing the component's [info] array. *)

      let result = action subzone in

      (* Make sure we have read everything. *)

      assert (Zone.head zone = Zone.length zone);

      result

    with Zone.EOZ ->
      assert false
  with Assert_failure _ as exc ->
    Printf.eprintf "Unable to parse the %s component.\n" component;
    fail capname;
    raise exc

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Common sub-structures} *)

(* The [package_info] structure is found in Header and Import components. *)

module Package = struct

  type info = {
      minor: int;
      major: int;
      aid: string
    } 

  let info zone =

    (* Read in the package's version numbers. *)

    let minor = Zone.u1 zone in
    let major = Zone.u1 zone in

    (* Read in the package's AID, and return. *)

    {
      minor = minor;
      major = major;
      aid = Zone.pascal zone Zone.u1
    } 

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Header component} *)

module Header = struct

  type component = {
      flags: flag list;
      this_package: Package.info
    } 

  and flag =
    | ACC_INT
    | ACC_EXPORT
    | ACC_APPLET

  (* Given names for a JAR archive and a package, this function extracts and parses its header component. *)

  let read file package =
    component file package (false, 1, "Header") (fun zone ->

      (* Check the magic number. *)

      assert ((Zone.u2 zone = 0xDECA) & (Zone.u2 zone = 0xFFED));

      (* Read in the minor and major version numbers. Fail if they do not have the expected value, as required by
	 JavaCard's specification. *)

      assert ((Zone.u1 zone = 1) & (Zone.u1 zone = 2));

      (* Read in the flags. *)

      let flags = Standard.test (Zone.u1 zone) [
	0x01, ACC_INT;
	0x02, ACC_EXPORT;
	0x04, ACC_APPLET
      ] in

      (* Read in the [package_info], and return. *)

      {
	flags = flags;
	this_package = Package.info zone
      } 
    )

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Applet component} *)

module Applet = struct

  type component =
      applet list

  and applet = {
      aid: string;
      install_method_offset: method_component_info_offset
    } 

  (* Given names for a JAR archive and a package, this function extracts and parses its applet component.
     It raises [Not_found] if the package has no such component. *)

  let read file package =
    component file package (true, 3, "Applet") (fun zone ->

      (* Read in the applets. *)

      Zone.table zone Zone.u1 (fun zone ->

	let aid = Zone.pascal zone Zone.u1 in
	let offset = Zone.u2 zone in {
	  aid = aid;
	  install_method_offset = offset
	}
      )

    )

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Import component} *)

module Import = struct

  type component =
      Package.info array

  (* Given names for a JAR archive and a package, this function extracts and parses its import component. *)

  let read file package =
    component file package (false, 4, "Import") (fun zone ->
      Array.of_list (Zone.table zone Zone.u1 Package.info)
    )

  (* Given an import component and a package token, this function returns the AID of the corresponding package. *)

  let resolve component token =
    try
      component.(token).Package.aid
    with Invalid_argument _ ->
      assert false

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Constant pool component} *)

module Pool = struct

  type ('a, 'b) static_field_method_ref =
    | StaticRefInternal of 'a
    | StaticRefExternal of package_token * class_token * 'b

  type static_field_ref =
      (static_field_image_offset, field_token) static_field_method_ref

  type static_method_ref =
      (method_component_info_offset, method_token) static_field_method_ref

  type classref =
    | Internal of class_component_info_offset
    | External of package_token * class_token

  type 'a instance_field_method_ref =
      classref * 'a

  type instance_field_ref =
      field_token instance_field_method_ref

  type instance_method_ref =
      method_token instance_field_method_ref

  type info =
    | Constant_Classref of classref
    | Constant_InstanceFieldref of instance_field_ref
    | Constant_VirtualMethodref of instance_method_ref
    | Constant_SuperMethodref of instance_method_ref
    | Constant_StaticFieldref of static_field_ref
    | Constant_StaticMethodref of static_method_ref

  type component =
      info array

  (* Given names for a JAR archive and a package, this function extracts and parses its constant pool component. *)

  let rec read file package =
    component file package (false, 5, "ConstantPool") (fun zone ->
      Array.of_list (Zone.table zone Zone.u2 info)
    )

  and info zone =
    let position = Zone.head zone in

    let info = match Zone.u1 zone with
    | 1 ->
	Constant_Classref (classref zone)
    | 2 ->
	Constant_InstanceFieldref (instance_ref zone)
    | 3 ->
	Constant_VirtualMethodref (instance_ref zone)
    | 4 ->
	Constant_SuperMethodref (instance_ref zone)
    | 5 ->
	Constant_StaticFieldref (static_ref zone)
    | 6 ->
	Constant_StaticMethodref (static_ref zone)
    | _ ->
	assert false in

    (* Skip any padding. Every [cp_info] structure occupies 4 bytes. *)

    Zone.seek zone (position + 4);
    info

  and classref zone =

    (* A class reference consists of two bytes. *)

    let hi = Zone.u1 zone in
    let lo = Zone.u1 zone in

    (* If the high bit of the structure is zero, then this is in fact an internal class reference, i.e. a 16-bit
       offset into the [info] item of the class component. If it is one, then this is really an external reference,
       and the package's token is obtained by stripping the high bit. *)

    if hi land 0x80 = 0 then
      Internal (hi lsl 8 + lo)
    else
      External (hi land 0x7F, lo)

  and instance_ref zone =
  
    (* This consists of a [classref] structure, followed by a one-byte field or method token. *)

    let classref = classref zone in
    let token = Zone.u1 zone in
    classref, token

  and static_ref zone =
    
    (* Read the first byte, and determine whether its high bit is set. *)

    let first = Zone.u1 zone in
    if first land 0x80 = 0 then

      (* We have an internal reference. It is a 16-bit offset into the Static Field Image or into the
	 [info] item of the method component. *)

      StaticRefInternal (Zone.u2 zone)

    else begin

      (* We have an external reference. It begins with a 7-bit package token. *)

      let package_token = first land 0x7F in
      let class_token = Zone.u1 zone in
      let token = Zone.u1 zone in

      StaticRefExternal (package_token, class_token, token)
      
    end

  (* These functions look up a constant pool index. *)

  let lookup action component index =
    try
      action component.(index)
    with Invalid_argument _ ->
      assert false

  let lookup_instance_field_ref =
    lookup (function
      |	Constant_InstanceFieldref ref ->
	  ref
      |	_ ->
	  assert false
    )

  let lookup_static_field_ref =
    lookup (function
      |	Constant_StaticFieldref ref ->
	  ref
      |	_ ->
	  assert false
    )

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Class component} *)

module Class = struct

  type interface_info = {
      intf_shareable: bool;
      intf_superinterfaces: Pool.classref list
    } 

  type implemented_intf_info =
      Pool.classref * method_token array

  type class_info = {
      class_shareable: bool;
      class_superclass: Pool.classref option;
      class_declared_instance_size: int;
      class_first_reference_token: int;
      class_reference_count: int;
      class_public_method_table_base: method_token;
      class_package_method_table_base: method_token;
      class_public_virtual_method_table:
	(method_component_info_offset option) array;
      class_package_virtual_method_table:
	method_component_info_offset array;
      class_superinterfaces: implemented_intf_info list
    } 

  type info =
    | InfoInterface of interface_info
    | InfoClass of class_info

  type component =
      (class_component_info_offset, info) Hashtbl.t

  (* This auxiliary function checks whether the next 16 bits contain 0xFFFF. If so, it returns [None]. Otherwise, it
     invokes [element], which must read 16 bits as well, and prefixes its result with [Some]. *)

  let option16 zone element =
    if Zone.peek2 zone = 0xFFFF then
      let _ = Zone.u2 zone in
      None
    else
      Some (element zone)

  (* Given names for a JAR archive and a package, this function extracts and parses its class component. *)

  let rec read file package =
    component file package (false, 6, "Class") (fun zone ->
      Zone.hash_table zone info
    )
    
  and info zone =

    (* Read the first byte, and determine whether its high bit is set. *)

    let first = Zone.u1 zone in
    if first land 0x80 <> 0 then

      (* This is an interface. *)

      InfoInterface {
        intf_shareable = (first land 0x40 <> 0);
        intf_superinterfaces =
          Zone.table zone (fun _ -> first land 0x0F) Pool.classref
      }

    else begin

      (* This is a class. *)

      let superclass = option16 zone Pool.classref in
      let declared_instance_size = Zone.u1 zone in
      let first_reference_token = Zone.u1 zone in
      let reference_count = Zone.u1 zone in
      let public_method_table_base = Zone.u1 zone in
      let public_method_table_count = Zone.u1 zone in
      let package_method_table_base = Zone.u1 zone in
      let package_method_table_count = Zone.u1 zone in
      let public_virtual_method_table = Zone.table zone
	  (fun _ -> public_method_table_count)
	  (fun zone -> option16 zone Zone.u2) in
      let package_virtual_method_table = Zone.table zone
	  (fun _ -> package_method_table_count)
	  Zone.u2 in

      InfoClass {
        class_shareable = (first land 0x40 <> 0);
        class_superclass = superclass;
        class_declared_instance_size = declared_instance_size;
        class_first_reference_token = first_reference_token;
        class_reference_count = reference_count;
        class_public_method_table_base = public_method_table_base;
        class_package_method_table_base = package_method_table_base;
        class_public_virtual_method_table =
          Array.of_list public_virtual_method_table;
        class_package_virtual_method_table =
          Array.of_list package_virtual_method_table;
        class_superinterfaces =
          Zone.table zone (fun _ -> first land 0x0F) implemented_intf_info
      } 

    end

  and implemented_intf_info zone =
    let classref = Pool.classref zone in
    let indices = Zone.table zone Zone.u1 Zone.u1 in
    classref, Array.of_list indices

  (* This auxiliary function maps a [class_component_info_offset] to an [info] structure. *)

  let resolve_ccio component ccio =
    try
      Hashtbl.find component ccio
    with Not_found ->
      assert false

  (* This function maps a class, described by a [class_component_info_offset], to its superclass, described by
     a [Pool.classref option]. *)

  let superclass component ccio =
    match resolve_ccio component ccio with
    | InfoClass info ->
	info.class_superclass
    | InfoInterface _ ->
	assert false

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Descriptor component} *)

module Descriptor = struct

  type access_flags =
    | ACC_PUBLIC
    | ACC_PRIVATE
    | ACC_PROTECTED
    | ACC_STATIC
    | ACC_FINAL
    | ACC_INTERFACE
    | ACC_ABSTRACT
    | ACC_INIT

  type field_ref =
    | FieldStatic of Pool.static_field_ref
    | FieldInstance of Pool.instance_field_ref

  type type_descriptor_info_offset =
      int

  type field_type =
    | TypePrim of prim
    | TypeRef of type_descriptor_info_offset

  type field_descriptor_info = {
      field_owner: class_descriptor_info;
      field_token: field_token option;
      field_access_flags: access_flags list;
      field_ref: field_ref;
      field_type: field_type
    } 

  and method_descriptor_info = {
      method_owner: class_descriptor_info;
      method_token: method_token option;
      method_access_flags: access_flags list;
      method_offset: method_component_info_offset option;
      type_offset: type_descriptor_info_offset;
      bytecode_count: int;
      exception_handler_count: int;
      exception_handler_index: int
    } 

  and class_descriptor_info = {
      token: class_token option;
      access_flags: access_flags list;
      this_classref: Pool.classref;
      interfaces: Pool.classref list;
      mutable fields: field_descriptor_info list;
      mutable methods: method_descriptor_info list
    } 

  type type_descriptor_element =
    | TypeVoid
    | TypeBoolean
    | TypeByte
    | TypeShort
    | TypeInt
    | TypeReference of Pool.classref
    | TypeArrayBoolean
    | TypeArrayByte
    | TypeArrayShort
    | TypeArrayInt
    | TypeArrayReference of Pool.classref

  type type_descriptor =
      type_descriptor_element list

  type type_descriptor_info = {
      cp_types: (type_descriptor_info_offset option) array;
      type_desc: (type_descriptor_info_offset, type_descriptor) Hashtbl.t
    }

  type component = {
      classes: (class_component_info_offset, class_descriptor_info) Hashtbl.t;
      types: type_descriptor_info;

      (* While parsing this component, we build a table which maps [method_component_info_offset]s into
	 [method_descriptor_info] structures. This will help parse the method component. *)

      mcio_table: (method_component_info_offset, method_descriptor_info) Hashtbl.t;

      (* Another table maps [static_field_image_offset]s into [field_descriptor_info] structures. *)

      sfio_table: (static_field_image_offset, field_descriptor_info) Hashtbl.t

    }

  (* These auxiliary functions map a [class_descriptor_info] structure to a [class_component_info_offset], and
     back. *)

  let get_ccio info =
    match info.this_classref with
    | Pool.Internal offset ->
	offset
    | Pool.External _ ->
	assert false

  let resolve_ccio component ccio =
    try
      Hashtbl.find component.classes ccio
    with Not_found ->
      assert false

  (* This auxiliary function maps a [method_component_info_offset] to a [method_descriptor_info]. *)

  let resolve_mcio component mcio =
    try
      Hashtbl.find component.mcio_table mcio
    with Not_found ->
      assert false

  (* This auxiliary function maps a [static_field_image_offset] to a [field_descriptor_info]. *)

  let resolve_sfio component sfio =
    try
      Hashtbl.find component.sfio_table sfio
    with Not_found ->
      assert false

  (* Given names for a JAR archive and a package, this function extracts and parses its descriptor component. *)

  let rec read file package =
    component file package (false, 11, "Descriptor") (fun zone ->
      let mcio_table = Hashtbl.create 1023
      and classes = Hashtbl.create 1023
      and sfio_table = Hashtbl.create 1023 in

      let _ = Zone.table zone Zone.u1
	  (class_descriptor_info mcio_table sfio_table classes) in
      {
        classes = classes;
        types = type_descriptor_info (Zone.sub zone (Zone.length zone - Zone.head zone));
        mcio_table = mcio_table;
        sfio_table = sfio_table
      } 

    )

  and token_option zone =
    match Zone.u1 zone with
    | 0xFF ->
	None
    | token ->
	Some token

  and class_descriptor_info mcio_table sfio_table classes zone =

    let token = token_option zone in

    let access_flags =
      Standard.test (Zone.u1 zone) [
        0x01, ACC_PUBLIC;
        0x10, ACC_FINAL;
        0x40, ACC_INTERFACE;
        0x80, ACC_ABSTRACT
      ] in
    let is_interface =
      List.mem ACC_INTERFACE access_flags in

    let this_classref = Pool.classref zone in

    let intf_count = Zone.u1 zone in
    let field_count = Zone.u2 zone in
    let meth_count = Zone.u2 zone in

    let interfaces = Zone.table zone (fun _ -> intf_count) Pool.classref in

    let class_info = {
      token = token;
      access_flags = access_flags;
      this_classref = this_classref;
      interfaces = interfaces;
      fields = [];
      methods = []
    } in

    let fields = Zone.table zone (fun _ -> field_count) (field_descriptor_info class_info sfio_table) in
    let methods = Zone.table zone
	(fun _ -> meth_count)
	(method_descriptor_info class_info mcio_table is_interface) in

    (* Update the mapping from [class_component_info_offset]s to [class_descriptor_info]s. *)

    Hashtbl.add classes (get_ccio class_info) class_info;
    class_info.fields <- fields;
    class_info.methods <- methods;
    class_info

  and field_descriptor_info class_info sfio_table zone =
    let token = token_option zone in
    let access_flags =
      Standard.test (Zone.u1 zone) [
        0x01, ACC_PUBLIC;
        0x02, ACC_PRIVATE;
        0x04, ACC_PROTECTED;
        0x08, ACC_STATIC;
        0x10, ACC_FINAL
      ] in
    let field_ref =
      if List.mem ACC_STATIC access_flags then
	FieldStatic (Pool.static_ref zone)
      else
	FieldInstance (Pool.instance_ref zone) in
    let typ = Zone.u2 zone in
    let typ =
      if typ land 0x8000 <> 0 then TypePrim (
	match typ with
	| 0x8002 ->
	    PrimBoolean
	| 0x8003 ->
	    PrimByte
	| 0x8004 ->
	    PrimShort
	| 0x8005 ->
	    PrimInt
	| _ ->
	    assert false
      )
      else TypeRef typ in
    let field_info = {
      field_owner = class_info;
      field_token = token;
      field_access_flags = access_flags;
      field_ref = field_ref;
      field_type = typ
    } in
    begin
      match field_ref with
      |	FieldStatic (Pool.StaticRefInternal sfio) ->
	  Hashtbl.add sfio_table sfio field_info
      |	FieldStatic (Pool.StaticRefExternal _) ->
	  assert false
      |	FieldInstance _ ->
	  ()
    end;
    field_info
	
  and method_descriptor_info class_info mcio_table is_interface zone =
    let token = token_option zone in
    let access_flags =
      Standard.test (Zone.u1 zone) [
        0x01, ACC_PUBLIC;
        0x02, ACC_PRIVATE;
        0x04, ACC_PROTECTED;
        0x08, ACC_STATIC;
        0x10, ACC_FINAL;
        0x40, ACC_ABSTRACT;
        0x80, ACC_INIT
      ] in
    let method_offset =
      match is_interface, Zone.u2 zone with
      |	true, 0 ->
	  None
      |	false, method_offset ->
	  Some method_offset
      |	_ ->
	  assert false in
    let type_offset = Zone.u2 zone in
    let bytecode_count = Zone.u2 zone in
    let exception_handler_count = Zone.u2 zone in
    let exception_handler_index = Zone.u2 zone in
    let result = {
      method_owner = class_info;
      method_token = token;
      method_access_flags = access_flags;
      method_offset = method_offset;
      type_offset = type_offset;
      bytecode_count = bytecode_count;
      exception_handler_count = exception_handler_count;
      exception_handler_index = exception_handler_index
    } in
    Standard.do_option method_offset (fun offset ->
      Hashtbl.add mcio_table offset result
    );
    result

  and type_descriptor_info zone =
    let cp_types = Zone.table zone Zone.u2 (fun zone ->
      match Zone.u2 zone with
      |	0xFFFF ->
	  None
      |	offset ->
	  Some offset
    ) in
    let type_desc = Zone.hash_table zone (fun zone ->
      let nibble_count = Zone.u1 zone in
      parse (nibble_list_of_zone nibble_count zone)
    ) in
    {
      cp_types = Array.of_list cp_types;
      type_desc = type_desc
    }

  and nibble_list_of_zone count zone =
    match count with
    | 0 ->
	[]
    | 1 ->
	[ (Zone.u1 zone) lsr 4 ]
    | _ ->
	let first = Zone.u1 zone in
	(first lsr 4) :: (first land 0x0F) :: (nibble_list_of_zone (count - 2) zone)

  and parse = function
    | [] ->
	[]
    | 0x1 :: rest ->
	TypeVoid :: parse rest
    | 0x2 :: rest ->
	TypeBoolean :: parse rest
    | 0x3 :: rest ->
	TypeByte :: parse rest
    | 0x4 :: rest ->
	TypeShort :: parse rest
    | 0x5 :: rest ->
	TypeInt :: parse rest
    | 0x6 :: nibble1 :: nibble2 :: nibble3 :: nibble4 :: rest ->
	let buffer = String.create 2 in
	buffer.[0] <- Char.chr ((nibble1 lsl 4) + nibble2);
	buffer.[1] <- Char.chr ((nibble3 lsl 4) + nibble4);
	(TypeReference (Pool.classref (Zone.create buffer))) :: parse rest
    | 0xA :: rest ->
	TypeArrayBoolean :: parse rest
    | 0xB :: rest ->
	TypeArrayByte :: parse rest
    | 0xC :: rest ->
	TypeArrayShort :: parse rest
    | 0xD :: rest ->
	TypeArrayInt :: parse rest
    | 0xE :: nibble1 :: nibble2 :: nibble3 :: nibble4 :: rest ->
	let buffer = String.create 2 in
	buffer.[0] <- Char.chr ((nibble1 lsl 4) + nibble2);
	buffer.[1] <- Char.chr ((nibble3 lsl 4) + nibble4);
	(TypeArrayReference (Pool.classref (Zone.create buffer))) :: parse rest
    | _ ->
	assert false
    
end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Method component} *)

module Method = struct

  (* Exception handler information. *)

  type handler_info = {
      nested: bool;
      start_offset: method_component_info_offset;
      end_offset: method_component_info_offset;
      handler_offset: method_component_info_offset;
      catch_type_index: cpool_index option
    } 

  type node = {
      node_mcio: method_component_info_offset;
      node_sequential_index: int;
      node_instruction: (int, cpool_index, cpool_index, cpool_index, cpool_index, cpool_index) opcode;
      mutable node_next: node option
    } 

  type bytecode =
      (method_component_info_offset, node) Hashtbl.t * int (* instruction count *) * node (* start instruction *)

  type concrete_method_info = {
      max_stack: int;
      max_locals: int;
      bytecode: bytecode
    } 

  type method_info = {
      nargs: int;
      code: concrete_method_info option
    } 

  type component = {
      handlers: handler_info array;
      methods: (method_component_info_offset, method_info) Hashtbl.t
    }

  (* This function turns a [method_component_info_offset] into a [method_info] structure. *)

  let resolve component mcio =
    try
      Hashtbl.find component.methods mcio
    with Not_found ->
      assert false

  (* Given names for a JAR archive and a package, plus a representation of the package's descriptor component, this
     function extracts and parses its method component. *)

  let rec read file package desc =
    component file package (false, 7, "Method") (fun zone ->

      let handlers = Zone.table zone Zone.u1 handler_info in
      let methods = Zone.hash_table zone (method_info desc) in
      {
        handlers = Array.of_list handlers;
        methods = methods
      }

    )

  and handler_info zone =
    let start_offset = Zone.u2 zone in
    let active_length = Zone.u2 zone in
    let nested, end_offset =
      if active_length land 0x8000 = 0 then
	true, start_offset + active_length
      else
	false, start_offset + (active_length land 0x7FFF) in
    let handler_offset = Zone.u2 zone in
    let catch_type_index =
      match Zone.u2 zone with
      |	0 ->
	  None
      |	index ->
	  Some index in
    {
      nested = nested;
      start_offset = start_offset;
      end_offset = end_offset;
      handler_offset = handler_offset;
      catch_type_index = catch_type_index
    } 

  and method_info desc zone =

    (* Retrieve information about this method from the Descriptor Component. This allows us to determine the length
       of the method's bytecode array, which is not stored inside the Method Component. *)

    let count =
      (Descriptor.resolve_mcio desc (Zone.head zone)).Descriptor.bytecode_count in

    let first = Zone.u1 zone in
    let max_stack, nargs, max_locals =
      if first land 0x80 = 0 then
	let second = Zone.u1 zone in
	first land 0x0F, second lsr 4, second land 0x0F
      else
	let max_stack = Zone.u1 zone in
	let nargs = Zone.u1 zone in
	max_stack, nargs, Zone.u1 zone in
    {
      nargs = nargs;
      code =
        if first land 0x40 <> 0 then
	  None
	else
	  Some {
	    max_stack = max_stack;
	    max_locals = max_locals;
	    bytecode = bytecode zone (Zone.head zone + count)
	  }
    }

  and bytecode zone limit =
    let table = Hashtbl.create 1023
    and count = ref 0
    and previous = ref None
    and start = ref None in
    let rec loop () =
      let offset = Zone.head zone in
      assert (offset <= limit);
      if offset < limit then begin
	let node = {
	  node_mcio = offset;
	  node_sequential_index = !count;
	  node_instruction = instruction zone;
	  node_next = None
        } in
	Standard.do_option !previous (fun previous ->
	  previous.node_next <- Some node
        );
	if !start = None then
	  start := Some node;
	Hashtbl.add table offset node;
	incr count;
	previous := Some node;
	loop()
      end in
    loop();
    match !start with
    | Some start ->
	table, !count, start
    | None ->
	assert false

  and instruction zone =
    match Zone.u1 zone with
    | 0x24 ->
	Arrayload AbsiA
    | 0x37 ->
	Arraystore AbsiA
    | 0x01 ->
	Aconst_null
    | 0x15 ->
	Load (AsiA, Zone.u1 zone)
    | 0x18 ->
	Load (AsiA, 0)
    | 0x19 ->
	Load (AsiA, 1)
    | 0x1a ->
	Load (AsiA, 2)
    | 0x1b ->
	Load (AsiA, 3)
    | 0x91 ->
	Newarray (ArrayRef (Zone.u2 zone))
    | 0x77 ->
	Return (Some AsiA)
    | 0x92 ->
	Arraylength
    | 0x28 ->
	Store (AsiA, Zone.u1 zone)
    | 0x2b ->
	Store (AsiA, 0)
    | 0x2c ->
	Store (AsiA, 1)
    | 0x2d ->
	Store (AsiA, 2)
    | 0x2e ->
	Store (AsiA, 3)
    | 0x93 ->
	Athrow
    | 0x25 ->
	Arrayload AbsiB
    | 0x38 ->
	Arraystore AbsiB
    | 0x12 ->
	Ipush (Zone.s1 zone)
    | 0x10 ->
	Spush (Zone.s1 zone)
    | 0x94 ->
	Checkcast (check zone)
    | 0x3d ->
	Dup
    | 0x3f ->
	let mn = Zone.u1 zone in
	Dup_x (mn lsr 4, mn land 0x0F)
    | 0x3e ->
	Dup2
    | 0x83 ->
	Getfield (AbsiA, UseStack, Zone.u1 zone)
    | 0x84 ->
	Getfield (AbsiB, UseStack, Zone.u1 zone)
    | 0x85 ->
	Getfield (AbsiS, UseStack, Zone.u1 zone)
    | 0x86 ->
	Getfield (AbsiI, UseStack, Zone.u1 zone)
    | 0xad ->
	Getfield (AbsiA, UseThis, Zone.u1 zone)
    | 0xae ->
	Getfield (AbsiB, UseThis, Zone.u1 zone)
    | 0xaf ->
	Getfield (AbsiS, UseThis, Zone.u1 zone)
    | 0xb0 ->
	Getfield (AbsiI, UseThis, Zone.u1 zone)
    | 0xa9 ->
	Getfield (AbsiA, UseStack, Zone.u2 zone)
    | 0xaa ->
	Getfield (AbsiB, UseStack, Zone.u2 zone)
    | 0xab ->
	Getfield (AbsiS, UseStack, Zone.u2 zone)
    | 0xac ->
	Getfield (AbsiI, UseStack, Zone.u2 zone)
    | 0x7b ->
	Getstatic (AbsiA, Zone.u2 zone)
    | 0x7c ->
	Getstatic (AbsiB, Zone.u2 zone)
    | 0x7d ->
	Getstatic (AbsiS, Zone.u2 zone)
    | 0x7e ->
	Getstatic (AbsiI, Zone.u2 zone)
    | 0x70 ->
	Goto (Zone.s1 zone)
    | 0xa8 ->
	Goto (Zone.s2 zone)
    | 0x5d ->
	I2b
    | 0x5e ->
	I2s
    | 0x42 ->
	Iadd
    | 0x27 ->
	Arrayload AbsiI
    | 0x54 ->
	Iand
    | 0x3a ->
	Arraystore AbsiI
    | 0x5f ->
	Icmp
    | 0x09 ->
	Ipush (-1)
    | 0xa ->
	Ipush 0
    | 0xb ->
	Ipush 1
    | 0xc ->
	Ipush 2
    | 0xd ->
	Ipush 3
    | 0xe ->
	Ipush 4
    | 0xf ->
	Ipush 5
    | 0x48 ->
	Idiv
    | 0x68 ->
	If (Acmpeq, Zone.s1 zone)
    | 0x69 ->
	If (Acmpne, Zone.s1 zone)
    | 0xa0 ->
	If (Acmpeq, Zone.s2 zone)
    | 0xa1 ->
	If (Acmpne, Zone.s2 zone)
    | 0x6a ->
	If (Scmpeq, Zone.s1 zone)
    | 0x6b ->
	If (Scmpne, Zone.s1 zone)
    | 0x6c ->
	If (Scmplt, Zone.s1 zone)
    | 0x6d ->
	If (Scmpge, Zone.s1 zone)
    | 0x6e ->
	If (Scmpgt, Zone.s1 zone)
    | 0x6f ->
	If (Scmple, Zone.s1 zone)
    | 0xa2 ->
	If (Scmpeq, Zone.s2 zone)
    | 0xa3 ->
	If (Scmpne, Zone.s2 zone)
    | 0xa4 ->
	If (Scmplt, Zone.s2 zone)
    | 0xa5 ->
	If (Scmpge, Zone.s2 zone)
    | 0xa6 ->
	If (Scmpgt, Zone.s2 zone)
    | 0xa7 ->
	If (Scmple, Zone.s2 zone)
    | 0x60 ->
	If (Eq, Zone.s1 zone)
    | 0x61 ->
	If (Ne, Zone.s1 zone)
    | 0x62 ->
	If (Lt, Zone.s1 zone)
    | 0x63 ->
	If (Ge, Zone.s1 zone)
    | 0x64 ->
	If (Gt, Zone.s1 zone)
    | 0x65 ->
	If (Le, Zone.s1 zone)
    | 0x98 ->
	If (Eq, Zone.s2 zone)
    | 0x99 ->
	If (Ne, Zone.s2 zone)
    | 0x9a ->
	If (Lt, Zone.s2 zone)
    | 0x9b ->
	If (Ge, Zone.s2 zone)
    | 0x9c ->
	If (Gt, Zone.s2 zone)
    | 0x9d ->
	If (Le, Zone.s2 zone)
    | 0x67 ->
	If (Nonnull, Zone.s1 zone)
    | 0x9f ->
	If (Nonnull, Zone.s2 zone)
    | 0x66 ->
	If (Null, Zone.s1 zone)
    | 0x9e ->
	If (Null, Zone.s2 zone)
    | 0x5a ->
	let index = Zone.u1 zone in
	Iinc (index, Zone.s1 zone)
    | 0x97 ->
	let index = Zone.u1 zone in
	Iinc (index, Zone.s2 zone)
    | 0x14 ->
	Ipush (Zone.s4 zone)
    | 0x17 ->
	Load (AsiI, Zone.u1 zone)
    | 0x20 ->
	Load (AsiI, 0)
    | 0x21 ->
	Load (AsiI, 1)
    | 0x22 ->
	Load (AsiI, 2)
    | 0x23 ->
	Load (AsiI, 3)
    | 0x76 ->
	let default = Zone.s2 zone in
	let pairs = Zone.table zone Zone.u2 (fun zone ->
	  let key = Zone.s4 zone in
	  let offset = Zone.s2 zone in
	  key, offset
        ) in
	Lookupswitch (SiI, default, pairs)
    | 0x46 ->
	Imul
    | 0x4c ->
	Ineg
    | 0x95 ->
	Instanceof (check zone)
    | 0x8e ->
	let nargs = Zone.u1 zone in
	let index = Zone.u2 zone in
	let token = Zone.u1 zone in
	Invokeinterface (nargs, index, token)
    | 0x8c ->
	Invokespecial (Zone.u2 zone)
    | 0x8d ->
	Invokestatic (Zone.u2 zone)
    | 0x8b ->
	Invokevirtual (Zone.u2 zone)
    | 0x56 ->
	Ior
    | 0x4a ->
	Irem
    | 0x79 ->
	Return (Some AsiI)
    | 0x4e ->
	Ishl
    | 0x50 ->
	Ishr
    | 0x2a ->
	Store (AsiI, Zone.u1 zone)
    | 0x33 ->
	Store (AsiI, 0)
    | 0x34 ->
	Store (AsiI, 1)
    | 0x35 ->
	Store (AsiI, 2)
    | 0x36 ->
	Store (AsiI, 3)
    | 0x44 ->
	Isub
    | 0x74 ->
	let default = Zone.s2 zone in
	let lo = Zone.s4 zone in
	let hi = Zone.s4 zone in
	let table = Zone.table zone (fun _ -> hi - lo + 1) Zone.s2 in
	Tableswitch (SiI, default, lo, hi, Array.of_list table)
    | 0x52 ->
	Iushr
    | 0x58 ->
	Ixor
    | 0x71 ->
	Jsr (Zone.s2 zone)
    | 0x8f ->
	New (Zone.u2 zone)
    | 0x90 ->
	Newarray (ArrayPrim (prim_code (Zone.u1 zone)))
    | 0x0 ->
	Nop
    | 0x3b ->
	Pop
    | 0x3c ->
	Pop2
    | 0x87 ->
	Putfield (AbsiA, UseStack, Zone.u1 zone)
    | 0x88 ->
	Putfield (AbsiB, UseStack, Zone.u1 zone)
    | 0x89 ->
	Putfield (AbsiS, UseStack, Zone.u1 zone)
    | 0x8a ->
	Putfield (AbsiI, UseStack, Zone.u1 zone)
    | 0xb5 ->
	Putfield (AbsiA, UseThis, Zone.u1 zone)
    | 0xb6 ->
	Putfield (AbsiB, UseThis, Zone.u1 zone)
    | 0xb7 ->
	Putfield (AbsiS, UseThis, Zone.u1 zone)
    | 0xb8 ->
	Putfield (AbsiI, UseThis, Zone.u1 zone)
    | 0xb1 ->
	Putfield (AbsiA, UseStack, Zone.u2 zone)
    | 0xb2 ->
	Putfield (AbsiB, UseStack, Zone.u2 zone)
    | 0xb3 ->
	Putfield (AbsiS, UseStack, Zone.u2 zone)
    | 0xb4 ->
	Putfield (AbsiI, UseStack, Zone.u2 zone)
    | 0x7f ->
	Putstatic (AbsiA, Zone.u2 zone)
    | 0x80 ->
	Putstatic (AbsiB, Zone.u2 zone)
    | 0x81 ->
	Putstatic (AbsiS, Zone.u2 zone)
    | 0x82 ->
	Putstatic (AbsiI, Zone.u2 zone)
    | 0x72 ->
	Ret (Zone.u1 zone)
    | 0x7a ->
	Return None
    | 0x5b ->
	S2b
    | 0x5c ->
	S2i
    | 0x41 ->
	Sadd
    | 0x26 ->
	Arrayload AbsiS
    | 0x53 ->
	Sand
    | 0x39 ->
	Arraystore AbsiS
    | 0x2 ->
	Spush (-1)
    | 0x3 ->
	Spush 0
    | 0x4 ->
	Spush 1
    | 0x5 ->
	Spush 2
    | 0x6 ->
	Spush 3
    | 0x7 ->
	Spush 4
    | 0x8 ->
	Spush 5
    | 0x47 ->
	Sdiv
    | 0x59 ->
	let index = Zone.u1 zone in
	Sinc (index, Zone.s1 zone)
    | 0x96 ->
	let index = Zone.u1 zone in
	Sinc (index, Zone.s2 zone)
    | 0x13 ->
	Ipush (Zone.s2 zone)
    | 0x16 ->
	Load (AsiS, Zone.u1 zone)
    | 0x1c ->
	Load (AsiS, 0)
    | 0x1d ->
	Load (AsiS, 1)
    | 0x1e ->
	Load (AsiS, 2)
    | 0x1f ->
	Load (AsiS, 3)
    | 0x75 ->
	let default = Zone.s2 zone in
	let pairs = Zone.table zone Zone.u2 (fun zone ->
	  let key = Zone.s2 zone in
	  let offset = Zone.s2 zone in
	  key, offset
        ) in
	Lookupswitch (SiS, default, pairs)
    | 0x45 ->
	Smul
    | 0x4b ->
	Sneg
    | 0x55 ->
	Sor
    | 0x49 ->
	Srem
    | 0x78 ->
	Return (Some AsiS)
    | 0x4d ->
	Sshl
    | 0x4f ->
	Sshr
    | 0x11 ->
	Spush (Zone.s2 zone)
    | 0x29 ->
	Store (AsiS, Zone.u1 zone)
    | 0x2f ->
	Store (AsiS, 0)
    | 0x30 ->
	Store (AsiS, 1)
    | 0x31 ->
	Store (AsiS, 2)
    | 0x32 ->
	Store (AsiS, 3)
    | 0x43 ->
	Ssub
    | 0x73 ->
	let default = Zone.s2 zone in
	let lo = Zone.s2 zone in
	let hi = Zone.s2 zone in
	let table = Zone.table zone (fun _ -> hi - lo + 1) Zone.s2 in
	Tableswitch (SiS, default, lo, hi, Array.of_list table)
    | 0x51 ->
	Sushr
    | 0x40 ->
	let mn = Zone.u1 zone in
	Swap_x (mn lsr 4, mn land 0x0F)
    | 0x57 ->
	Sxor
           | 0xb9 | 0xba | 0xbb | 0xbc | 0xbd | 0xbe | 0xbf
    | 0xc0 | 0xc1 | 0xc2 | 0xc3 | 0xc4 | 0xc5 | 0xc6 | 0xc7
    | 0xc8 | 0xc9 | 0xca | 0xcb | 0xcc | 0xcd | 0xce | 0xcf
    | 0xc0 | 0xc1 | 0xc2 | 0xc3 | 0xc4 | 0xc5 | 0xc6 | 0xc7
    | 0xc8 | 0xc9 | 0xca | 0xcb | 0xcc | 0xcd | 0xce | 0xcf
    | 0xd0 | 0xd1 | 0xd2 | 0xd3 | 0xd4 | 0xd5 | 0xd6 | 0xd7
    | 0xd8 | 0xd9 | 0xda | 0xdb | 0xdc | 0xdd | 0xde | 0xdf
    | 0xe0 | 0xe1 | 0xe2 | 0xe3 | 0xe4 | 0xe5 | 0xe6 | 0xe7
    | 0xe8 | 0xe9 | 0xea | 0xeb | 0xec | 0xed | 0xee | 0xef
    | 0xf0 | 0xf1 | 0xf2 | 0xf3 | 0xf4 | 0xf5 | 0xf6 | 0xf7
    | 0xf8 | 0xf9 | 0xfa | 0xfb | 0xfc | 0xfd | 0xfe | 0xff ->
	assert false
    | _ ->
	assert false

  and check zone =
    let atype = Zone.u1 zone in
    let index = Zone.u2 zone in
    match atype, index with
    | (10 | 11 | 12 | 13), 0 ->
	CheckArray (ArrayPrim (prim_code atype))
    | 14, _ ->
	CheckArray (ArrayRef index)
    | 0, _ ->
	CheckClass index
    | _, _ ->
	assert false

  and prim_code = function
    | 10 ->
	PrimBoolean
    | 11 ->
	PrimByte
    | 12 ->
	PrimShort
    | 13 ->
	PrimInt
    | _ ->
	assert false

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Static field component} *)

module StaticField = struct

  type array_init_info = {
      array_type: prim;
      array_data: string
    } 

  type component = {
      image_size: int;
      reference_count: int;
      array_init: array_init_info list;
      default_value_count: int;
      non_default_values: string
    } 

  (* Given names for a JAR archive and a package, this function extracts and parses its static field component. *)

  let rec read file package =
    component file package (false, 8, "StaticField") (fun zone ->
      
      let image_size = Zone.u2 zone in
      let reference_count = Zone.u2 zone in
      let array_init = Zone.table zone Zone.u2 array_init_info in
      let default_value_count = Zone.u2 zone in
      let non_default_value_count = Zone.u2 zone in
      let non_default_values = Zone.string (Zone.sub zone non_default_value_count) in
      {
        image_size = image_size;
        reference_count = reference_count;
        array_init = array_init;
        default_value_count = default_value_count;
        non_default_values = non_default_values
      } 

    )

  and array_init_info zone =
    let typ =
      match Zone.u1 zone with
      |	2 ->
	  PrimBoolean
      |	3 ->
	  PrimByte
      |	4 ->
	  PrimShort
      |	5 ->
	  PrimInt
      |	_ ->
	  assert false in
    let count = Zone.u2 zone in
    let values = Zone.string (Zone.sub zone count) in
    {
      array_type = typ;
      array_data = values
    } 
    
end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Reference Location component} *)

module ReferenceLocation = struct

  type component = {
      offsets_to_byte_indices: method_component_info_offset list;
      offsets_to_byte2_indices: method_component_info_offset list
    } 

  (* This auxiliary function turns parses the rather peculiar format used to represent lists of offsets in the
     Reference Location component. *)

  let rec convert base = function
    | [] ->
	[]
    | 255 :: rest ->
	convert (base + 255) rest
    | entry :: rest ->
	let offset = base + entry in
	offset :: convert offset rest

  (* Given names for a JAR archive and a package, this function extracts and parses its reference location
     component. *)

  let rec read file package =
    component file package (false, 9, "RefLocation") (fun zone ->
      
      let offsets_to_byte_indices  = Zone.table zone Zone.u2 Zone.u1 in
      let offsets_to_byte2_indices = Zone.table zone Zone.u2 Zone.u1 in
      {
        offsets_to_byte_indices = convert 0 offsets_to_byte_indices;
        offsets_to_byte2_indices = convert 0 offsets_to_byte2_indices
      } 

    )
    
end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Export component} *)

module Export = struct

  type class_export_info = {
      class_offset: class_component_info_offset;
      static_field_offsets: static_field_image_offset list;
      static_method_offsets: method_component_info_offset list
    } 

  type component =
      class_export_info array

  (* Given names for a JAR archive and a package, this function extracts and parses its export component.
     It raises [Not_found] if the package has no such component. *)

  let rec read file package =
    component file package (true, 10, "Export") (fun zone ->
      Array.of_list (Zone.table zone Zone.u1 class_export_info)
    )

  and class_export_info zone =
    let class_offset = Zone.u2 zone in
    let static_field_count = Zone.u1 zone in
    let static_method_count = Zone.u1 zone in
    let static_field_offsets =
      Zone.table zone (fun _ -> static_field_count) Zone.u2 in
    let static_method_offsets =
      Zone.table zone (fun _ -> static_method_count) Zone.u2 in
    {
      class_offset = class_offset;
      static_field_offsets = static_field_offsets;
      static_method_offsets = static_method_offsets
    } 
    
end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Putting it (almost) all together} *)

type file = {
    capname: string;
    header: Header.component;
    import: Import.component;
    pool: Pool.component;
    cc: Class.component;
    mc: Method.component;
    descriptor: Descriptor.component
  } 

(* [load capname packname] loads the package named [packname] from the CAP file named [capname]. *)

let load capname packname =
  Verbose.say (Printf.sprintf "Loading CAP file %s...\n" capname);
  let desc = Descriptor.read capname packname in
  {
    capname = capname;
    header = Header.read capname packname;
    import = Import.read capname packname;
    pool = Pool.read capname packname;
    cc = Class.read capname packname;
    mc = Method.read capname packname desc;
    descriptor = desc
  } 

(* This auxiliary function helps handle errors. *)

let protect action file =
  try
    action file
  with Assert_failure _ as exc ->
    fail file.capname;
    raise exc

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Accessing information} *)

(* This function returns the name of the file where the data was read. *)

let filename file =
  file.capname

(* This function returns the package's AID. *)

let package_aid file =
  file.header.Header.this_package.Package.aid

(* This function allows looping over all packages imported by this package. The [action] function is successively
   given the AID of every imported package. *)

let imports action file =
  Array.iter (fun info ->
    action info.Package.aid
  ) file.import

(* Types for internal/external references. References are either internal or external. Internal references are
   represented by an abstract type. External references consist of a package AID, a class/interface token, and
   additional information, such as a field or method token. *)

type internal_class_reference =
    Descriptor.class_descriptor_info

type internal_method_reference =
    Descriptor.method_descriptor_info

type internal_field_reference =
    Descriptor.field_descriptor_info

type ('a, 'b) reference =
  | Internal of 'a
  | External of string * class_token * 'b

type class_reference =
    (internal_class_reference, unit) reference

type method_reference =
    (internal_method_reference, method_token) reference

type field_reference =
    (internal_field_reference, field_token) reference

(* These functions map a field/method to its owning class. *)

let method_owner method_info =
  method_info.Descriptor.method_owner

let field_owner field_info =
  field_info.Descriptor.field_owner

(* This internal module allows conversion from CAP Constant Pool format to external reference format. *)

module Extern = struct

  let classref file = function
    | Pool.Internal ccio ->
	Internal (Descriptor.resolve_ccio file.descriptor ccio)
    | Pool.External (package_token, class_token) ->
	External (Import.resolve file.import package_token, class_token, ())

  let static_method_ref file = function
    | Pool.StaticRefInternal mcio ->
	Internal (Descriptor.resolve_mcio file.descriptor mcio)
    | Pool.StaticRefExternal (package_token, class_token, method_token) ->
	External (Import.resolve file.import package_token, class_token, method_token)

  let static_field_ref file = function
    | Pool.StaticRefInternal sfio ->
	Internal (Descriptor.resolve_sfio file.descriptor sfio)
    | Pool.StaticRefExternal (package_token, class_token, field_token) ->
	External (Import.resolve file.import package_token, class_token, field_token)

  let instance_method_ref file (cref, method_token) =
    match classref file cref with
    | Internal class_info -> (
	try
	  let method_info = List.find (fun method_info ->
	    method_info.Descriptor.method_token = Some method_token
	  ) class_info.Descriptor.methods in
	  Internal method_info
	with Not_found ->
	  assert false
      )
    | External (aid, class_token, ()) ->
	External (aid, class_token, method_token)

  let instance_field_ref file (cref, field_token) =
    match classref file cref with
    | Internal class_info -> (
	try
	  let field_info = List.find (fun field_info ->
	    field_info.Descriptor.field_token = Some field_token
	  ) class_info.Descriptor.fields in
	  Internal field_info
	with Not_found ->
	  assert false
      )
    | External (aid, class_token, ()) ->
	External (aid, class_token, field_token)

end

(* This function lists the classes/interfaces defined within a package. *)

let classes action file =
  Hashtbl.iter (fun _ info -> action info) file.descriptor.Descriptor.classes

(* This function lists the methods defined by a given class/interface. *)

let methods action class_info =
  List.iter action class_info.Descriptor.methods

(* This function maps a class to its superclass, if any. *)

let superclass file info =
  Standard.map_option (Extern.classref file) (Class.superclass file.cc (Descriptor.get_ccio info))

let superclass =
  protect superclass

(* These functions return human-readable names for classes/interfaces and methods. They are used for debugging. *)

module Human = struct

  let icref info =
    Printf.sprintf "<class #%d>" (Descriptor.get_ccio info)

  let simple_imref info =
    let basic = match info.Descriptor.method_offset with
    | None ->
	Printf.sprintf "token %d" (Standard.option info.Descriptor.method_token)
    | Some mcio ->
	match info.Descriptor.method_token with
	| Some token ->
	    Printf.sprintf "token %d, #%d" token mcio
	| None ->
	    Printf.sprintf "#%d" mcio in
    "<" ^ basic ^ ">"

  let simple_ifref info =
    let basic =
      match info.Descriptor.field_token, info.Descriptor.field_ref with
      | Some token, _ ->
	  Printf.sprintf "token %d" token
      | None, Descriptor.FieldStatic (Pool.StaticRefInternal sfio) ->
	  Printf.sprintf "#%d" sfio
      |	_ ->
	  assert false in
    "<" ^ basic ^ ">"
	

end

(* This function returns a hash code for a class/interface. *)

let hash info =
  Hashtbl.hash (Descriptor.get_ccio info)

(* This functor returns a description of the specified method. The method must belong to a class (not an interface).
   It must be a concrete (i.e. not abstract) method. *)

module MethodCode (X : sig

  val capfile: file
  val desc: internal_method_reference

end) = struct

  open Descriptor
  open Method

  let fail() =
    fail X.capfile.capname

  (* Extract the handlers which are relevant for this method. *)

  let handlers =
    try
      Array.sub X.capfile.mc.handlers
	X.desc.exception_handler_index X.desc.exception_handler_count
    with Invalid_argument _ ->
      fail();
      assert false

  (* Obtain the entry for this method in the Method Component. Make sure it is a concrete method. Determine how many
     local variables it uses. We include the method's parameters. *)

  let locals, bytecode, n, start =
    try
      let method_info = Method.resolve X.capfile.mc (Standard.option (X.desc.method_offset)) in
      match method_info.code with
      |	Some { max_locals = max_locals; bytecode = (bytecode, instr_count, start) } ->
	  method_info.nargs + max_locals, bytecode, instr_count, start
      |	None ->
	  assert false
    with Assert_failure _ as exc ->
      fail();
      raise exc

  (* Build a code graph. We provide the graph's nodes, and create an edge between each instruction and the instruction
     which follows it in the program text. *)

  type node = Method.node

  let index node =
    node.node_sequential_index

  let successors action node =
    Standard.do_option node.node_next action

  let iter action =
    Hashtbl.iter (fun _ node -> action node) bytecode

  (* [opcode] maps a node to its opcode. At the same time, it resolves several indirect references: it turns jump
     offsets into direct node pointers, and resolves constant pool indices. *)

  let opcode node =

    let resolve_branch branch =
      try
	Hashtbl.find bytecode (node.node_mcio + branch)
      with Not_found ->
	assert false

    and resolve_class =
      Pool.lookup (function
	| Pool.Constant_Classref cref ->
	    Extern.classref X.capfile cref
	| _ ->
	    assert false
      ) X.capfile.pool

    and resolve_field =
      Pool.lookup (function
	| Pool.Constant_InstanceFieldref ifref ->
	    Extern.instance_field_ref X.capfile ifref
	| Pool.Constant_StaticFieldref sfref ->
	    Extern.static_field_ref X.capfile sfref
	| _ ->
	    assert false
      ) X.capfile.pool

    and resolve_method =
      Pool.lookup (function
	| Pool.Constant_StaticMethodref smref ->
	    Extern.static_method_ref X.capfile smref
	| Pool.Constant_SuperMethodref imref ->
	    Extern.instance_method_ref X.capfile imref
	| Pool.Constant_VirtualMethodref vmref ->
	    Extern.instance_method_ref X.capfile vmref
	| _ ->
	    assert false
      ) X.capfile.pool in

    try

      map_opcode (resolve_branch, 
		  resolve_class,
		  resolve_class,
		  resolve_class,
		  resolve_field, 
		  resolve_method) node.node_instruction

    with Assert_failure _ as exc ->
      fail();
      raise exc

end

