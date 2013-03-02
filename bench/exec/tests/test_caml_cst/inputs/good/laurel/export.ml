(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/export.ml,v 1.11 2000/03/04 07:07:26 fpottier Exp $ *)

(* This module parses JavaCard 2.1 ``export'' files. *)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Type definitions} *)

type file = {
    cpool: cpool_entry array;
    this_package: cpool_index;
    classes: class_info list;
    exportname: string (* used when reporting errors *)
  } 

and cpool_index =
    int

and token =
    int

and cpool_entry =
  | Constant_Package of constant_package_info
  | Constant_Classref of cpool_index
  | Constant_Integer of int (* TEMPORARY should be unsigned 32-bit, not signed 31-bit *)
  | Constant_Utf8 of string

and constant_package_info = {
    flags: package_flag list;
    package_name_index: cpool_index;
    minor: int;
    major: int;
    aid: string
  } 

and package_flag =
  | ACC_LIBRARY

and class_info = {
    class_token: token;
    class_access_flags: access_flag list;
    class_name_index: cpool_index;
    supers: cpool_index list;
    interfaces: cpool_index list;
    mutable fields: field_info list;
    mutable methods: method_info list
  } 

and access_flag =
  | ACC_PUBLIC
  | ACC_PROTECTED
  | ACC_STATIC
  | ACC_FINAL
  | ACC_INTERFACE
  | ACC_ABSTRACT
  | ACC_SHAREABLE

and field_info = {
    field_owner: class_info;
    field_token: token;
    field_access_flags: access_flag list;
    field_name_index: cpool_index;
    field_descriptor_index: cpool_index;
    attributes: attribute_info list
  } 

and attribute_info =
  | ConstantValue of cpool_index

and method_info = {
    method_owner: class_info;
    method_token: token;
    method_access_flags: access_flag list;
    method_name_index: cpool_index;
    method_descriptor_index: cpool_index
  } 

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Error handling} *)

(* Errors are signaled using [assert false], which has the advantage of printing detailed position information.
   Other error messages are printed as side effects. *)

let fail exportname =
  Printf.eprintf "Inconsistent export file: %s\n" exportname;
  flush stderr

let protect action file =
  try
    action file
  with Assert_failure _ as exc ->
    fail file.exportname;
    raise exc

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Decoding functions} *)

let rec load exportname =
  Verbose.say (Printf.sprintf "Loading export file %s...\n" exportname);
  try
    file exportname (Io_utils.read_whole_file exportname)
  with
  | Sys_error msg ->
      Printf.eprintf "System error while reading %s: %s\n" exportname msg;
      flush stderr;
      assert false
  | Assert_failure _ as exc ->
      fail exportname;
      raise exc

and file exportname buffer =
  let zone = Zone.create buffer in
  try

    (* Check the magic number. *)

    assert ((Zone.u2 zone = 0x00FA) & (Zone.u2 zone = 0xCADE));

    (* Read in the minor and major version numbers. Fail if they do not have the expected value, as required by
       JavaCard's specification. *)

    assert ((Zone.u1 zone = 1) & (Zone.u1 zone = 2));

    (* Read in the constant pool, and convert it to an array. *)

    let cpool = Array.of_list (Zone.table zone Zone.u2 cpool_entry) in

    (* Read in [this_package]. *)

    let this_package = Zone.u2 zone in
      
    (* Read in the class table. *)

    let classes = Zone.table zone Zone.u1 class_info in

    (* Make sure we have read everything. *)

    assert (Zone.head zone = String.length buffer);

    (* Return a data structure. *)

    {
      cpool = cpool;
      this_package = this_package;
      classes = classes;
      exportname = exportname
    } 

  with Zone.EOZ ->
    assert false

and cpool_entry zone =
  match Zone.u1 zone with
  | 13 ->
      Constant_Package (constant_package_info zone)
  | 7 ->
      Constant_Classref (Zone.u2 zone)
  | 3 ->
      Constant_Integer (Zone.s4 zone)
  | 1 ->
      Constant_Utf8 (Zone.pascal zone Zone.u2)
  | _ ->
      assert false

and constant_package_info zone =

  (* Read in the package flags. *)

  let flags =
    match Zone.u1 zone with
    | 1 ->
	[ ACC_LIBRARY ]
    | 0 ->
	[]
    | _ ->
	assert false in

  (* Read in the package's name index. *)

  let name_index = Zone.u2 zone in

  (* Read in the package's version numbers. *)

  let minor = Zone.u1 zone in
  let major = Zone.u1 zone in

  (* Read in the package's AID. *)

  let aid = Zone.pascal zone Zone.u1 in

  (* Return a data structure. *)

  {
    flags = flags;
    package_name_index = name_index;
    minor = minor;
    major = major;
    aid = aid
  } 

and class_info zone =

  (* Read in the class token. *)

  let token = Zone.u1 zone in

  (* Read in the access flags. *)

  let access_flags = access_flags zone in

  (* Read in the name index. *)

  let name_index = Zone.u2 zone in

  (* Read in the superclasses. *)

  let supers = Zone.table zone Zone.u2 Zone.u2 in

  (* Read in the interfaces. *)

  let interfaces = Zone.table zone Zone.u1 Zone.u2 in

  (* Create a data structure. *)

  let class_info = {
    class_token = token;
    class_access_flags = access_flags;
    class_name_index = name_index;
    supers = supers;
    interfaces = interfaces;
    fields = [];
    methods = []
  } in

  (* Read in the fields. *)

  let fields = Zone.table zone Zone.u2 (field_info class_info) in

  (* Read in the methods. *)

  let methods = Zone.table zone Zone.u2 (method_info class_info) in

  class_info.fields <- fields;
  class_info.methods <- methods;
  class_info

and field_info class_info zone =

  (* Read in the field token. *)

  let token = Zone.u1 zone in

  (* Read in the access flags. *)

  let access_flags = access_flags zone in

  (* Read in the name index. *)

  let name_index = Zone.u2 zone in

  (* Read in the descriptor index. *)

  let descriptor_index = Zone.u2 zone in

  (* Read in the attributes. *)

  let attributes = Zone.table zone Zone.u2 attribute_info in

  (* Return a data structure. *)

  {
    field_owner = class_info;
    field_token = token;
    field_access_flags = access_flags;
    field_name_index = name_index;
    field_descriptor_index = descriptor_index;
    attributes = attributes
  }   

and method_info class_info zone =
  
  (* Read in the method token. *)

  let token = Zone.u1 zone in

  (* Read in the access flags. *)

  let access_flags = access_flags zone in

  (* Read in the name index. *)

  let name_index = Zone.u2 zone in

  (* Read in the descriptor index. *)

  let descriptor_index = Zone.u2 zone in

  (* Return a data structure. *)

  {
    method_owner = class_info;
    method_token = token;
    method_access_flags = access_flags;
    method_name_index = name_index;
    method_descriptor_index = descriptor_index
  }     

and attribute_info zone =

  (* Skip the [attribute_name_index] and the [attribute_length]. *)

  Zone.seek zone (Zone.head zone + 6);

  (* Return the [constantvalue_index]. *)

  ConstantValue (Zone.u2 zone)

and access_flags zone =
  Standard.test (Zone.u2 zone) [
    0x0001, ACC_PUBLIC;
    0x0004, ACC_PROTECTED;
    0x0008, ACC_STATIC;
    0x0010, ACC_FINAL;
    0x0200, ACC_INTERFACE;
    0x0400, ACC_ABSTRACT;
    0x0800, ACC_SHAREABLE
  ]

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Utility functions} *)

(* This function returns the name of the file where the data was read. *)

let filename file =
  file.exportname

(* This function accepts a [file], a constant pool index, and returns the corresponding constant pool entry. *)

let pool file index =
  try
    file.cpool.(index)
  with Invalid_argument _ ->
    assert false

(* This function accepts a [file], a constant pool index, and returns the contents of the corresponding constant pool
   entry, assuming it is a [Constant_Utf8] entry. *)

let pool_Utf8 file index =
  match pool file index with
  | Constant_Utf8 name ->
      name
  | _ ->
      assert false

(* This function accepts a [file], a constant pool index, and returns the name of the class it represents, assuming it
   is a [Constant_Classref] entry. *)

let pool_classref file index =
  match pool file index with
  | Constant_Classref index ->
      pool_Utf8 file index
  | _ ->
      assert false

(* This function accepts a [file] and returns its [constant_package_info] structure. *)

let package_info file =
  match pool file file.this_package with
  | Constant_Package info ->
      info
  | _ ->
      assert false

(* This function accepts a [file] and returns the name of the package contained in it. *)

let package_name file =
  pool_Utf8 file (package_info file).package_name_index

(* This function accepts a [file] and returns the AID of the package contained in it. *)

let package_aid =
  protect (function file -> (package_info file).aid)

(* These functions map entities to their names. Class names are fully qualified; field/method names are not. *)

let class_name =
  protect (fun file class_info ->
    pool_classref file class_info.class_name_index
  )

let field_name =
  protect (fun file field_info ->
    pool_Utf8 file field_info.field_name_index
  )

let method_name =
  protect (fun file method_info ->
    pool_Utf8 file method_info.method_name_index
  )

(* This function accepts a [class_info] and returns the name of its superclass, if any. The [class_info] structure
   must represent a class (not an interface). *)

let superclass =
  protect (fun file info ->
    match info.supers with
    | [] ->
	None
    | [ index ] ->
	Some (pool_classref file index)
    | _ :: _ :: _ ->
	assert false
  )

(* This function looks for a [class_info] structure carrying the specified class token. *)

let resolve file class_token =
  try
    List.find (fun info ->
      info.class_token = class_token
    ) file.classes
  with Not_found ->
    Printf.eprintf "Class token %d not found within export file %s\n"
      class_token file.exportname;
    flush stderr;
    assert false

(* This function looks for a [field_info] structure carrying the specified field token. *)

let resolve_field exportfile class_info field_token =
  try
    List.find (fun info ->
      info.field_token = field_token
    ) class_info.fields
  with Not_found ->
    Printf.eprintf "Field token %d not found within class %s\n"
      field_token (class_name exportfile class_info);
    flush stderr;
    assert false

(* This function looks for a [method_info] structure carrying the specified method token. *)

let resolve_method exportfile class_info method_token =
  try
    List.find (fun info ->
      info.method_token = method_token
    ) class_info.methods
  with Not_found ->
    Printf.eprintf "Method token %d not found within class %s\n"
      method_token (class_name exportfile class_info);
    flush stderr;
    assert false

