open Util;;

module Input = struct
    
  type t =
    {
      filename       : string;
      directory      : string;
      expectingerror : bool;
    }
  
  (***************************************** GETTERS **************************)
  let get_filename i =
    i.filename
  ;;
  
  let get_directory i =
    i.directory
  ;;
  
  let get_expecting_error i =
    i.expectingerror
  ;;
  
  let get_fullname i =
    Filename.concat i.directory i.filename
  ;;

  (*************************************** SETTERS **************************)
  
  let make directory subdirectory dirname filename =
    Log.logi (Printf.sprintf "found input file : '%s'\n" filename);
    let expectingerror =
      (
        match (subdirectory) with
        | "good" -> false
        | "bad" -> true
        | _ -> assert false
      ) in
    {
      filename = filename;
      directory = Filename.concat(Filename.concat directory subdirectory) dirname ;
      expectingerror = expectingerror;
    }
  ;;
  
  (************************************** FORMAT *****************************)
  
  let to_string i =
    let buf = Buffer.create 1024 in
    Buffer.add_string buf (Printf.sprintf "{\n");
    Buffer.add_string buf (Printf.sprintf "  filename        = %s\n" (get_filename i));
    Buffer.add_string buf (Printf.sprintf "  directory       = %s\n" (get_directory i));
    Buffer.add_string buf (Printf.sprintf "  expecting_error = %s\n" (string_of_bool(get_expecting_error i)));
    Buffer.add_string buf (Printf.sprintf "}\n");
    Buffer.contents buf
  
end

(* TODO move in a functor in order to not allocate if not necessary *)
let inputs_good_htable = Hashtbl.create 10
let inputs_bad_htable = Hashtbl.create 10

(* scan [inputs] directory for subdirectories that must correspond to start symbols *)
(* the behavior is undefined if subdirectories are not entry symbols *)
(* each subdirectory is scanned for [good] and [bad] subsubdirectories *)
(* the [good] and [bad] subsubdirectories are then scanned for input files *)
let scan_inputs generated =
  let parser = GeneratedParser.Generated.get_parser generated in
  let directory = Filename.concat (ParsersDB.MlyParser.get_directory parser)
      Settings.inputs_dir in
  pushd (Filename.concat directory "good");
  let goods = scan_files "" "." in
  let num = ref 0 in
  let name_dir s = let d = Filename.dirname s in 
  let length = String.length d in
  if length <2 then
      ""
  else
    (* FIXME UGLY HACK *)
    String.sub d 2 (String.length d -2) in 
  let goodlist = List.map (fun file -> num := !num + 1; Input.make (Settings.inputs_dir) "good" (name_dir file) (Filename.basename file)) goods in
  popd();
  pushd (Filename.concat directory "bad");
  let bads = scan_files "" "." in
  let badlist = List.map (fun file -> num := !num + 1; Input.make (Settings.inputs_dir) "bad" (name_dir file) (Filename.basename file)) bads in
  popd();
  Log.log2 (Printf.sprintf "\tfound %d inputs files for parser '%s'\n"
        !num
        (GeneratedParser.Generated.get_basename generated)
    );
  Hashtbl.add inputs_good_htable generated goodlist;
  Hashtbl.add inputs_bad_htable generated badlist;
;;

let get_good_inputs_list parser =
  Hashtbl.find inputs_good_htable parser
;;

let get_bad_inputs_list parser =
  Hashtbl.find inputs_bad_htable parser
;;

let get_all_inputs_list parser =
  (get_good_inputs_list parser)@(get_bad_inputs_list parser)
;;

let print () =
  let f parser inputs =
    Printf.printf "---\n";
    List.iter (fun input -> Printf.printf "input : %s" (Input.to_string input )) inputs;
    Printf.printf "---\n" in
  Hashtbl.iter f inputs_good_htable;
  Hashtbl.iter f inputs_bad_htable;
;;
