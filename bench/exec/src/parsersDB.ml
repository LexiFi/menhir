module MlyParser = struct
  
  type t =
    {
      directory : string;
      filename : string;
      onlysizes : bool;
      additionalfiles : string list;
    }
  
  (* similar to Php explode function *)
  let string_explode char str =
    let rec loop accword acclst i =
      if i = String.length str then
        (
          if (Buffer.length accword) > 0 then
            List.rev ((Buffer.contents accword) :: acclst)
          else List.rev (acclst)
        )
      else
        (
          if str.[i] = char then
            (
              loop (Buffer.create 10)
                ( if (Buffer.length accword) > 0 then
                    ((Buffer.contents accword) :: acclst)
                  else
                    acclst
                )
                ( i + 1)
              
            ) else
            (
              Buffer.add_char accword str.[i];
              loop accword acclst (i + 1)
            )
        ) in
    loop (Buffer.create 10) [] 0
  ;;
  
  (********************************** GETTERS *******************************)
  let get_directory p =
    p.directory
  
  let get_filename p =
    p.filename
  
  let get_basename p =
    Filename.chop_extension p.filename
  
  let get_absolute_filename p =
    Filename.concat p.directory p.filename
  
  let get_additionals p =
    p.additionalfiles
  
  let is_onlysizes p =
    p.onlysizes
  
  let get_generated_directory p = 
    Filename.concat p.directory Settings.generated_dir
   
  (* FIXME hard encoded lexer should be more flexible *)
  let get_lexer p =
    "lexer.mll"
  
  (********************************** SETTERS *******************************)
  let make dir file onlysizes additionalfiles =
    {
      directory = dir;
      filename = file;
      onlysizes = onlysizes;
      additionalfiles = additionalfiles;
    }
  ;;
  
  let from_string str =
    let lst = string_explode ' ' str in
    match lst with
    | filename::"true":: tl ->
        make (Filename.dirname filename) (Filename.basename filename) true tl
    | filename::"false":: tl ->
        make (Filename.dirname filename) (Filename.basename filename) false tl
    | _ -> assert false
  ;;
  
  (********************************* FORMATTERS *****************************)
  
  let to_string p =
    let buf = Buffer.create 1024 in
    Buffer.add_string buf (Printf.sprintf "directory = %s\n" p.directory);
    Buffer.add_string buf (Printf.sprintf "filename  = %s\n" p.filename);
    Buffer.contents buf
  ;;
  
  (* Use for set ordered type. See below *)
  let compare a b =
    String.compare (get_absolute_filename a) (get_absolute_filename b)
  ;;
end

(* Generated parsers set *)
module MlyParserSet = Set.Make (MlyParser);;

let set = ref MlyParserSet.empty;;

let add p =
  set := MlyParserSet.add p !set
;;

let remove p =
  set := MlyParserSet.remove p !set
;;

let read () =
  let f = open_in Settings.parsers_file in
  let rec loop () =
    try
      let line = input_line f in
      if (String.length line) != 0 then
        (
          let parser = MlyParser.from_string line in
          add parser;
          Log.log3 (Printf.sprintf
                "INFO: loading parser configuration for '%s'\n"
                (MlyParser.get_filename parser)
            );
          loop ()
        )
      else
        loop ()
    with End_of_file -> () in
  loop ();
  Log.log1 (Printf.sprintf "-> %d parsers configurations loaded\n" (MlyParserSet.cardinal !set));
  close_in f
;;

let write () =
  let f = open_out Settings.parsers_file in
  MlyParserSet.iter (fun line -> Printf.fprintf f "%s\n" (MlyParser.to_string line)) !set;
  close_out f
;;

let print () =
  MlyParserSet.iter (fun x -> Printf.printf "%s\n" (MlyParser.to_string x)) !set;;

let iter f =
  MlyParserSet.iter f !set;;
