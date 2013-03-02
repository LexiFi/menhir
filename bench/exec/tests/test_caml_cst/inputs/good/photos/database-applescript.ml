(* $Header: /home/yquem/cristal/fpottier/cvs/photos/database-applescript.ml,v 1.1 2006/02/09 21:15:06 fpottier Exp $ *)

open Util
open Info

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Printing information records in XML format. *)

module Print = struct

  open Printf

  let properties f props =
    StringSet.iter (fun prop ->
      fprintf f " <property>%s</property>" prop
    ) props

  let information f info =
    fprintf f "<caption>%s</caption>%a" info.caption properties info.properties

  let infomap f m =
    StringMap.iter (fun element info ->
      fprintf f "<element name=\"%s\">\n  %a\n</element>\n" element information info
    ) m

end

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Reading and writing the file that contains all information records. *)

let read () =
  let f = open_in "photos-data" in
  let lexbuf = Lexing.from_channel f in
  let infos = Xml.elements StringMap.empty lexbuf in
  close_in f;
  infos

let infos =
  ref (read())

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Getting and setting an element's information record. *)

let get_info element =
  try
    StringMap.find element !infos
  with Not_found ->
    {
      caption = "";
      properties = StringSet.empty
    }

let set_info element info =
  infos := StringMap.add element info !infos

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Getting and setting an element's caption. *)

let get_caption element =
  (get_info element).caption

let set_caption element caption =
  set_info element { (get_info element) with caption = caption }

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Getting and setting an element's properties. *)

(** Find all properties of an element. *)
let properties_set element =
  (get_info element).properties

let properties element =
  StringSet.elements (properties_set element)

(** Check whether a property holds of an element. *)
let get_property element property =
  StringSet.mem property (properties_set element)

(** Set/clear a property of an element. *)
let set_property element flag property =
  let info = get_info element in
  let properties =
    (if flag then StringSet.add else StringSet.remove) property info.properties
  in
  set_info element { info with properties = properties }

(** Add a set of properties to an element at once. *)
let add_properties element properties =
  let info = get_info element in
  set_info element { info with properties = StringSet.union (StringSet.of_list properties) info.properties }

(** Set all properties of an element at once. *)
let set_properties element properties =
  let info = get_info element in
  set_info element { info with properties = StringSet.of_list properties }

(** Find all properties of a list of elements. *)
let properties_many elements =
  eliminate_duplicates (List.flatten (List.map properties elements))

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Producing a list of all keywords. *)

open Printf

let out f s =
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '"' ->
        fprintf f "\\\""
    | c ->
        fprintf f "%c" c
  done

let () =
  StringMap.iter (fun element info ->
    if info.caption <> "" then begin
      let script = "ordres" in
      let c = open_out script in
      fprintf c "tell application \"iPhoto\"\n";
      fprintf c "  repeat with p in (every photo whose image filename is \"%s\")\n" element;
      fprintf c "    set title of p to \"";
      out c info.caption;
      fprintf c "\"\n";
      fprintf c "  end repeat\n";
      fprintf c "end tell\n";
      close_out c;
      let (_ : int) = Sys.command (sprintf "/opt/local/bin/recode latin1..unicode %s" script) in
      let (_ : int) = Sys.command (sprintf "osascript %s" script) in
      ()
    end
  ) !infos

let () =
  exit 0

let () =
  StringMap.iter (fun element info ->
    let script = "ordres" in
    let c = open_out script in
    if not (StringSet.is_empty info.properties) then begin
      fprintf c "tell application \"iPhoto\"\n";
      fprintf c "  select (every photo whose image filename is \"%s\")\n" element;
      StringSet.iter (fun keyword ->
        fprintf c "  assign keyword string \"%s\"\n" keyword
      ) info.properties;
      fprintf c "end tell\n";
      close_out c;
      let (_ : int) = Sys.command (sprintf "/opt/local/bin/recode latin1..unicode %s" script) in
      let (_ : int) = Sys.command (sprintf "osascript %s" script) in
      ()
    end
  ) !infos

