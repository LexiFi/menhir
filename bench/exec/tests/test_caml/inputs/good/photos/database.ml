(* $Header: /home/yquem/cristal/fpottier/cvs/photos/database.ml,v 1.18 2005/12/11 21:36:46 fpottier Exp $ *)

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
  let f = open_in Config.datafile in
  let lexbuf = Lexing.from_channel f in
  let infos = Xml.elements StringMap.empty lexbuf in
  close_in f;
  infos

let infos =
  ref (read())

let sync () =
  let tmpname = "/tmp/photos-data" in
  let f = open_out tmpname in
  Print.infomap f !infos;
  close_out f;
  command2 "/bin/mv -f" tmpname Config.datafile

let () =
  at_exit sync

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
(* Image files. *)

(** Find all currently known elements. *)
let all_elements () =
  all_photos Config.repository

(** The original photo file associated with an element. *)
let original element =
  Config.repository ^ element

(** The thumbnail associated with an element. Created or updated if
    necessary. *)
let thumbnail element =
  let srcname = original element
  and dstname = Config.thumbnails ^ element in
  Util.dateprotect Image.postcard srcname dstname;
  dstname

(** The reduced version of an element. Created or updated if
    necessary. *)
let reduced element =
  let srcname = original element
  and dstname = Config.reduced ^ element in
  Util.dateprotect (Image.reduce "'1024x768>'" 75) srcname dstname;
  dstname

(** The mini version of an element. Created or updated if
    necessary. *)
let mini element =
  let srcname = original element
  and dstname = Config.mini ^ element in
  Util.dateprotect (Image.reduce "'96x96>'" 50) srcname dstname;
  dstname

(** Determining an element's date. *)
let elementdate element =
  Image.creationdate (original element)

(** Ordering elements by date. *)
(* Since [elementdate] is slow, we arrange to call it at most once per element.
   This might reap a small constant factor. *)
let sort_by_date elements =
  let elements = List.map (fun element -> elementdate element, element) elements in
  let elements = List.sort (fun (date1, _) (date2, _) ->
    if date1 < date2 then -1 else if date1 = date2 then 0 else 1
  ) elements in
  List.map snd elements

(** Suppressing an element. *)
let suppress element =
  infos := StringMap.remove element !infos;
  command1 "/bin/rm -f" (original element);
  command1 "/bin/rm -f" (Config.thumbnails ^ element);
  command1 "/bin/rm -f" (Config.reduced ^ element);
  command1 "/bin/rm -f" (Config.mini ^ element)

