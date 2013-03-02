(* $Header: /home/yquem/cristal/fpottier/cvs/photos/ui.ml,v 1.24 2005/12/11 21:36:47 fpottier Exp $ *)

(* TEMPORARY réfléchir aux diagnostics d'erreur, aux messages d'information et de progrès, aux barres de progrès *)
(* TEMPORARY implanter un diaporama maison avec commandes de rotation *)

open Printf
open Database
open Log

(* ------------------------------------------------------------------------------ *)
(* Storing in-memory versions of the mini thumbnails. *)

let minitable =
  Hashtbl.create 1023

let minipixbuf element =
  try
    Hashtbl.find minitable element
  with Not_found ->
    let fpixbuf = GdkPixbuf.from_file (mini element) in
    let pixbuf = GdkPixbuf.create ~width:96 ~height:96 () in
    GdkPixbuf.fill pixbuf Int32.zero;
    let pixbuf = GdkPixbuf.add_alpha ~transparent:(0,0,0) pixbuf in
    GdkPixbuf.copy_area
      ~dest:pixbuf
      ~dest_x:((96 - GdkPixbuf.get_width fpixbuf) / 2)
      ~dest_y:((96 - GdkPixbuf.get_height fpixbuf) / 2)
      fpixbuf;
    Hashtbl.add minitable element pixbuf;
    pixbuf

let resetminipixbuf element =
  Hashtbl.remove minitable element

(* ------------------------------------------------------------------------------ *)
(* Catching key events for a widget. *)

let on_key widget modifiers keysym action =
  let _ = widget#event#connect#key_press ~callback:(fun (key : GdkEvent.Key.t) ->
    if (GdkEvent.Key.keyval key = keysym) &&
      (List.for_all (fun modifier -> List.mem modifier (GdkEvent.Key.state key)) modifiers) then begin
	action(); true
      end
    else
      false)
  in
  ()

(* ------------------------------------------------------------------------------ *)
(** Displaying a progress dialog. The [steps] parameter counts the number of
    discrete time units which the task is expected to occupy. [progress steps]
    returns a function, [pulse], which must be called exactly [steps] times.
    Every time it is called, the progress bar advances. The last time it is
    called, the progress dialog disappears. *)
let progress steps =
  assert (steps > 0);
  let window = GWindow.window
    ~position:`CENTER () in
  window#set_default_size ~width:280 ~height:100;
  let box = GPack.hbox
    ~border_width:30
    ~spacing:8
    ~packing:window#add () in
  let bar = GRange.progress_bar
    ~packing:box#add () in
  bar#set_text "Veuillez patienter...";
  (* Prevent the user from closing the progress window. *)
  let _ = window#event#connect#delete ~callback:(fun _ -> true) in
  window#show();
  yield();
  let i = ref 0 in
  let pulse () =
    incr i;
    if !i = steps then
      window#destroy ()
    else begin
      bar#set_fraction ((float_of_int !i) /. (float_of_int steps));
      yield()
    end
  in
  pulse

(* ------------------------------------------------------------------------------ *)
(** Displaying a confirmation dialog. If the OK button is pressed, the [action]
    continuation is invoked, otherwise nothing happens. *)
(* TEMPORARY lier la touche RETURN au bouton Oui et Escape à non *)
let ask text action =
  let window = GWindow.window
    ~modal:true
    ~position:`CENTER () in
  window#set_default_size ~width:280 ~height:100;
  let box = GPack.vbox
    ~border_width:30
    ~spacing:20
    ~packing:window#add () in
  let _ = GMisc.label
    ~text
    ~packing:box#pack () in
  let box = GPack.hbox
    ~homogeneous:true
    ~spacing:40
    ~packing:box#pack () in
  let non = GButton.button
    ~stock:`NO
    ~packing:box#pack () in
  let oui = GButton.button
    ~stock:`YES
    ~packing:box#pack () in

  let _ = non#connect#clicked ~callback:window#destroy in
  let _ = oui#connect#clicked ~callback:(fun () ->
    window#destroy();
    action()
  ) in

  window#show ()

(* ------------------------------------------------------------------------------ *)
(** Displaying a text entry dialog. If the OK button is pressed, the [action]
    continuation is invoked with the text input by the user, otherwise nothing
    happens. *)
let request text action =
  let window = GWindow.window
    ~modal:true
    ~position:`CENTER () in
  window#set_default_size ~width:280 ~height:100;
  let box = GPack.vbox
    ~border_width:30
    ~spacing:20
    ~packing:window#add () in
  let _ = GMisc.label
    ~text
    ~packing:box#pack () in
  let entry = GEdit.entry
    ~width:260
    ~packing:box#pack () in
  let box = GPack.hbox
    ~homogeneous:true
    ~spacing:40
    ~packing:box#pack () in
  let non = GButton.button
    ~stock:`CANCEL
    ~packing:box#pack () in
  let oui = GButton.button
    ~stock:`OK
    ~packing:box#pack () in

  let k () =
    let text = ftu8 entry#text in
    window#destroy();
    action text
  in

  let _ = non#connect#clicked ~callback:window#destroy in
  let _ = oui#connect#clicked ~callback:k in
  on_key entry [] GdkKeysyms._Return k;

  window#show ()

(* ------------------------------------------------------------------------------ *)
(** Displaying an information dialog. *)
let info text =
  let window = GWindow.window
    ~modal:true
    ~position:`CENTER () in
  window#set_default_size ~width:280 ~height:100;
  let box = GPack.vbox
    ~border_width:30
    ~spacing:20
    ~packing:window#add () in
  let _ = GMisc.label
    ~text
    ~packing:box#pack () in
  let box = GPack.hbox
    ~homogeneous:true
    ~spacing:40
    ~packing:box#pack () in
  let ok = GButton.button
    ~stock:`OK
    ~packing:box#pack () in

  let _ = ok#connect#clicked ~callback:window#destroy in

  window#show ()

(* ------------------------------------------------------------------------------ *)
(** Printing all of an element's properties as a single string. We assume that
    properties do not contain the '/' character and use it as a separator. *)
let print_properties element =
  match properties element with
  | [] ->
      ""
  | one :: more ->
      List.fold_left (fun property accu -> property ^ "/" ^ accu) one more

let slash =
  Str.regexp "/"

(** Parsing a string of '/'-separated properties. *)
let parse_properties text =
  Str.split slash text

(* ------------------------------------------------------------------------------ *)
(** Create the search window. We create only one such window, since there seems
    to be little need for several. It is hidden by default. We do not initialize
    it entirely at this point, because some of its callbacks rely on the existence
    of the main window, which we haven't created yet. *)
let search_window =
  
  (* Create a window. *)
  let window = GWindow.window ~title:"Recherche" ~position:`CENTER () in

  (* Define its initial size. The window will still be able to shrink or
     grow when resized by the user. On the other hand, specifying height
     and width parameters in the window creation call above would not
     allow it to shrink below the specified size. *)

  window#set_default_size ~width:640 ~height:284;

  (* Closing the window hides it, but does not destroy it, so it may
     be shown again later. *)
  let _ = window#event#connect#delete ~callback:(fun _ -> window#misc#hide(); true) in

  window

(* ------------------------------------------------------------------------------ *)

(** Create the main window, which displays a number of elements. *)
let display_window, fill_model =

  (* Create a window. *)

  let window = GWindow.window ~title:(utf8 "Boîte à photos") () in
  window#set_default_size ~width:1280 ~height:1024;

  (* Since this is the application's main window, closing it should cause
     the application to terminate. *)

  let (_ : GtkSignal.id) = window#connect#destroy ~callback:GMain.quit in

  (* Near the window's top, create the tool bar. Below it, create an area
     surrounded with scroll bars. *)

  let box = GPack.vbox
    ~border_width:8
    ~spacing:8
    ~packing:window#add () in

  let toolbar = GButton.toolbar
    ~style:`BOTH
    ~border_width:5
    ~packing:box#pack () in

  let sw = GBin.scrolled_window
    (* Allow the scroll bars to disappear when there is no need for them. *)
    ~hpolicy:`AUTOMATIC
    ~vpolicy:`AUTOMATIC
    ~shadow_type:`IN
    ~packing:box#add () in

  (* Let us now define a model, that is, a [list store] object populated with
     a list of typed columns. *)

  let columns = new GTree.column_list in
  let element_column = columns#add Gobject.Data.string in
  let date_column = columns#add Gobject.Data.string in
  let thumbnail_column = columns#add Gobject.Data.gobject in
  let caption_column = columns#add Gobject.Data.string in
  let properties_column = columns#add Gobject.Data.string in
  let model = GTree.list_store columns in

  (* We may fill the model with data by creating rows and assining a
     value to every cell. Please note that [latin1] encoded strings
     must be converted to UTF8 first. Note that this function is not
     initially called; the model is, at first, empty. *)

  let fill_model elements =
    let n = List.length elements in
    let pulse = if n > 10 then progress n else fun () -> () in
    sw#misc#hide();
    model#clear();
    List.iter (fun element ->
      let row = model#append () in
      model#set ~row ~column:element_column (utf8 element);
      model#set ~row ~column:date_column (Util.humandate (elementdate element));
      model#set ~row ~column:thumbnail_column (minipixbuf element);
      model#set ~row ~column:caption_column (utf8 (get_caption element));
      model#set ~row ~column:properties_column (utf8 (print_properties element));
      pulse();
    ) elements;
    sw#misc#show()
  in

  (* Define how to react to some editing changes. *)

  let caption_was_edited path text =
    let row = model#get_iter path in
    let element = ftu8 (model#get ~row ~column:element_column) in
    set_caption element (ftu8 text);
    sync();
    model#set ~row ~column:caption_column text

  and properties_was_edited path text =
    let row = model#get_iter path in
    let element = ftu8 (model#get ~row ~column:element_column) in
    set_properties element (parse_properties (ftu8 text));
    sync();
    model#set ~row ~column:properties_column text
  in

  (* Then, we define a view on top of this model. *)

  let view = GTree.view
      ~model
      (* Tell GTK we expect users to read across tree rows and associate cells
	 with one another. By default, GTK will then render the tree with
	 alternating row colors. This is only a semantic hint, however. *)
      ~rules_hint:true
      (* The column headers do not react to click events. *)
      ~headers_clickable:false
      ~packing:sw#add () in
  (* Allow multiple rows to be selected at once. *)
  view#selection#set_mode `MULTIPLE;
  (* A utility function for adding possibly editable text columns. *)
  let append_text_column ?edited title column =
    let title = utf8 title in
    let renderer = 
      match edited with
      |	None ->
	  GTree.cell_renderer_text []
      |	Some callback ->
	  let renderer = GTree.cell_renderer_text [ `EDITABLE true] in
	  let _ = renderer#connect#edited ~callback in
	  renderer
    in
    (* A renderer for this column. We choose a text renderer (see [GtkCellRendererText])
       and specify that its [text] property is to be taken from the model, which means
       that, even though there is a single renderer object for the whole column, the
       text in each row is different. *)
    let (_ : int) = view#append_column (GTree.view_column ~title ~renderer:(renderer, [("text", column)]) ()) in
    ()
  in
  append_text_column "Élément" element_column;
  append_text_column "Date" date_column;
  let (_ : int) = view#append_column (GTree.view_column
    ~title:"Vue"
    ~renderer:(GTree.cell_renderer_pixbuf [], [("pixbuf", thumbnail_column)]) ()
  ) in
  append_text_column ~edited:caption_was_edited "Légende" caption_column;
  append_text_column ~edited:properties_was_edited "Catégories" properties_column;

  (* Finding out which elements are selected. The iterator [fold_selected_elements] is robust,
     that is, it allows [f] to add or delete rows. *)

  let fold_selected_elements show_progress accu f =
    let rows = view#selection#get_selected_rows in
    let n = List.length rows in
    if n > 0 then
      let pulse =
	if show_progress then
	  progress n
	else
	  function () -> ()
      in
      List.fold_left (fun accu rowref ->
	assert (GtkTree.RowReference.valid rowref);
	let row = model#get_iter (GtkTree.RowReference.get_path rowref) in
	let element = ftu8 (model#get ~row ~column:element_column) in
	let accu = f accu row element in
	pulse();
	accu
      ) accu (List.map (GtkTree.RowReference.create model#as_model) rows)
    else
      accu
  in

  let iter_selected_elements show_progress f =
    fold_selected_elements show_progress () (fun () row element -> f row element)
  in

  let selected_elements () =
    fold_selected_elements false [] (fun accu _ element -> element :: accu)
  in

  (* Responding to user commands. *)

  (** Downloading files off the camera. *)
  let download () =
    log_window#present();

    (* Mount the camera's file system. *)

    Util.command (sprintf "mount %s" Config.camera);

    (* Copy all files from the camera to the repository. *)

    let srcdir = Config.camera ^ Config.camera_files in
    let elements = ref [] in
    Util.for_each_photo srcdir (function filename ->

      let srcname = srcdir ^ filename in
      let dstname = Config.repository ^ filename in

      (* If the file already exists in the repository, do not copy it;
	 we might risk overwriting an old photo with the same name. *)

      if Sys.file_exists dstname then
	log (sprintf "File %s already exists in %s -- skipped." filename Config.repository)

      else begin

	(* Try to copy the file. If this succeeds, delete it on the camera. *)

	log (sprintf "Copie de %s..." filename);
	Util.command (sprintf "/bin/cp %s %s" (Filename.quote srcname) Config.repository);
	elements := filename :: !elements;
	Cache.add filename;
	Sys.remove srcname

      end

    );

    (* Unmount the file system. *)

    Util.command (sprintf "umount %s" Config.camera);

    (* Update the display. *)

    fill_model (List.rev !elements)
  in

  let gimp () =
    let elements = sort_by_date (selected_elements()) in
    Util.command ((Util.mapply "gimp" (List.map original elements)) ^ "&")
  in

  let destroy_some_selected_elements predicate () =
    iter_selected_elements false (fun row element ->
      if predicate element then begin
        Cache.remove None element;
	let (_ : bool) = model#remove row in
	()
      end
    )
  in

  let destroy () =
    ask (utf8 "Voulez-vous réellement détruire les éléments sélectionnés?") (
      destroy_some_selected_elements (fun _ -> true)
    )
  in

  (* TEMPORARY diaporama plante lorsqu'aucune image n'est sélectionnée *)
  let diaporama () =
    let elements = sort_by_date (selected_elements()) in

    (* Create a temporary slideshow directory containing symbolic links to
       the actual files. This is used to work around a problem with qiv,
       whose "delete" function does not work when the files reside on a
       DOS filesystem. *)

    let temp = "/tmp/diaporama/" in
    Util.command1 "/bin/rm -rf" temp;
    Util.command1 "/bin/mkdir -p" temp;

    let temporary element =
      temp ^ element  in

    List.iter (fun element ->
      Util.command2 "/bin/ln -s" (original element) (temporary element)
    ) elements;

    (* Run the slide show. qiv options are:
       -f (full screen)
       -i (no status bar)
       -s (slideshow)
       -d <seconds> (slideshow delay)
       -t shrink down images to fit screen *)

    Util.command (Util.mapply "qiv -f -i -t" (List.map temporary elements));

    (* If the user chose to delete some photos, then they were moved by qiv
       to the trash directory. Detect this and suppress the originals. *)

    let trash = temp ^ ".qiv-trash/" in
    if Sys.file_exists trash then
      destroy_some_selected_elements (fun element -> Sys.file_exists (trash ^ element)) ();

    (* Remove the temporary directory. *)

    Util.command1 "/bin/rm -rf" temp

  in

  let rotate direction () =
    iter_selected_elements true (fun row element ->
      Image.rotate direction (original element);
      resetminipixbuf element;
      model#set ~row ~column:thumbnail_column (minipixbuf element)
    )
  in

  let email () =
    let attachments = fold_selected_elements true "" (fun accu _ element ->
      sprintf "%s -a %s" accu (Filename.quote (reduced element))
    ) in
    Util.command (sprintf "xterm -e /usr/bin/mutt %s &" attachments)
  in

  let background () =
(*
    let width, height =
(*    let screen = Gdk.Screen.default() in (* requires GTK 2.2 *)
      Gdk.Screen.width ~screen (), Gdk.Screen.height ~screen () *)
      1365, 1024
    in *)
    (* TEMPORARY vérifier que la sélection est réduite à un seul élément? *)
    iter_selected_elements false (fun _ element ->
      Util.command1 "gconftool-2 --type string --set /desktop/gnome/background/picture_filename" "scaled";
      Util.command1 "gconftool-2 --type string --set /desktop/gnome/background/picture_filename" (original element)
    )
  in

  let gallery () =
    request (utf8 "Quel nom donner à cet album?") (fun titre ->
      let module M = Xhtml.Make (struct
	let elements = selected_elements ()
	let title = titre
	let subdirectory = titre
	let pulse = progress (List.length elements)
      end) in
      let gallerydir = M.gallery() in
      Util.command (sprintf "/usr/bin/mozilla -P default %sindex.html &" (Filename.quote gallerydir))
    )
  in

  let classer () =
    request (utf8 "Quelle(s) catégorie(s) souhaitez-vous ajouter aux éléments sélectionnés?") (fun text ->
      let properties = parse_properties text in
      iter_selected_elements false (fun row element ->
	add_properties element properties;
	model#set ~row ~column:properties_column (utf8 (print_properties element))
      );
      sync()
    )
  in

  let dvd () =
    request (utf8 "À partir de quelle date souhaitez-vous sauvegarder vers DVD?") (fun text ->
      let lexbuf = Lexing.from_string text in
      try
	let module M = Backup.Backup (struct
	  let start, _ = Queryl.date lexbuf
	  let progress = progress
	end) in
	()
      with Search.SyntaxError (_, msg) ->
	info (utf8 msg)
    )
  in

  (* Populate the tool bar. *)

  let insert text ?file ?stock callback =
    let _ = toolbar#insert_button
      ~text:(utf8 text)
      ~icon:(GMisc.image ?file ?stock ~icon_size:`DIALOG ())#coerce
      ~callback () in
    ()
  in

  insert "Acquérir" ~file:"/home/fpottier/cvs/photos/e10-48x48.png" download;
  insert "Diaporama" ~stock:`ZOOM_100 diaporama;
  insert "GIMP" ~file:"/usr/share/icons/Bluecurve/48x48/apps/gimp.png" gimp;
  insert "Tourner" ~stock:`UNDO (rotate Image.Left);
  insert "Tourner" ~stock:`REDO (rotate Image.Right);
  insert "Classifier" ~file:"/usr/share/pixmaps/gdict.png" classer;
  insert "Détruire" ~stock:`DELETE destroy;
  insert "Envoyer" ~file:"/usr/share/pixmaps/redhat-email.png" email;
  insert "Fond d'écran" ~file:"/usr/share/icons/Bluecurve/48x48/apps/background-capplet.png" background;
  insert "Rechercher" ~stock:`FIND search_window#present;
  insert "Créer un album" ~file:"/usr/share/pixmaps/redhat-web-browser.png" gallery;
  insert "Créer un DVD" ~stock:`CDROM dvd;
  insert "Quitter" ~stock:`CLOSE window#destroy;
  (* TEMPORARY recopier les icones en local *)
  (* TEMPORARY griser certaines icones lorsque la sélection est vide *)
  (* TEMPORARY manque un raccourci pour tout sélectionner, ou plutot il existe, mais pas toujours actif *)

  on_key window [] GdkKeysyms._Return diaporama; (* TEMPORARY un peu violent; actif meme en mode edition *)
  on_key window [] GdkKeysyms._Escape window#destroy;
  (* on_key window [ `CONTROL ] GdkKeysyms._c classer; TEMPORARY trouver une autre touche que ^C *)
  on_key window [ `CONTROL ] GdkKeysyms._a (fun () -> view#selection#select_all);
  (* TEMPORARY mieux vaudrait donner le focus au bon widget *)
  (* sw#focus#set (Some view#coerce); *)

  (* We're done. Let the user interact. *)

  window#show ();

  window, fill_model

(* ------------------------------------------------------------------------------ *)
(* Populate the search window. *)

let syntax = [
  "true", "filtre tous les éléments";
  "name \"photo\"", "filtre l'élément dont le nom de fichier est \"photo\"";
  "before 2003/12/31", "filtre les éléments antérieurs au 31 décembre 2003";
  "on 2004/01/01", "filtre les éléments datés du premier janvier 2004";
  "after 2004/02/28", "filtre les éléments postérieurs au 28 février 2004";
  "today", "filtre les éléments datés d'aujourd'hui";
  "yesterday", "filtre les éléments datés d'hier";
  "matches \"R\"", "filtre les éléments dont la légende ou la description satisfait l'expression régulière R";
  "has \"Marcel\"", "filtre les éléments appartenant à la catégorie \"Marcel\"";
  "not C", "filtre les éléments non filtrés par le critère C";
  "C<sub>1</sub> and C<sub>2</sub>", "filtre les éléments filtrés par les critères C<sub>1</sub> et C<sub>2</sub>";
  "C<sub>1</sub> or C<sub>2</sub>", "filtre les éléments filtrés par les critères C<sub>1</sub> ou C<sub>2</sub>"
]

let syntax =
  "Syntaxe des critères de recherche:\n\n" ^
  List.fold_right (fun (example, comment) syntax ->
    sprintf "<span foreground=\"blue\">%s</span> %s\n" example comment ^ syntax
  ) syntax ""

let _ =

  let box = GPack.vbox
    ~border_width:8
    ~spacing:8
    ~packing:search_window#add () in

  let _ = GMisc.label
    ~text:(utf8 "Veuillez fournir un critère de recherche...")
    ~xalign:0.0
    ~justify:`LEFT
    ~packing:box#pack () in
  let _ = GMisc.separator `HORIZONTAL
    ~packing:box#pack () in

  let hbox = GPack.hbox
    ~spacing:8
    ~packing:box#pack () in

  let entry = GEdit.entry
    ~width:512
    ~packing:(hbox#pack ~from:`START) () in

  let button = GButton.button
    ~label:"Rechercher"
    ~packing:(hbox#pack ~from:`END ~expand:true) () in

  let _ = GMisc.separator `HORIZONTAL
    ~packing:box#pack () in
  let _ = GMisc.label
    ~xalign:0.0
    ~markup:(utf8 syntax)
    ~packing:box#pack () in

  (* How to search. *)
  (* TEMPORARY these progress dialogs are a mess, think multi-threaded? *)

  let search () =
    let text = entry#text in
    let lexbuf = Lexing.from_string (ftu8 text) in
    try
      let query = Queryp.main Queryl.token lexbuf in
      search_window#misc#hide();
      fill_model (Search.search query) (* TEMPORARY dialogue de progression? *)
    with
    | Queryp.Error ->
	let start = lexbuf.Lexing.lex_start_p.Lexing.pos_cnum
	and stop =  lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum in
	entry#select_region ~start ~stop;
	info (utf8 "Votre requête n'est pas grammaticalement correcte.")
    | Search.SyntaxError (start, msg) ->
	entry#select_region ~start ~stop:(start+1);
	info (utf8 "Votre requête n'est pas grammaticalement correcte: " ^ msg)
    | Queryl.IllegalCharacter (start, stop) ->
	entry#select_region ~start ~stop;
	info (utf8 "Votre requête n'est pas lexicalement correcte.")
  in

  (* Bind the search button and the Return key to the above callback. *)

  let _ = button#connect#clicked ~callback:search in
  on_key entry [] GdkKeysyms._Return search;

  (* We're done. *)
  
  ()

(* ------------------------------------------------------------------------------ *)
(* Main program. *)

let _ =
  GMain.main()

