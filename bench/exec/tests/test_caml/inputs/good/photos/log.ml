(* $Header: /home/yquem/cristal/fpottier/cvs/photos/log.ml,v 1.3 2005/04/03 19:02:18 fpottier Exp $ *)

(* ------------------------------------------------------------------------------ *)
(* Initialize LablGTK. *)

let (_ : string) =
  GMain.init()

let utf8 =
  Glib.Convert.locale_to_utf8

let ftu8 =
  Glib.Convert.locale_from_utf8

(* ------------------------------------------------------------------------------ *)
(** Assuming a single-threaded program, we must sometimes explicitly allow GTK to
    handle events, otherwise the progress bar won't be able to repaint. *)
let yield () =
  while Glib.Main.pending() do
    ignore (Glib.Main.iteration false)
  done

(* ------------------------------------------------------------------------------ *)
(** Logging messages. *)

let log_window, log =

  let window = GWindow.window
    ~title:"Messages" () in
  window#set_default_size ~width:384 ~height:512;
  let _ = window#event#connect#delete ~callback:(fun _ -> window#misc#hide(); true) in
  let sw = GBin.scrolled_window
    ~hpolicy:`AUTOMATIC
    ~vpolicy:`AUTOMATIC
    ~shadow_type:`IN
    ~packing:window#add () in
  let buffer = GText.buffer () in
  let view = GText.view
    ~buffer
    ~editable:false
    ~cursor_visible:false
    ~packing:sw#add () in
  window#show ();

  let log text =
    let _ = Sys.command (Printf.sprintf "echo %s >> /home/fpottier/cvs/photos/log" (Filename.quote text)) in
    buffer#insert (utf8 (text ^ "\n"));
    let (_ : bool) = view#scroll_to_iter (buffer#get_iter `END) in
    window#show ();
    yield()
  in

  window, log

