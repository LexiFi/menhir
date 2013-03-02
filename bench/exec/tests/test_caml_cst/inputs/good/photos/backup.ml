module Backup (P : sig

  (* The starting date. *)
  val start: float

  (* How to display a progress dialog. *)
  val progress: int -> (unit -> unit)

end) = struct

  open Printf
  open Database
  open Log

  (* Compute a list of all elements that are newer than the start
     date. *)

  let newer : string list Cache.DateMap.t =
    Cache.DateMap.above P.start !Cache.map

  (* Truncate the list so that the size of the originals does not
     exceed the size of a DVD. *)

  let gigabyte =
    1024 * 1024

  let threshold =
    4508876 (* 4.3GB *)

  let accepted, size, rejected, min_date, max_date =
    Cache.DateMap.fold (fun date elements accu ->
      List.fold_right (fun element (accepted, size, rejected, min_date, max_date) ->
	if size > threshold then
	  accepted, size, true, min_date, max_date
	else
	  Cache.ElementSet.add element accepted,
	  size + Image.size (original element),
	  false,
	  min date min_date,
	  max date max_date
      ) elements accu
    ) newer (Cache.ElementSet.empty, 0, false, max_float, min_float)

  let accepted =
    Cache.ElementSet.elements accepted

  let n =
    List.length accepted

  (* Display some information. *)  

  let () =
    if rejected then
      log "Tous les éléments ne tiennent pas sur un seul DVD."
    else
      log "Tous les éléments tiennent sur un seul DVD."

  let () =
    log (sprintf "Sélectionné %d éléments, pour un total de %.3f giga-octets."
	   n
	   ((float_of_int size) /. (float_of_int gigabyte)))

  let () =
    log (sprintf "La période concernée s'étale du %s\n au %s."
	   (Util.humandate min_date)
	   (Util.humandate max_date))

  let () =
    log_window#present()

  (* Create an image directory. We copy the entire data file to each
     DVD, as it is reasonably small, and part of the Originals
     directory. *)

  let () =
    Util.command1 "/bin/rm -rf" Config.dvd_root;
    Util.command1 "/bin/mkdir" Config.dvd_root;
    Util.command2 "/bin/cp -a" Config.datafile Config.dvd_datafile;
    Util.command1 "/bin/mkdir" Config.dvd_repository;
    let pulse = P.progress n in
    List.iter (fun element ->
      Util.command2 "/bin/cp -a" (original element) Config.dvd_repository;
      pulse()
    ) accepted;
    log "Copie terminée. Vérification de la copie...";
    let pulse = P.progress n in
    List.iter (fun element ->
      if Sys.command
	  (sprintf "diff %s %s"
	     (Filename.quote (original element))
	     (Filename.quote Config.dvd_repository)) <> 0 then
	log (sprintf "La comparaison a échoué pour %s." element);
      pulse()
    ) accepted;
    log "Vérification terminée."

end

