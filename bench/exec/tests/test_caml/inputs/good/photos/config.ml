(* $Header: /home/yquem/cristal/fpottier/cvs/photos/config.ml,v 1.15 2005/12/11 21:36:46 fpottier Exp $ *)

(** Mount point for the camera (which appears as a USB mass storage device). *)
let camera =
  "/mnt/olympus/"

(** Directory where the camera stores the photos. *)
let camera_files =
  "dcim/100olymp/"

(** Root directory for the photo repository. *)
let root =
(* "/home/madiran/fpottier-home-backup/Photos/" *)
  "/mnt/storage/Photos/"

(** The file that stores the cache. *)
let cache =
  root ^ "cache"

(** Directory where original photos are stored. *)
let repository =
  root ^ "Originals/"

(** Directory where mini thumbnails (used for GTK presentation) are stored. *)
let mini =
  root ^ "Mini/"

(** Directory where thumbnails (used for Web presentation) are stored. *)
let thumbnails =
  root ^ "Thumbnails/"

(** Directory where reduced versions of the images (used for Web presentation) are stored. *)
let reduced =
  root ^ "Reduced/"

(** Main data file. *)
let datafile =
  root ^ "photos-data"

(** Directory where albums (Web presentations) are stored. *)
let albums =
  root ^ "Albums/"

(** System-wide backdrop. *)
let backdrop =
  "/home/fpottier/backdrop.jpg"

(** DVD image root directory. *)
let dvd_root =
  root ^ "Image/"

(** DVD image data directory. *)
let dvd_datafile =
  dvd_root ^ "photos-data"

(** DVD image repository directory. *)
let dvd_repository =
  dvd_root ^ "Originals/"

