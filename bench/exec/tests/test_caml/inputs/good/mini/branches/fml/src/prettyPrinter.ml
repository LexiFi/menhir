(* $Id$ *)

(** This module implements some common stuff for pretty printer. *)
open Format 

type output = 
  | Channel of Pervasives.out_channel
  | Buffer of Buffer.t 

type formatter_output = 
	{
	  out: string -> int -> int -> unit;
	  flush: unit -> unit;
	  newline: unit -> unit;
	  spaces: int -> unit;
	  with_tags: bool;
	  open_tag : tag -> unit;
	  close_tag : tag -> unit;
	  margin: int;
	}

type mode = 
      Latex of output
    | Txt of output
    | Formatter of formatter_output

let output_string output = 
  match output with
    | Channel cout -> Pervasives.output_string cout
    | Buffer b -> Buffer.add_string b

let flush output = 
  match output with
    | Channel cout -> Pervasives.flush cout
    | Buffer b -> ()

