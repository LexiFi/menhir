(* $Header: /net/pauillac/caml/repository/bigbro/stripper.ml,v 1.1.1.1 2001/02/13 15:39:35 fpottier Exp $ *)

open String_utils

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The internal state of the automaton is stored in a structure. The structure is made mutable to ease the load on the
garbage collector.

*)

type state =						(* The finite automaton's states. *)
    StateIdle
  | StateInTag
  | StateInComment
  | StateOutComment1
  | StateOutComment2

type data = {
    mutable src_index: int;				(* Position within the source text. *)
    mutable dst_index: int;				(* Position within the destination text (smaller). *)
    mutable state: state;				(* Current automaton's state. *)
    mutable tag_start: int				(* Beginning of the last tag's name. *)
  } 

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The automaton's code.

*)

let strip convert_ws src_text =
  
  (* We know that the destination string will be shorter, so we allocate it ahead of time for efficiency. *)

  let src_length = String.length src_text in
  let dst_text = String.create src_length in

  (* Initialize the data. *)

  let data = {
    src_index = 0;
    dst_index = 0;
    state = StateIdle;
    tag_start = 0
  } in

  (* The copying utility. *)

  let copy c =
    String.set dst_text data.dst_index c;
    data.dst_index <- succ data.dst_index in

  (* Loop. *)

  let rec loop () =
      
    if data.src_index >= src_length then
      ()
    else begin
      let c = src_text.[data.src_index] in
      begin
	match data.state with

	  StateIdle -> (
	    match c with
	      '<' ->

		(* If a tag is being opened, remember its start position (used to detect comments). *)

		data.tag_start <- succ data.src_index;
		data.state <- StateInTag

	    | ' ' | '\t' | '\r' | '\n' ->

		(* Whitespace is copied but might be converted to spaces. *)

		copy (if convert_ws then ' ' else c)

	    | _ ->

		(* Other characters are copied. *)

		copy c

	  )
	| StateInTag -> (
	    match c with
	      '>' ->
		data.state <- StateIdle
	    | '-' ->
		if (data.src_index = data.tag_start + 2) & (src_text.[data.tag_start] = '!')
		    & (src_text.[data.tag_start + 1] = '-') then
		  data.state <- StateInComment
	    | _ ->
		()
	  )
	|	StateInComment ->
	    if c = '-' then
	      data.state <- StateOutComment1
	| StateOutComment1 -> (
	    match c with
	      '-' ->
		data.state <- StateOutComment2
	    | _ ->
		data.state <- StateInComment
	  )
	| StateOutComment2 -> (
	    match c with
	      '>' ->
		data.state <- StateIdle
	    | '-' ->
		()
	    | _ ->
		data.state <- StateInComment
	  )
      end;
      data.src_index <- succ data.src_index;
      loop()
    end

  in loop();

  (* Shorten the destination string. *)

  let dst_text = String.sub dst_text 0 data.dst_index in

  (* Drop initial whitespace and determine whether there remains anything. *)

  let is_ws = function
      ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false in

  let _, dst_text = StringOp.filter is_ws dst_text in
  if dst_text = "" then None else Some dst_text
;;

