(* $Header: /net/pauillac/caml/repository/bigbro/skimmer.ml,v 1.2 2001/03/09 14:49:30 fpottier Exp $ *)

open String_utils

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The internal state of the automaton is stored in a structure. The structure is made mutable to ease the load on the
garbage collector.

*)

module StringMap = Map.Make (struct
  type t = string
  let compare = Pervasives.compare
end)

type state =						(* The finite automaton's states. *)
    StateIdle
  | StateInTag
  | StateAwaitingAttr
  | StateInAttrName
  | StateAwaitingEqual
  | StateAwaitingValue
  | StateInValue
  | StateInComment
  | StateOutComment1
  | StateOutComment2

type attribute_value = (int * int * string) option      (* The attribute's value (if any) and its position. *)

type data = {
  text: string;					        (* The whole piece of text. *)
  length: int;					        (* Its length. *)
  mutable index: int;					(* The automaton's current position within the text. *)
  mutable line: int;
  mutable column: int;

  mutable state: state;				        (* The automaton's current state. *)

  mutable tag_start: int;				(* The start of the current tag's name, if within one. *)
  mutable tag_end: int;				        (* The end of the current tag's name, if determined. *)
  mutable tag_attr: attribute_value StringMap.t;	(* The list of tag attributes found so far. *)

  mutable attr_start: int;				(* The start of the current attribute's name, if within one. *)
  mutable attr_end: int;				(* The end of the current attribute's name, if determined. *)
  mutable attr_has_value: bool;			        (* Whether a value was found for the current attribute. *)
  mutable attr_value_line: int;	        		(* The start of the value text, if determined. *)
  mutable attr_value_column: int;
  mutable attr_value_start: int;                        (* The value's extent, if determined. *)
  mutable attr_value_end: int;

  mutable quote: char				        (* The quote character used to open the value, if any. *)
}

type tag_info = {                                       (* The structure passed to clients. *)
  ti_text: string;                                      (* The document's text. *)
  ti_index: int;                                        (* The position of this tag's closing angle bracket. *)
  ti_tag_start: int;                                    (* The start and end positions of the tag's name. *)
  ti_tag_end: int;
  ti_tag_attr: attribute_value StringMap.t              (* The list of the tag's attributes. *)
}

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Skim parses a piece of HTML text and calls the user-supplied function whenever a tag is found.

*)

let skim callback text = 

  (* Create the initial state. Except the first four, these values are placeholders. *)

  let data = {
    text = text;
    length = String.length text;
    index = 0;
    line = 1;
    column = 1;
    state = StateIdle;

    tag_start = 0;
    tag_end = 0;
    tag_attr = StringMap.empty;
    attr_start = 0;
    attr_end = 0;
    attr_has_value = false;
    attr_value_line = 0;
    attr_value_column = 0;
    attr_value_start = 0;
    attr_value_end = 0;
    quote = '"'
  } in

  (* When entering a new tag or a new attribute, some information needs to be initialized to avoid reading
     values left over from the previous tag/attribute. *)

  let clear_tag () =
    data.tag_attr <- StringMap.empty

  and clear_attribute () =
    data.attr_has_value <- false

  (* When we have fully analyzed an attribute, we can add it, in a nice form, to the tag's list of attributes. *)

  and end_attribute () =
    data.tag_attr <- StringMap.add
      (String.uppercase (String.sub data.text data.attr_start (data.attr_end - data.attr_start)))
      (if data.attr_has_value then
	 Some (data.attr_value_line, data.attr_value_column,
	   String.sub data.text data.attr_value_start (data.attr_value_end - data.attr_value_start))
       else
	 None)
      data.tag_attr

  (* When we have fully analyzed a tag, we pass it to found_tag which determines what to do with it. *)

  and end_tag () =
    callback {
      ti_text = data.text;
      ti_index = data.index;
      ti_tag_start = data.tag_start;
      ti_tag_end = data.tag_end;
      ti_tag_attr = data.tag_attr
    };
    data.state <- StateIdle in

  (* When we have fully analyzed an attribute's value, we mark its end, we end the attribute and we start
     awaiting the next one. *)

  let end_attribute_value () =
    data.attr_value_end <- data.index;
    end_attribute();
    data.state <- StateAwaitingAttr in

  (* The automaton's main loop. *)

  let rec loop () =

    (* Read a character. *)

    if data.index >= data.length then
      ()
    else begin
      let c = data.text.[data.index] in
      begin
	match data.state with

	  (* Idle state: we are out of any tag. *)

	  StateIdle -> (
	    match c with
	      '<' ->

		(* A tag is being opened; remember its start position. *)

		clear_tag();
		data.tag_start <- succ data.index;
		data.state <- StateInTag
	    | _ ->
		()
	  )

	  (* We are currently within a tag's name. *)

	| StateInTag -> (
	    match c with
	      '>' ->

		(* The whole tag ends here. *)

		data.tag_end <- data.index;
		end_tag()

	    | ' ' | '\t' | '\r' | '\n' ->

		(* The tag name is over. Remember its end position and start looking for attributes. *)

		data.tag_end <- data.index;
		data.state <- StateAwaitingAttr

	    | '-' ->

		(* If the tag name starts with !--, this is in fact a comment, not a tag. *)

		let start = data.tag_start in
		if (data.index = start + 2) & (text.[start] = '!') & (text.[start + 1] = '-') then
		  data.state <- StateInComment

	    | _ ->
		()
	  )

	  (* We are within the tag (we have read its name and possibly several attributes). 
	     We are awaiting a new attribute. *)

	| StateAwaitingAttr -> (
	    match c with
	      '>' ->
		end_tag()
	    | ' ' | '\t' | '\r' | '\n' ->
		()
	    | _ ->

		(* Here is a new attribute. Remember its start position and start scanning its name. *)

		clear_attribute();
		data.attr_start <- data.index;
		data.state <- StateInAttrName
	  )

	  (* We are currently scanning an attribute's name. *)

	| StateInAttrName -> (
	    match c with
	      '>' ->

		(* This was an attribute with no value. *)

		data.attr_end <- data.index;
		end_attribute();
		end_tag()

	    | ' ' | '\t' | '\r' | '\n' ->

		(* The attribute name is over. Remember its end position and await a possible equal sign. *)

		data.attr_end <- data.index;
		data.state <- StateAwaitingEqual

	    | '=' ->

		(* If the equal sign comes immediately, we start awaiting the value right now. *)

		data.attr_end <- data.index;
		data.state <- StateAwaitingValue

	    | _ ->
		()
	  )

	  (* We have read an attribute's name. We are awaiting either an equal sign (if the attribute
	     has a value), or a new attribute or the end of the tag (if it doesn't). *)

	| StateAwaitingEqual -> (
	    match c with
	      '=' ->
		data.state <- StateAwaitingValue
	    | ' ' | '\t' | '\r' | '\n' ->
		()
	    | '>' ->

		(* This was an attribute with no value. *)

		end_attribute();
		end_tag()

	    | _ ->

		(* If neither = nor > is found, then we assume that the last attribute didn't have
		   a value, and this is the name of a new attribute. *)

		end_attribute();

		clear_attribute();
		data.attr_start <- data.index;
		data.state <- StateInAttrName
	  )

	  (* We have read an equal sign, so we are awaiting the attribute's value.
	     The value is either any piece of text enclosed within quotes, or a single word (no
	     whitespace). When using quotes, we recognize either the single or the double quote, and we
	     remember which one was used as opening quote so we can look for a matching closing quote. *)

	| StateAwaitingValue -> (
	    match c with
	      '>' ->

		(* An equal sign followed by an end marker is illegal; let's handle it properly anyway. *)

		end_attribute();
		end_tag()

	    | ' ' | '\t' | '\r' | '\n' ->
		()
	    | _ ->

		(* Remember which character was used as opening quote. *)

		data.quote <- c;

		(* If this character is indeed a quote (single or double), then the actual value
		   starts at the next character, otherwise it starts right here. *)

		data.attr_has_value <- true;
		data.attr_value_start <- if (c = '"') or (c = '\'') then succ data.index else data.index;
		data.attr_value_line <- data.line;
		data.attr_value_column <- if (c = '"') or (c = '\'') then succ data.column else data.column;
		data.state <- StateInValue
	  )

	  (* We are reading the value and must detect its end. *)

	| StateInValue -> (
	    match c with
	      ' ' | '\t' | '\r' | '\n' ->

		(* White space is considered as the end of the value if there was no opening quote. *)

		if (data.quote <> '"') & (data.quote <> '\'') then
		  end_attribute_value()

	    | '"' | '\'' ->

		(* A quote is considered as the end of the value if the same quote was used to open it. *)

		if (c = data.quote) then
		  end_attribute_value()

	    | '>' ->

		(* End of the tag must mean end of the value. Add this attribute and revert to idle state. *)

		end_attribute_value();
		end_tag()

	    | _ ->
		()
	  )

	  (* We are inside a comment and must detect its end. The end of comments is defined by --> which
	     is recognized by a simple sub-automaton. *)

	| StateInComment ->
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

      data.column <- succ data.column;
      if c = '\n' then begin
	data.line <- succ data.line;
        data.column <- 1
      end;

      data.index <- succ data.index;
      loop()
    end

  in loop()
;;
