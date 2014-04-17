(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(** String operations. *)

external length : bytes -> int = "%string_length"
(** Return the length (number of characters) of the given string. *)

external get : bytes -> int -> char = "%string_safe_get"
(** [String.get s n] returns character number [n] in string [s].
   The first character is character number 0.
   The last character is character number [String.length s - 1].
   You can also write [s.[n]] instead of [String.get s n].

   Raise [Invalid_argument "index out of bounds"]
   if [n] is outside the range 0 to [(String.length s - 1)]. *)


external set : bytes -> int -> char -> unit = "%string_safe_set"
(** [String.set s n c] modifies string [s] in place,
   replacing the character number [n] by [c].
   You can also write [s.[n] <- c] instead of [String.set s n c].
   Raise [Invalid_argument "index out of bounds"]
   if [n] is outside the range 0 to [(String.length s - 1)]. *)

external create : int -> bytes = "caml_create_string"
(** [String.create n] returns a fresh string of length [n].
   The string initially contains arbitrary characters.
   Raise [Invalid_argument] if [n < 0] or [n > Sys.max_string_length].
*)

val make : int -> char -> bytes
(** [String.make n c] returns a fresh string of length [n],
   filled with the character [c].
   Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}.*)

val copy : bytes -> bytes
(** Return a copy of the given string. *)

val of_string : string -> bytes
(** Return a copy of the given string as a byte sequence. *)

val to_string : bytes -> string
(** Return a copy of the given bytes as a string. *)

val sub : bytes -> pos:int -> len:int -> bytes
(** [String.sub s start len] returns a fresh string of length [len],
   containing the characters number [start] to [start + len - 1]
   of string [s].
   Raise [Invalid_argument] if [start] and [len] do not
   designate a valid substring of [s]; that is, if [start < 0],
   or [len < 0], or [start + len > ]{!StringLabels.length}[ s]. *)

val fill : bytes -> pos:int -> len:int -> char -> unit
(** [String.fill s start len c] modifies string [s] in place,
   replacing the characters number [start] to [start + len - 1]
   by [c].
   Raise [Invalid_argument] if [start] and [len] do not
   designate a valid substring of [s]. *)

val blit :
  src:bytes -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int
  -> unit
(** [String.blit src srcoff dst dstoff len] copies [len] characters
   from string [src], starting at character number [srcoff], to
   string [dst], starting at character number [dstoff]. It works
   correctly even if [src] and [dst] are the same string,
   and the source and destination chunks overlap.
   Raise [Invalid_argument] if [srcoff] and [len] do not
   designate a valid substring of [src], or if [dstoff] and [len]
   do not designate a valid substring of [dst]. *)

val concat : sep:bytes -> bytes list -> bytes
(** [String.concat sep sl] concatenates the list of strings [sl],
   inserting the separator string [sep] between each. *)

val iter : f:(char -> unit) -> bytes -> unit
(** [String.iter f s] applies function [f] in turn to all
   the characters of [s].  It is equivalent to
   [f s.[0]; f s.[1]; ...; f s.[String.length s - 1]; ()]. *)

val iteri : f:(int -> char -> unit) -> bytes -> unit
(** Same as {!String.iter}, but the
   function is applied to the index of the element as first argument
   (counting from 0), and the character itself as second argument.
   @since 4.00.0
*)

val map : f:(char -> char) -> bytes -> bytes
(** [String.map f s] applies function [f] in turn to all
   the characters of [s] and stores the results in a new string that
   is returned.
   @since 4.00.0 *)

val trim : bytes -> bytes
(** Return a copy of the argument, without leading and trailing whitespace.
   The characters regarded as whitespace are: [' '], ['\012'], ['\n'],
   ['\r'], and ['\t'].  If there is no whitespace character in the argument,
   return the original string itself, not a copy.
   @since 4.00.0 *)

val escaped : bytes -> bytes
(** Return a copy of the argument, with special characters
   represented by escape sequences, following the lexical
   conventions of OCaml.  If there is no special
   character in the argument, return the original string itself,
   not a copy. *)

val index : bytes -> char -> int
(** [String.index s c] returns the position of the leftmost
   occurrence of character [c] in string [s].
   Raise [Not_found] if [c] does not occur in [s]. *)

val rindex : bytes -> char -> int
(** [String.rindex s c] returns the position of the rightmost
   occurrence of character [c] in string [s].
   Raise [Not_found] if [c] does not occur in [s]. *)

val index_from : bytes -> int -> char -> int
(** Same as {!StringLabels.index}, but start
   searching at the character position given as second argument.
   [String.index s c] is equivalent to [String.index_from s 0 c].*)

val rindex_from : bytes -> int -> char -> int
(** Same as {!StringLabels.rindex}, but start
   searching at the character position given as second argument.
   [String.rindex s c] is equivalent to
   [String.rindex_from s (String.length s - 1) c]. *)

val contains : bytes -> char -> bool
(** [String.contains s c] tests if character [c]
   appears in the string [s]. *)

val contains_from : bytes -> int -> char -> bool
(** [String.contains_from s start c] tests if character [c]
   appears in the substring of [s] starting from [start] to the end
   of [s].
   Raise [Invalid_argument] if [start] is not a valid index of [s]. *)

val rcontains_from : bytes -> int -> char -> bool
(** [String.rcontains_from s stop c] tests if character [c]
   appears in the substring of [s] starting from the beginning
   of [s] to index [stop].
   Raise [Invalid_argument] if [stop] is not a valid index of [s]. *)

val uppercase : bytes -> bytes
(** Return a copy of the argument, with all lowercase letters
   translated to uppercase, including accented letters of the ISO
   Latin-1 (8859-1) character set. *)

val lowercase : bytes -> bytes
(** Return a copy of the argument, with all uppercase letters
   translated to lowercase, including accented letters of the ISO
   Latin-1 (8859-1) character set. *)

val capitalize : bytes -> bytes
(** Return a copy of the argument, with the first character set to uppercase. *)

val uncapitalize : bytes -> bytes
(** Return a copy of the argument, with the first character set to lowercase. *)

type t = bytes
(** An alias for the type of strings. *)

val compare: t -> t -> int
(** The comparison function for strings, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [String] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

(**/**)

(* The following is for system use only. Do not call directly. *)

external unsafe_get : bytes -> int -> char = "%string_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit :
  src:bytes -> src_pos:int -> dst:bytes -> dst_pos:int -> len:int ->
    unit = "caml_blit_string" "noalloc"
external unsafe_fill :
  bytes -> pos:int -> len:int -> char -> unit = "caml_fill_string" "noalloc"
external unsafe_to_string : bytes -> string = "%identity"
external unsafe_of_string : string -> bytes = "%identity"
