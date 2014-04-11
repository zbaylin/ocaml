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

(** Bytearray operations.

  Given a bytearray [s] of length [l], we call character number in [s]
  the index of a character in [s].  Indexes start at [0], and we will
  call a character number valid in [s] if it falls within the range
  [[0...l-1]]. A position is the point between two characters or at
  the beginning or end of the string.  We call a position valid
  in [s] if it falls within the range [[0...l]]. Note that character
  number [n] is between positions [n] and [n+1].

  Two parameters [start] and [len] are said to designate a valid
  range of [s] if [len >= 0] and [start] and [start+len] are
  valid positions in [s].

  OCaml bytearrays can be modified in place, for instance via the
  {!Bytearray.set} and {!Bytearray.blit} functions described below.
  Strings, on the other hand, cannot be modified.
 *)

external length : bytearray -> int = "%string_length"
(** Return the length (number of characters) of the argument. *)

external get : bytearray -> int -> char = "%string_safe_get"
(** [get s n] returns character number [n] in argument [s].
   You can also write [s.[n]] instead of [Bytearray.get s n].

   Raise [Invalid_argument] if [n] not a valid character number in [s]. *)


external set : bytearray -> int -> char -> unit = "%string_safe_set"
(** [set s n c] modifies [s] in place, replacing the character
   number [n] by [c].
   You can also write [s.[n] <- c] instead of [Bytearray.set s n c].

   Raise [Invalid_argument] if [n] is not a valid character number in [s]. *)

external create : int -> bytearray = "caml_create_string"
(** [create n] returns a fresh bytearray of length [n].
   The bytearray initially contains arbitrary characters.

   Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}. *)

val make : int -> char -> bytearray
(** [make n c] returns a fresh bytearray of length [n],
   filled with the character [c].

   Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}.*)

val copy : bytearray -> bytearray
(** Return a copy of the given bytearray. *)

val from_string : string -> bytearray
(** Return a copy of the given string as a bytearray. *)

val to_string : bytearray -> string
(** Return a copy of the given bytearray as a string. *)

val sub : bytearray -> int -> int -> bytearray
(** [sub s start len] returns a fresh bytearray of
   length [len], containing the subarray of [s] that
   starts at position [start] and has length [len].

   Raise [Invalid_argument] if [start] and [len] do not
   designate a valid range of [s]. *)

val fill : bytearray -> int -> int -> char -> unit
(** [fill s start len c] modifies [s] in place, replacing [len]
   characters by [c], starting at [start].

   Raise [Invalid_argument] if [start] and [len] do not
   designate a valid range of [s]. *)

val blit : bytearray -> int -> bytearray -> int -> int -> unit
(** [blit src srcoff dst dstoff len] copies [len] characters
   from bytearray [src], starting at character number [srcoff], to
   bytearray [dst], starting at character number [dstoff]. It works
   correctly even if [src] and [dst] are the same bytearray,
   and the source and destination intervals overlap.

   Raise [Invalid_argument] if [srcoff] and [len] do not
   designate a valid range of [src], or if [dstoff] and [len]
   do not designate a valid range of [dst]. *)

val concat : bytearray -> bytearray list -> bytearray
(** [concat sep sl] concatenates the list of bytearrays [sl],
   inserting the separator bytearray [sep] between each. *)

val iter : (char -> unit) -> bytearray -> unit
(** [iter f s] applies function [f] in turn to all
   the characters of [s].  It is equivalent to
   [f s.[0]; f s.[1]; ...; f s.[String.length s - 1]; ()]. *)

val iteri : (int -> char -> unit) -> bytearray -> unit
(** Same as {!String.iter}, but the
   function is applied to the index of the element as first argument
   (counting from 0), and the character itself as second argument.
   @since 4.00.0
*)

val map : (char -> char) -> bytearray -> bytearray
(** [map f s] applies function [f] in turn to all
   the characters of [s] and stores the results in a new
   bytearray that is returned as the result.
   @since 4.00.0 *)

val trim : bytearray -> bytearray
(** Return a copy of the argument, without leading and trailing
   whitespace.  The characters regarded as whitespace are: [' '],
   ['\012'], ['\n'], ['\r'], and ['\t'].
   @since 4.00.0 *)

val escaped : bytearray -> bytearray
(** Return a copy of the argument, with special characters
   represented by escape sequences, following the lexical
   conventions of OCaml.  Its inverse function is {!Scanf.unescaped}. *)

val index : bytearray -> char -> int
(** [index s c] returns the character number of the first
   occurrence of character [c] in [s].

   Raise [Not_found] if [c] does not occur in [s]. *)

val rindex : bytearray -> char -> int
(** [rindex s c] returns the character number of the last
   occurrence of character [c] in [s].

   Raise [Not_found] if [c] does not occur in [s]. *)

val index_from : bytearray -> int -> char -> int
(** [index_from s i c] returns the character number of the
   first occurrence of character [c] in [s] after position [i].
   [String.index s c] is equivalent to [String.index_from s 0 c].

   Raise [Invalid_argument] if [i] is not a valid position in [s].
   Raise [Not_found] if [c] does not occur in [s] after position [i]. *)

val rindex_from : bytearray -> int -> char -> int
(** [rindex_from s i c] returns the character number of the
   last occurrence of character [c] in [s] before position [i+1].
   [rindex s c] is equivalent to
   [rindex_from s (String.length s - 1) c].

   Raise [Invalid_argument] if [i+1] is not a valid position in [s].
   Raise [Not_found] if [c] does not occur in [s] before position [i+1]. *)

val contains : bytearray -> char -> bool
(** [contains s c] tests if character [c]
   appears in [s]. *)

val contains_from : bytearray -> int -> char -> bool
(** [contains_from s start c] tests if character [c]
   appears in [s] after position [start].
   [contains s c] is equivalent to
   [contains_from s 0 c].

   Raise [Invalid_argument] if [start] is not a valid position in [s]. *)

val rcontains_from : bytearray -> int -> char -> bool
(** [rcontains_from s stop c] tests if character [c]
   appears in [s] before position [stop+1].

   Raise [Invalid_argument] if [stop < 0] or [stop+1] is not a valid
   position in [s]. *)

val uppercase : bytearray -> bytearray
(** Return a copy of the argument, with all lowercase letters
   translated to uppercase, including accented letters of the ISO
   Latin-1 (8859-1) character set. *)

val lowercase : bytearray -> bytearray
(** Return a copy of the argument, with all uppercase letters
   translated to lowercase, including accented letters of the ISO
   Latin-1 (8859-1) character set. *)

val capitalize : bytearray -> bytearray
(** Return a copy of the argument, with the first character set to uppercase. *)

val uncapitalize : bytearray -> bytearray
(** Return a copy of the argument, with the first character set to lowercase. *)

type t = bytearray
(** An alias for the type of bytearrays. *)

val compare: t -> t -> int
(** The comparison function for bytearrays, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [String] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

(**/**)

(* The following is for system use only. Do not call directly. *)

external unsafe_get : bytearray -> int -> char = "%string_unsafe_get"
external unsafe_set : bytearray -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit :
  bytearray -> int -> bytearray -> int -> int -> unit
  = "caml_blit_string" "noalloc"
external unsafe_fill :
  bytearray -> int -> int -> char -> unit = "caml_fill_string" "noalloc"
external unsafe_to_string : bytearray -> string = "%identity"
external unsafe_from_string : string -> bytearray = "%identity"
