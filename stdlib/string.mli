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

(** String operations.

FIXME (bring this into line with bytes.mli)

  Given a string [s] of length [l], we call character number in [s]
  the index of a character in [s].  Indexes start at [0], and we will
  call a character number valid in [s] if it falls within the range
  [[0...l-1]]. A position is the point between two characters or at
  the beginning or end of the string.  We call a position valid
  in [s] if it falls within the range [[0...l]]. Note that character
  number [n] is between positions [n] and [n+1].

  Two parameters [start] and [len] are said to designate a valid
  substring of [s] if [len >= 0] and [start] and [start+len] are
  valid positions in [s].

  OCaml strings can be modified in place, for instance via the
  {!String.set} and {!String.blit} functions described below.  This
  possibility should be used rarely and with much care, however, since
  both the OCaml compiler and most OCaml libraries share strings as if
  they were immutable, rather than copying them.  In particular,
  string literals are shared: a single copy of the string is created
  at program loading time and returned by all evaluations of the
  string literal.  Consider for example:

  {[
      # let f () = "foo";;
      val f : unit -> string = <fun>
      # (f ()).[0] <- 'b';;
      - : unit = ()
      # f ();;
      - : string = "boo"
  ]}

  Likewise, many functions from the standard library can return string
  literals or one of their string arguments.  Therefore, the returned strings
  must not be modified directly.  If mutation is absolutely necessary,
  it should be performed on a fresh copy of the string, as produced by
  {!String.copy}.

 *)

external length : string -> int = "%string_length"
(** Return the length (number of characters) of the given string. *)

external get : string -> int -> char = "%string_safe_get"
(** [String.get s n] returns character number [n] in string [s].

   Raise [Invalid_argument] if [n] not a valid character number in [s]. *)


external set : bytes -> int -> char -> unit = "%string_safe_set"
  [@@ocaml.deprecated]
(** [String.set s n c] modifies byte sequence [s] in place,
   replacing the byte at index [n] with [c].

   Raise [Invalid_argument] if [n] is not a valid index in [s].

   @deprecated This is a deprecated alias of {!Bytes.set}.
*)

external create : int -> bytes = "caml_create_string" [@@ocaml.deprecated]
(** [String.create n] returns a fresh byte sequence of length [n].
   The sequence is uninitialized and contains arbitrary bytes.

   Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}.

   @deprecated This is a deprecated alias of {!Bytes.create}.
*)

val make : int -> char -> string
(** [String.make n c] returns a fresh string of length [n],
   filled with the character [c].

   Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}. *)

val copy : string -> string
(** Return a copy of the given string. *)

val sub : string -> int -> int -> string
(** [String.sub s start len] returns a fresh string of length [len],
   containing the substring of [s] that starts at position [start] and
   has length [len].

   Raise [Invalid_argument] if [start] and [len] do not
   designate a valid range of [s]. *)

val fill : bytes -> int -> int -> char -> unit [@@ocaml.deprecated]
(** [String.fill s start len c] modifies byte sequence [s] in place,
   replacing [len] bytes by [c], starting at [start].

   Raise [Invalid_argument] if [start] and [len] do not
   designate a valid range of [s].

   @deprecated This is a deprecated alias of {!Bytes.fill}. *)

val blit : string -> int -> bytes -> int -> int -> unit
(** [String.blit src srcoff dst dstoff len] copies [len] bytes
   from the string [src], starting at index [srcoff],
   to byte sequence [dst], starting at character number [dstoff].

   Raise [Invalid_argument] if [srcoff] and [len] do not
   designate a valid range of [src], or if [dstoff] and [len]
   do not designate a valid range of [dst]. *)

val concat : string -> string list -> string
(** [String.concat sep sl] concatenates the list of strings [sl],
   inserting the separator string [sep] between each. *)

val iter : (char -> unit) -> string -> unit
(** [String.iter f s] applies function [f] in turn to all
   the characters of [s].  It is equivalent to
   [f s.[0]; f s.[1]; ...; f s.[String.length s - 1]; ()]. *)

val iteri : (int -> char -> unit) -> string -> unit
(** Same as {!String.iter}, but the
   function is applied to the index of the element as first argument
   (counting from 0), and the character itself as second argument.
   @since 4.00.0 *)

val map : (char -> char) -> string -> string
(** [String.map f s] applies function [f] in turn to all
   the characters of [s] and stores the results in a new string that
   is returned.
   @since 4.00.0 *)

val trim : string -> string
(** Return a copy of the argument, without leading and trailing
   whitespace.  The characters regarded as whitespace are: [' '],
   ['\012'], ['\n'], ['\r'], and ['\t'].  If there is no leading nor
   trailing whitespace character in the argument, return the original
   string itself, not a copy.
   @since 4.00.0 *)

val escaped : string -> string
(** Return a copy of the argument, with special characters
   represented by escape sequences, following the lexical
   conventions of OCaml.  If there is no special
   character in the argument, return the original string itself,
   not a copy. Its inverse function is Scanf.unescaped. *)

val index : string -> char -> int
(** [String.index s c] returns the character number of the first
   occurrence of character [c] in string [s].

   Raise [Not_found] if [c] does not occur in [s]. *)

val rindex : string -> char -> int
(** [String.rindex s c] returns the character number of the last
   occurrence of character [c] in string [s].

   Raise [Not_found] if [c] does not occur in [s]. *)

val index_from : string -> int -> char -> int
(** [String.index_from s i c] returns the character number of the
   first occurrence of character [c] in string [s] after position [i].
   [String.index s c] is equivalent to [String.index_from s 0 c].

   Raise [Invalid_argument] if [i] is not a valid position in [s].
   Raise [Not_found] if [c] does not occur in [s] after position [i]. *)

val rindex_from : string -> int -> char -> int
(** [String.rindex_from s i c] returns the character number of the
   last occurrence of character [c] in string [s] before position [i+1].
   [String.rindex s c] is equivalent to
   [String.rindex_from s (String.length s - 1) c].

   Raise [Invalid_argument] if [i+1] is not a valid position in [s].
   Raise [Not_found] if [c] does not occur in [s] before position [i+1]. *)

val contains : string -> char -> bool
(** [String.contains s c] tests if character [c]
   appears in the string [s]. *)

val contains_from : string -> int -> char -> bool
(** [String.contains_from s start c] tests if character [c]
   appears in [s] after position [start].
   [String.contains s c] is equivalent to
   [String.contains_from s 0 c].

   Raise [Invalid_argument] if [start] is not a valid position in [s]. *)

val rcontains_from : string -> int -> char -> bool
(** [String.rcontains_from s stop c] tests if character [c]
   appears in [s] before position [stop+1].

   Raise [Invalid_argument] if [stop < 0] or [stop+1] is not a valid
   position in [s]. *)

val uppercase : string -> string
(** Return a copy of the argument, with all lowercase letters
   translated to uppercase, including accented letters of the ISO
   Latin-1 (8859-1) character set. *)

val lowercase : string -> string
(** Return a copy of the argument, with all uppercase letters
   translated to lowercase, including accented letters of the ISO
   Latin-1 (8859-1) character set. *)

val capitalize : string -> string
(** Return a copy of the argument, with the first character set to uppercase. *)

val uncapitalize : string -> string
(** Return a copy of the argument, with the first character set to lowercase. *)

type t = string
(** An alias for the type of strings. *)

val compare: t -> t -> int
(** The comparison function for strings, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [String] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

(**/**)

(* The following is for system use only. Do not call directly. *)

external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : bytes -> int -> char -> unit = "%string_unsafe_set"
  [@@ocaml.deprecated]
external unsafe_blit :
  string -> int -> bytes -> int -> int -> unit
  = "caml_blit_string" "noalloc"
external unsafe_fill :
  bytes -> int -> int -> char -> unit = "caml_fill_string" "noalloc"
  [@@ocaml.deprecated]
