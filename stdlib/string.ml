(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Damien Doligez, projet Gallium, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* String operations, based on bytearray operations *)

external length : string -> int = "%string_length"
external get : string -> int -> char = "%string_safe_get"
external set : bytearray -> int -> char -> unit = "%string_safe_set"
external create : int -> bytearray = "caml_create_string"
external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : bytearray -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit : string -> int ->  bytearray -> int -> int -> unit
                     = "caml_blit_string" "noalloc"
external unsafe_fill : bytearray -> int -> int -> char -> unit
                     = "caml_fill_string" "noalloc"

module BA = Bytearray

let make = (Obj.magic BA.make : int -> char -> string)
let copy = (Obj.magic BA.copy : string -> string)
let sub = (Obj.magic BA.sub : string -> int -> int -> string)
let fill = BA.fill
let blit =
  (Obj.magic BA.blit : string -> int -> bytearray -> int -> int -> unit)
let concat = (Obj.magic BA.concat : string -> string list -> string)
let iter = (Obj.magic BA.iter : (char -> unit) -> string -> unit)
let iteri = (Obj.magic BA.iteri : (int -> char -> unit) -> string -> unit)
let map = (Obj.magic BA.map : (char -> char) -> string -> string)

(* Beware: we cannot use BA.trim or BA.escape because they always make a copy,
   but our spec says we don't copy in some cases *)

external is_printable: char -> bool = "caml_is_printable"

let is_space = function
  | ' ' | '\012' | '\n' | '\r' | '\t' -> true
  | _ -> false

let trim s =
  if s = "" then s
  else if is_space (unsafe_get s 0) || is_space (unsafe_get s (length s - 1))
    then BA.unsafe_to_string (BA.trim (BA.unsafe_of_string s))
  else s

let escaped s =
  let rec needs_escape i =
    if i >= length s then false else
      match unsafe_get s i with
      | '"' | '\\' | '\n' | '\t' | '\r' | '\b' -> true
      | c when is_printable c -> needs_escape (i+1)
      | _ -> true
  in
  if needs_escape 0 then
    BA.unsafe_to_string (BA.escaped (BA.unsafe_of_string s))
  else
    s

let index = (Obj.magic BA.index : string -> char -> int)
let rindex = (Obj.magic BA.rindex : string -> char -> int)
let index_from = (Obj.magic BA.index_from : string -> int -> char -> int)
let rindex_from = (Obj.magic BA.rindex_from : string -> int -> char -> int)
let contains = (Obj.magic BA.contains : string -> char -> bool)
let contains_from = (Obj.magic BA.contains_from : string -> int -> char -> bool)
let rcontains_from =
  (Obj.magic BA.rcontains_from : string -> int -> char -> bool)
let uppercase = (Obj.magic BA.uppercase : string -> string)
let lowercase = (Obj.magic BA.lowercase : string -> string)
let capitalize = (Obj.magic BA.capitalize : string -> string)
let uncapitalize = (Obj.magic BA.uncapitalize : string -> string)

type t = string

let compare (x: t) (y: t) = Pervasives.compare x y
