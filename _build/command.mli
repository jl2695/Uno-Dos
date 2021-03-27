type t =
  | Place of string
  | Draw

exception Empty

exception Malformed

val parse : string -> t
