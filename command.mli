type t =
  | Place of int
  | Draw

exception Empty

exception Malformed

val parse : string -> t
