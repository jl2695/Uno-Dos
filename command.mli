type command =
  | Place of string
  | Draw
  | Name of string
  | Begin

exception Empty

exception Malformed

val parse : string -> command
