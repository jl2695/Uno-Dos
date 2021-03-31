type command =
  | Place of string
  | Draw
  | Name of string
  | Begin
  | Sort

exception Empty

exception Malformed

val parse : string -> command
