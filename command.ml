type command =
  | Place of string
  | Draw
  | Name of string
  | Begin

exception Empty

exception Malformed

let parse str =
  if String.length (String.trim str) = 0 then raise Empty
  else
    let words = String.split_on_char ' ' str in
    let spaces_removed =
      List.filter (fun x -> String.length x > 0) words
    in
    match spaces_removed with
    | [] -> raise Empty
    | [ h; t ] when h = "place" -> Place t
    | h :: t when h = "draw" -> Draw
    | [ h; t ] when h = "name" -> Name t
    | h :: t when h = "begin" -> Begin
    | h :: t -> raise Malformed
