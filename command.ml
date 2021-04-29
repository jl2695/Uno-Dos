type command =
  | Place of string
  | Draw
  | Name of string
  | AI of int

type color_command =
  | Red
  | Yellow
  | Blue
  | Green

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
    | [ h ] when h = "draw" -> Draw
    | [ h; t ] when h = "name" -> Name t
    | [ h; t ] when h = "AI" -> AI (int_of_string t)
    | h :: t -> raise Malformed

let parse_colors color =
  if String.length (String.trim color) = 0 then raise Empty
  else
    let words = String.split_on_char ' ' color in
    let spaces_removed =
      List.filter (fun x -> String.length x > 0) words
    in
    match spaces_removed with
    | [] -> raise Empty
    | [ h ] when h = "red" -> Red
    | [ h ] when h = "blue" -> Blue
    | [ h ] when h = "yellow" -> Yellow
    | [ h ] when h = "green" -> Green
    | _ -> raise Malformed
