type color =
  | Red
  | Yellow
  | Blue
  | Green

type ctype =
  | Normal
  | Skip
  | Reverse
  | DrawTwo
  | Wild
  | DrawFour

type card = {
  number : int option;
  color : color option;
  ctype : ctype option;
}

type t = card list

(** Helper function taken from a0 that creates an infix operator that
    makes a list of all integers from i through j inclusive *)
let ( -- ) i j =
  let rec from i j l = if i > j then l else from i (j - 1) (j :: l) in
  from i j []

(* let rec normal_card_helper color acc = function | [] -> acc | h :: t
   -> let normal_card = { number: Some 5; color: Some color; ctype:
   Normal } in normal_card_helper color (normal_card::acc) t *)

(** Use random module, each card has a number associated with it*)
let init () = []
