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
  ctype : ctype;
}

type t = card list

exception NoMoreCards

(** Helper function taken from a0 that creates an infix operator that
    makes a list of all integers from i through j inclusive *)
let ( -- ) i j =
  let rec from i j l = if i > j then l else from i (j - 1) (j :: l) in
  from i j []

let ( --- ) i j = (i -- j) @ (i -- j) @ (i -- j) @ (i -- j)

let rec normal_color_helper color acc = function
  | [] -> acc
  | h :: t ->
      let normal_card =
        { number = Some h; color = Some color; ctype = Normal }
      in
      normal_color_helper color (normal_card :: acc) t

let rec normal_helper acc = function
  | [] -> acc
  | h :: t -> normal_color_helper h [] (0 --- 9) @ normal_helper acc t

let compare_cards p1 p2 =
  if snd p1 = snd p2 then 0 else if snd p1 > snd p2 then 1 else -1

(** Use random module, each card has a number associated with it*)
let init () =
  let deck = normal_helper [] [ Red; Yellow; Blue; Green ] in
  Random.self_init ();
  deck
  |> List.map (fun x -> (x, Random.int 69))
  |> List.sort compare_cards
  |> List.map (fun (x, y) -> x)
