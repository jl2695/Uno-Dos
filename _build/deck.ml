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

type t = card list ref

exception NoMoreCards

(** Helper function taken from a0 that creates an infix operator that
    makes a list of all integers from i through j inclusive *)
let ( -- ) i j =
  let rec from i j l = if i > j then l else from i (j - 1) (j :: l) in
  from i j []

let ( --- ) i j = (i -- j) @ (i -- j)

(** Auxiliary function for normal_cards. *)
let rec normal_color_helper color acc = function
  | [] -> acc
  | h :: t ->
      let normal_card =
        { number = Some h; color = Some color; ctype = Normal }
      in
      normal_color_helper color (normal_card :: acc) t

(** [normal_cards acc] adds the normal numbered cards to [acc]. *)
let rec normal_cards acc = function
  | [] -> acc
  | h :: t -> normal_color_helper h [] (0 --- 9) @ normal_cards acc t

(** Auxiliary function to special_color. *)
let special_helper_color (tp : ctype) color deck =
  let special_card =
    { number = None; color = Some color; ctype = tp }
  in
  special_card :: deck

(** Auxiliary function to special_color. *)
let rec special_helper_type tp_list color acc =
  match tp_list with
  | [] -> acc
  | h :: t ->
      special_helper_type t color (special_helper_color h color acc)

(** [special_color tp_list acc c_list] adds the special cards of the
    types in [tp_list] and of the colors in [c_list] to [deck]. *)
let rec special_color tp_list acc = function
  | [] -> acc
  | h :: t ->
      special_color tp_list (special_helper_type tp_list h acc) t

(** An auxiliary function for special_cards *)
let special_helper deck tp =
  let special_card = { number = None; color = None; ctype = tp } in
  special_card :: deck

(** [special_cards acc tp_list] adds the special cards of the types in
    [tp_list] without color to [deck]. *)
let rec special_cards acc (tp_list : ctype list) =
  match tp_list with
  | [] -> acc
  | h :: t -> special_cards (special_helper acc h) t

(** [repeat f n t_list deck c_list] repeatedly applies the function [f]
    [n] times. It's type is made for the function special_color which
    adds the special cards that have colors to the deck. *)
let rec repeat f n t_list deck c_list =
  if n = 0 then deck
  else repeat f (n - 1) t_list (f t_list deck c_list) c_list

(** [repeat' f n deck t_list] is identical to repeat but the type is for
    the function special_cards which adds the special cards that do not
    have colors to the deck. *)
let rec repeat' f n deck t_list =
  if n = 0 then deck else repeat' f (n - 1) (f deck t_list) t_list

(** [add_special] adds the special cards to [deck] - both with and
    without color. *)
let add_special deck =
  repeat' special_cards 4
    (repeat special_color 2
       [ Skip; Reverse; DrawTwo ]
       deck
       [ Red; Yellow; Blue; Green ])
    [ DrawFour; Wild ]

(** [compare_cards p1 p2] compares the randomly generated integer values
    of two cards and is used to shuffle the cards. *)
let compare_cards p1 p2 =
  if snd p1 = snd p2 then 0 else if snd p1 > snd p2 then 1 else -1

(** [init ()] initialises the deck with 80 normal cards numbered from 0
    to 9 with 2 of each color, 8 skip cards with 2 of each color, 8
    reverse cards with 2 of each color, 8 draw two cards with 2 of each
    color, 4 wild cards and 4 draw four cards.*)
let init () =
  let deck =
    add_special (normal_cards [] [ Red; Yellow; Blue; Green ])
  in
  Random.self_init ();
  deck
  |> List.map (fun x -> (x, Random.int 69))
  |> List.sort compare_cards
  |> List.map (fun (x, y) -> x)
  |> ref
