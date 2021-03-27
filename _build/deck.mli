(** Representation of deck. This module represents the cards stored in
    the game deck, including the order, type, color, and number of the
    cards. *)

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

exception NoMoreCards

(** The type [card] represents all cards and stores information on the
    type, number, and color of the card itself. *)
type card = {
  number : int option;
  color : color option;
  ctype : ctype;
}

(** The abstract type of values representing decks *)
type t = card list

(** [init] is the initial state of the deck. The card order is
    randomized everytime the game resets. *)
val init : unit -> t
