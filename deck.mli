(** Representation of deck. This module represents the cards stored in
    the game deck, including the order, type, color, and number of the
    cards. *)

(**Represents the color of the uno card.*)
type color =
  | Red
  | Yellow
  | Blue
  | Green

(**Represents the action the uno card can do.*)
type ctype =
  | Normal
  | Skip
  | Reverse
  | DrawTwo
  | Wild
  | DrawFour
  | WildDos
  | WildNum

(**exception to be thrown when a user attempts to draw from an empty
   deck.*)
exception NoMoreCards

(** The type [card] represents all cards and stores information on the
    type, number, and color of the card itself. *)
type card = {
  number : int option;
  color : color option;
  ctype : ctype;
}

(** The abstract type of values representing decks *)
type t = card list ref

(** [init] is the initial state of the deck. The card order is
    randomized everytime the game resets. *)
val init : unit -> t

(** [init_dos] is the initial state of the deck while playing dos. The
    card order is randomized everytime the game resets. *)
val init_dos : unit -> t
