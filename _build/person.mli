(** Representation of player. It contains information on the hand,
    position and name of the player.*)

(** The type representing the player. It contains information about the
    player's hand, name and position. *)
type t = {
  mutable hand : Deck.card list;
  mutable name : string;
  mutable position : int;
  ai : bool;
  difficulty : string option;
}

(** [draw p d] will mutate the hand field of player [p] by adding the
    top card on the deck [d] to it. *)
val draw : t -> Deck.t -> unit

(** [init deck name player_number] initilize a player. This creates a
    new player with a given name and number and draws the top 7 cards
    from the deck.*)
val init : Deck.t -> string -> int -> bool -> t

(** [compare_cards card1 card2] comparison function for sorting cards.*)
val compare_cards : Deck.card -> Deck.card -> int

(** [sort_hand player] sort the hand of the [player] in place.*)
val sort_hand : t -> unit
