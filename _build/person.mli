(** Representation of player. It contains information on the 
    hand, position and name of the player.*)

(** *)
type t = {
  mutable hand: Deck.card list;
  name: string;
  position: int
}

(** [draw d p] mutates the hand field of the person record by taking the 
    top card of the deck [d] and adding it to the hand of the player [p]. *)
val draw : t -> Deck.t -> unit