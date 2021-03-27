(** Representation of player. It contains information on the hand,
    position and name of the player.*)

(** The type representing the player. It contains information about the
    player's hand, name and position. *)
type t = {
  mutable hand : Deck.card list;
  name : string;
  mutable position : int;
}

exception NoMoreCards

(** [draw p d] will mutate the hand field of player [p] by adding the
    top card on the deck [d] to it. *)
val draw : t -> Deck.t -> unit

val init : Deck.t -> string -> t

(* val order_hand : t -> t *)
