(** Module that contains the rules for a valid card to place for UNO. *)

(** [is_valid_card card pile] checks whether [card] is a valid card to
    place onto [pile] given UNO rules. *)
val is_valid_card : Deck.card -> Deck.card -> bool
