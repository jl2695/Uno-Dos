(** Module that contains the rules for a valid card to place for DOS. *)

(** [is_valid_dos card pile] checks whether [card] is a valid card to
    place onto [pile] given DOS rules. *)
val is_valid_dos : Deck.card -> Deck.card -> bool
