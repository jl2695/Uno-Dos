type t

val init_state : int -> string array -> t

val get_people : t -> Person.t array

val get_curr_deck : t -> Deck.t

val get_card_pile : t -> Deck.card

val draw_st : t -> int -> Deck.card list ref -> t

val place_st : t -> int -> int -> t

val sort_st : t -> int -> t
