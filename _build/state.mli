type t

val init_state : int -> t

val get_people : t -> Person.t array

val get_curr_deck : t -> Deck.t
