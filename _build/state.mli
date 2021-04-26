(**current state of the game.*)
type t

(** [init_state num_players player_names] create and initial state for a
    game with [num_players] players. *)
val init_state : int -> string array -> t

(** get the array of players for the game. *)
val get_people : t -> Person.t array

(** get the current deck of the state. *)
val get_curr_deck : t -> Deck.t

(**get the top card of the current pile. *)
val get_card_pile : t -> Deck.card

(**[get_pos state] gets the position of the next desired player of
   state.*)
val get_pos : t -> int

(** [draw_st state player_number deck] draw a card from [deck] and put
    it into the hand of the player at the specified position
    [player_number]*)
val draw_st : t -> int -> Deck.card list ref -> int -> t

(** [place_st state player_number card_index] remove the card at
    [card_index] from the players hand and place it on the card pile.*)
val place_st : t -> int -> int -> t

(**[sort_st state player_number] sort the hand of the player.*)
val sort_st : t -> int -> t
