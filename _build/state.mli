(**current state of the game.*)
type t = {
  mutable people : Person.t array;
  mutable curr_deck : Deck.t;
  mutable card_pile : Deck.card;
  mutable dos_pile : Deck.card;
  mutable pos : int;
  mutable game_ended : bool;
  mutable curr_round : int;
  mutable total_rounds : int;
}

(** [init_state num_players player_names ai_num ai_name_array
    tot_rounds]
    creates and initializes state for a game with [num_players] human
    players and [ai_num] AI players. The total_rounds field for the
    state it returns is set to [tot_rounds]. *)
val init_state :
  int -> string array -> int -> string array -> int -> bool -> t

val reinitialize_state : t -> int -> int -> t

(** get the array of players for the game. *)
val get_people : t -> Person.t array

(** get the person at index i. *)
val get_person : t -> int -> Person.t

(** get the current deck of the state. *)
val get_curr_deck : t -> Deck.t

(**get the top card of the current pile. *)
val get_card_pile : t -> Deck.card

(**get the top card of the second pile when playing dos. *)
val get_dos_pile : t -> Deck.card

(**[get_pos state] gets the position of the next desired player of
   state.*)
val get_pos : t -> int

(** [get_game_ended state] returns whether the game has ended (i.e. A
    player has no cards left. ) *)
val get_game_ended : t -> bool

(** [get_curr_round state] gets the current round number of the game. *)
val get_curr_round : t -> int

(** [set_curr_round state i] sets the curr_round field of [state] to
    [i]. *)
val set_curr_round : t -> int -> unit

(** [get_total_rounds state] gets the total number of rounds in [state]. *)
val get_total_rounds : t -> int

(** [set_total_rounds state i] sets the total_rounds field of [state] to
    [i]. *)
val set_total_rounds : t -> int -> unit

(** [draw_st state player_number deck] draw a card from [deck] and put
    it into the hand of the player at the specified position
    [player_number]*)
val draw_st : t -> int -> Deck.card list ref -> int -> t

(** [place_st state player_number card_index] remove the card at
    [card_index] from the players hand and place it on the card pile.*)
val place_st : t -> int -> int -> t

(** [place_st state player_number card_index] remove the card at
    [card_index] from the players hand and place it on the card pile.*)
val place_st_dos_single : t -> int -> int -> int -> t

(** [place_st state player_number card_index] remove the card at
    [card_index] from the players hand and place it on the card pile.*)
val place_st_dos_double :
  t -> int -> int -> ('a * int) list list -> int -> t
