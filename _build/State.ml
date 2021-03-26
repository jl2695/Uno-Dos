type t = {
  num_players : int;
  curr_deck : Deck.t;
}

let init_state num = { num_players = num; curr_deck = Deck.init () }
