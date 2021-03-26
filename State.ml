type t = {
  mutable people : Person.t array;
  mutable curr_deck : Deck.t;
}

let init_state num = 
  (* let curr_deck = Deck.init () in
  Person.init() curr_deck  *)
  let dummy_person = {
    hand = [];
    name = "";
    position = 0;
  } in
  let i_state = {
    people = Array.make num dummy_person;
    curr_deck = Deck.init ();
  }
  for i = 1 to num do
    i_state.people <- Person.init ();
    i_state.curr_deck <- 

  { num_players = num; curr_deck = Deck.init () }
