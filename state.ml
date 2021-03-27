type t = {
  mutable people : Person.t array;
  mutable curr_deck : Deck.t;
  mutable card_pile : Deck.card;
}

exception NoMoreCards

(** Take in arrays of names *)
let init_state num name_array =
  let dummy_person = { Person.hand = []; name = ""; position = 0 } in
  let d = Deck.init () in
  let i_state =
    {
      people = Array.make num dummy_person;
      curr_deck = d;
      card_pile = { number = None; color = None; ctype = Normal };
    }
  in
  for i = 0 to num - 1 do
    i_state.people.(i) <-
      Person.init i_state.curr_deck name_array.(i) (i + 1);
    i_state.curr_deck <-
      (match !d with [] -> raise NoMoreCards | h :: t -> ref t)
  done;
  i_state

let draw_st st pos d =
  match !d with
  | [] -> raise NoMoreCards
  | h :: t ->
      let old = st.people.(pos).hand in
      st.people.(pos).hand <- h :: old;
      st.curr_deck <- ref t;
      st

let place_st st pos =
  match st.people.(pos).hand with
  | [] -> raise NoMoreCards
  | h :: t ->
      st.people.(pos).hand <- t;
      st.card_pile <- h;
      st

let get_people s = s.people

let get_curr_deck s = s.curr_deck

let get_card_pile s = s.card_pile
