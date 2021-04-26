type t = {
  mutable people : Person.t array;
  mutable curr_deck : Deck.t;
  mutable card_pile : Deck.card;
  mutable pos : int;
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
      pos = 0;
    }
  in
  for i = 0 to num - 1 do
    i_state.people.(i) <-
      Person.init i_state.curr_deck name_array.(i) (i + 1);
    i_state.curr_deck <-
      (match !d with [] -> raise NoMoreCards | h :: t -> ref t)
  done;
  i_state

let rec draw_st st pos d n =
  if n > 0 then (
    match !d with
    | [] -> raise NoMoreCards
    | h :: t ->
        let old = st.people.(pos).hand in
        st.people.(pos).hand <- old @ [ h ];
        st.curr_deck <- ref t;
        draw_st st pos d (n - 1))
  else st

(** [remove_ele n res] removes the nth element from res*)
let rec remove_ele n res = function
  | [] -> res
  | h :: t ->
      if n <> 0 then remove_ele (n - 1) (h :: res) t
      else remove_ele (n - 1) res t

(** [place_st st pos card_index] places the card at [card_index] from
    the player at position [pos] and updates the state [st].*)
let place_st st pos card_index =
  match st.people.(pos).hand with
  | [] -> raise NoMoreCards
  | h :: t -> (
      let card = List.nth st.people.(pos).hand card_index in
      let new_hand =
        List.rev (remove_ele card_index [] st.people.(pos).hand)
      in
      let num_players = Array.length st.people in
      let next_pos = (pos + 1) mod num_players in
      match card.ctype with
      | Normal ->
          st.people.(pos).hand <- new_hand;
          st.pos <- next_pos;
          st.card_pile <- card;
          st
      | Skip ->
          st.people.(pos).hand <- new_hand;
          st.pos <- (pos + 2) mod num_players;
          st.card_pile <- card;
          st
      | DrawTwo ->
          let new_st = draw_st st next_pos st.curr_deck 2 in
          new_st.people.(pos).hand <- new_hand;
          new_st.pos <- next_pos;
          new_st.card_pile <- card;
          new_st
      | DrawFour ->
          let new_st = draw_st st next_pos st.curr_deck 4 in
          new_st.people.(pos).hand <- new_hand;
          new_st.pos <- next_pos;
          new_st.card_pile <- card;
          new_st
      | Reverse ->
          if pos - 1 < 0 then st.pos <- num_players - 1
          else st.pos <- pos - 1;
          st.people.(pos).hand <- new_hand;
          st.card_pile <- card;
          st
      | Wild ->
          st.people.(pos).hand <- new_hand;
          st.pos <- next_pos;
          st.card_pile <- card;
          st)

let sort_st st pos =
  Person.sort_hand st.people.(pos);
  st

let get_people s = s.people

let get_pos s = s.pos

let get_curr_deck s = s.curr_deck

let get_card_pile s = s.card_pile
