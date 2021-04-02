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
      st.people.(pos).hand <- old @ [ h ];
      st.curr_deck <- ref t;
      st

(** [remove_ele n res] removes the nth element from res*)
let rec remove_ele n res = function
  | [] -> res
  | h :: t ->
      if n <> 0 then remove_ele (n - 1) (h :: res) t
      else remove_ele (n - 1) res t

let place_st st pos card_index =
  match st.people.(pos).hand with
  | [] -> raise NoMoreCards
  | h :: t ->
      let card = List.nth st.people.(pos).hand card_index in
      let new_hand =
        List.rev (remove_ele card_index [] st.people.(pos).hand)
      in
      st.people.(pos).hand <- new_hand;
      st.card_pile <- card;
      st

let sort_st st pos =
  Person.sort_hand st.people.(pos);
  st

let get_people s = s.people

let get_curr_deck s = s.curr_deck

let get_card_pile s = s.card_pile
