open Command
open Deck
open Person

type t = {
  mutable people : Person.t array;
  mutable curr_deck : Deck.t;
  mutable card_pile : Deck.card;
  mutable pos : int;
}

exception NoMoreCards

(** Take in arrays of names *)
let init_state p_num p_name_array ai_num ai_name_array =
  let dummy_person =
    {
      Person.hand = [];
      name = "";
      position = 0;
      ai = false;
      difficulty = None;
    }
  in
  let d = Deck.init () in
  let i_state =
    {
      people = Array.make (p_num + ai_num) dummy_person;
      curr_deck = d;
      card_pile = { number = None; color = None; ctype = Normal };
      pos = 0;
    }
  in
  for i = 0 to p_num - 1 do
    i_state.people.(i) <-
      Person.init i_state.curr_deck p_name_array.(i) (i + 1) false;
    i_state.curr_deck <-
      ( match !d with
      | [] -> raise NoMoreCards
      | h :: t ->
          d := t;
          d )
  done;
  for j = p_num to p_num + ai_num - 1 do
    i_state.people.(j) <-
      Person.init i_state.curr_deck
        ai_name_array.(j - p_num)
        (p_num + j) true;
    i_state.curr_deck <-
      ( match !d with
      | [] -> raise NoMoreCards
      | h :: t ->
          d := t;
          d )
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
        draw_st st pos st.curr_deck (n - 1) )
  else st

(** [remove_ele n res] removes the nth element from res.*)
let rec remove_ele n res = function
  | [] -> res
  | h :: t ->
      if n <> 0 then remove_ele (n - 1) (h :: res) t
      else remove_ele (n - 1) res t

let red_d4 = { number = None; color = Some Red; ctype = DrawFour }

let green_d4 = { number = None; color = Some Green; ctype = DrawFour }

let yellow_d4 = { number = None; color = Some Yellow; ctype = DrawFour }

let blue_d4 = { number = None; color = Some Blue; ctype = DrawFour }

let red_wild = { number = None; color = Some Red; ctype = Wild }

let green_wild = { number = None; color = Some Green; ctype = Wild }

let yellow_wild = { number = None; color = Some Yellow; ctype = Wild }

let blue_wild = { number = None; color = Some Blue; ctype = Wild }

let prompt_color () =
  print_endline
    "Choose the next color of play (red, yellow, blue, or green)."

let rec enter_color st wild =
  print_string "> ";
  match parse_colors (read_line ()) with
  | Red ->
      if wild then st.card_pile <- red_wild else st.card_pile <- red_d4;
      st
  | Blue ->
      if wild then st.card_pile <- blue_wild
      else st.card_pile <- blue_d4;
      st
  | Yellow ->
      if wild then st.card_pile <- yellow_wild
      else st.card_pile <- yellow_d4;
      st
  | Green ->
      if wild then st.card_pile <- green_wild
      else st.card_pile <- green_d4;
      st
  | exception Empty ->
      print_endline "Please enter a color!";
      enter_color st wild
  | exception Malformed ->
      print_endline "Please enter a color!";
      enter_color st wild

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
          prompt_color ();
          enter_color new_st false
      | Reverse ->
          if pos - 1 < 0 then st.pos <- num_players - 1
          else st.pos <- pos - 1;
          st.people.(pos).hand <- new_hand;
          st.card_pile <- card;
          st
      | Wild ->
          st.people.(pos).hand <- new_hand;
          st.pos <- next_pos;
          prompt_color ();
          enter_color st true )

let sort_st st pos =
  Person.sort_hand st.people.(pos);
  st

let get_people s = s.people

let get_pos s = s.pos

let get_curr_deck s = s.curr_deck

let get_card_pile s = s.card_pile
