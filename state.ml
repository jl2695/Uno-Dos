open Command
open Deck
open Person

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

(** Take in arrays of names *)
let draw d =
  match !d with
  | [] -> raise NoMoreCards
  | h :: t ->
      d := t;
      h

let init_state p_num p_name_array ai_num ai_name_array tot_rounds dos =
  let dummy_person =
    { Person.hand = []; name = ""; position = 0; score = 0; ai = false }
  in
  let d = if dos then Deck.init_dos () else Deck.init () in
  let i_state =
    if dos then
      {
        people = Array.make (p_num + ai_num) dummy_person;
        curr_deck = d;
        card_pile = draw d;
        dos_pile = draw d;
        pos = 0;
        game_ended = false;
        curr_round = 1;
        total_rounds = tot_rounds;
      }
    else
      {
        people = Array.make (p_num + ai_num) dummy_person;
        curr_deck = d;
        card_pile = draw d;
        dos_pile = { number = None; color = None; ctype = Normal };
        pos = 0;
        game_ended = false;
        curr_round = 1;
        total_rounds = tot_rounds;
      }
  in
  for i = 0 to p_num - 1 do
    i_state.people.(i) <-
      Person.init i_state.curr_deck p_name_array.(i) (i + 1) false;
    Person.sort_hand i_state.people.(i + 1);
    i_state.curr_deck <-
      (match !d with
      | [] -> raise NoMoreCards
      | h :: t ->
          d := t;
          d)
  done;
  for j = p_num to p_num + ai_num - 1 do
    i_state.people.(j) <-
      Person.init i_state.curr_deck
        ai_name_array.(j - p_num)
        (p_num + j) true;
    i_state.curr_deck <-
      (match !d with
      | [] -> raise Deck.NoMoreCards
      | h :: t ->
          d := t;
          d)
  done;
  i_state

let reinitialize_state st next_round winner_pos =
  let d = Deck.init () in
  st.curr_round <- next_round;
  st.people.(winner_pos).score <- st.people.(winner_pos).score + 1;
  st.curr_deck <- d;
  st.game_ended <- false;
  for i = 0 to Array.length st.people - 1 do
    let player = st.people.(i) in
    player.hand <- [];
    for i = 1 to 7 do
      Person.draw player d;
      st.curr_deck <-
        (match !d with
        | [] -> raise Deck.NoMoreCards
        | h :: t ->
            d := t;
            d)
    done
  done;
  st.card_pile <- { number = None; color = None; ctype = Normal };
  st

let rec draw_st st pos d n =
  if n > 0 then (
    match !d with
    | [] -> raise NoMoreCards
    | h :: t ->
        let old = st.people.(pos).hand in
        st.people.(pos).hand <- old @ [ h ];
        st.curr_deck <- ref t;
        Person.sort_hand st.people.(pos);
        draw_st st pos st.curr_deck (n - 1))
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

let size = ANSITerminal.size ()

let width = fst size

let height = snd size

let center_cursor str =
  let len = String.length str in
  let center = width / 2 in
  ANSITerminal.set_cursor (center - (len / 2)) height

let print_endline_centered str =
  center_cursor str;
  print_endline str

let prompt_color () =
  print_endline_centered
    "Choose the next color of play (red, yellow, blue, or green)."

let rec enter_color st wild =
  prompt_color ();
  ANSITerminal.set_cursor ((width / 2) - 8) height;
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
      print_endline_centered "Please enter a color!";
      enter_color st wild
  | exception Malformed ->
      print_endline_centered "Please enter a color!";
      enter_color st wild

let ai_color color st wild =
  match parse_colors color with
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

let rand_choose_color () =
  Random.self_init ();
  let num = Random.int 4 in
  match num with
  | 0 -> "green"
  | 1 -> "red"
  | 2 -> "blue"
  | 3 -> "yellow"
  | _ -> "green"

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
      if new_hand = [] then st.game_ended <- true;
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
          if new_st.people.(pos).ai = true then
            ai_color (rand_choose_color ()) new_st false
          else enter_color new_st false
      | Reverse ->
          if pos - 1 < 0 then st.pos <- num_players - 1
          else st.pos <- pos - 1;
          st.people.(pos).hand <- new_hand;
          st.card_pile <- card;
          st
      | Wild ->
          st.people.(pos).hand <- new_hand;
          st.pos <- next_pos;
          if st.people.(pos).ai = true then
            ai_color (rand_choose_color ()) st true
          else enter_color st true
      | WildDos ->
          st.people.(pos).hand <- new_hand;
          st.pos <- next_pos;
          st.card_pile <- card;
          st
      | WildNum ->
          st.people.(pos).hand <- new_hand;
          st.pos <- next_pos;
          st.card_pile <- card;
          st)

let get_people s = s.people

let get_person s i = s.people.(i)

let get_pos s = s.pos

let get_curr_deck s = s.curr_deck

let get_card_pile s = s.card_pile

let get_dos_pile s = s.dos_pile

let get_game_ended s = s.game_ended

let get_curr_round s = s.curr_round

let set_curr_round s i = s.curr_round <- i

let get_total_rounds s = s.total_rounds

let set_total_rounds s i = s.total_rounds <- i

(** [place_st_dos_single st pos card_index] places the card at
    [card_index] from the player at position [pos] and updates the state
    [st].*)
let place_st_dos_single st pos card_index mch_pile =
  match st.people.(pos).hand with
  | [] -> raise NoMoreCards
  | h :: t ->
      let deck = get_curr_deck st in
      let new_card = draw deck in
      let new_hand =
        List.rev (remove_ele card_index [] st.people.(pos).hand)
      in
      let num_players = Array.length st.people in
      let next_pos = (pos + 1) mod num_players in
      if new_hand = [] then st.game_ended <- true;
      st.people.(pos).hand <- new_hand;
      st.pos <- next_pos;
      if mch_pile = 1 then st.card_pile <- new_card
      else st.dos_pile <- new_card;
      st

(** [place_st_dos_double st pos card_index] places the card at
    [card_index] from the player at position [pos] and updates the state
    [st].*)
let place_st_dos_double st pos grp_idx grps mch_pile =
  match grps with
  | [] -> raise NoMoreCards
  | h :: t ->
      let deck = get_curr_deck st in
      let new_card = draw deck in
      let num_players = Array.length st.people in
      let next_pos = (pos + 1) mod num_players in
      let grp = List.nth grps grp_idx in
      let fst_idx = snd (List.hd grp) in
      let snd_idx = snd (List.hd (List.rev grp)) in
      (* Remove the first card from the player hand *)
      let removed_first =
        List.rev (remove_ele fst_idx [] st.people.(pos).hand)
      in
      (* Remove the second card from the player hand *)
      let new_hand = List.rev (remove_ele snd_idx [] removed_first) in
      assert (List.length new_hand <> 0);
      if new_hand = [] then st.game_ended <- true;
      st.people.(pos).hand <- new_hand;
      st.pos <- next_pos;
      if mch_pile = 1 then st.card_pile <- new_card
      else st.dos_pile <- new_card;
      st
