type t = {
  mutable people : Person.t array;
  mutable curr_deck : Deck.t;
}

exception NoMoreCards

(** Take in arrays of names *)
let init_state num name_array =
  let dummy_person = { Person.hand = []; name = ""; position = 0 } in
  let d = Deck.init () in
  let i_state =
    { people = Array.make num dummy_person; curr_deck = d }
  in
  for i = 0 to num - 1 do
    i_state.people.(i) <-
      Person.init i_state.curr_deck name_array.(i) (i + 1);
    i_state.curr_deck <-
      (match !d with [] -> raise NoMoreCards | h :: t -> ref t)
  done;
  i_state

let get_people s = s.people

let get_curr_deck s = s.curr_deck
