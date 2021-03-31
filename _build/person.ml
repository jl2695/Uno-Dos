type t = {
  mutable hand : Deck.card list;
  mutable name : string;
  mutable position : int;
}

exception NoMoreCards

let draw (person : t) (d : Deck.t) =
  match !d with
  | [] -> raise NoMoreCards
  | h :: t ->
      let old = person.hand in
      person.hand <- h :: old

let init (d : Deck.t) n pos =
  let person = { hand = []; name = n; position = pos } in
  for i = 1 to 7 do
    draw person d;
    match !d with [] -> raise NoMoreCards | h :: t -> d := t
  done;
  person

let compare_cards (c1 : Deck.card) (c2 : Deck.card) =
  if c1.number = c2.number then 0
  else if c1.number > c2.number then 1
  else -1

let sort_hand person =
  person.hand <- List.sort compare_cards person.hand
