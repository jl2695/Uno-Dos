type t = {
  mutable hand : Deck.card list;
  mutable name : string;
  mutable position : int;
  mutable score : int;
  ai : bool;
  difficulty : string option;
}

let draw (person : t) (d : Deck.t) =
  match !d with
  | [] -> raise Deck.NoMoreCards
  | h :: t ->
      let old = person.hand in
      person.hand <- h :: old

let init (d : Deck.t) n pos ai =
  if ai = false then (
    let player =
      {
        hand = [];
        name = n;
        position = pos;
        score = 0;
        ai = false;
        difficulty = None;
      }
    in
    for i = 1 to 7 do
      draw player d;
      match !d with [] -> raise Deck.NoMoreCards | h :: t -> d := t
    done;
    player)
  else
    let player =
      {
        hand = [];
        name = n;
        position = pos;
        score = 0;
        ai = true;
        difficulty = Some "easy";
      }
    in
    for i = 1 to 7 do
      draw player d;
      match !d with [] -> raise Deck.NoMoreCards | h :: t -> d := t
    done;
    player

let compare_cards (c1 : Deck.card) (c2 : Deck.card) =
  if c1.number = c2.number then 0
  else if c1.number > c2.number then 1
  else -1

let sort_hand person =
  person.hand <- List.sort compare_cards person.hand
