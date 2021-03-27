type t = {
  mutable hand : Deck.card list;
  name : string;
  mutable position : int;
}

exception NoMoreCards

let draw (person : t) (d : Deck.t) =
  match !d with
  | [] -> raise NoMoreCards
  | h :: t ->
      let old = person.hand in
      Deck.remove_card d;
      person.hand <- h :: old

let init (d : Deck.t) n =
  let person = { hand = []; name = n; position = 0 } in
  for i = 1 to 7 do
    draw person d
  done;
  person
