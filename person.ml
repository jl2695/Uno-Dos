type t = {
  mutable hand : Deck.card list;
  name : string;
  position : int;
}

exception NoMoreCards

let draw (person : t) (d : Deck.t) =
  match d with
  | [] -> raise NoMoreCards
  | h :: t ->
      let old = person.hand in
      person.hand = h :: old
