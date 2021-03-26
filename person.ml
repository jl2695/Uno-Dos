type t = {
  mutable hand: Deck.card list;
  name: string;
  position: int
}

let draw (person: t) (d: Deck.t) = print_string "yomama"