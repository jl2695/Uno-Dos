open Deck

let is_valid_dos card p1 =
  (p1.number != None && p1.number = card.number)
  || (p1.color != None && p1.color = card.color)
  || (p1.number = None && p1.color = None)
  || card.color = None
