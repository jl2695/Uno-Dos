open Deck

let is_valid_card card pile =
  (pile.number != None && pile.number = card.number)
  || (pile.color != None && pile.color = card.color)
  || (pile.number = None && pile.color = None)
  || (card.number = None && card.color = None)
  || (pile.ctype = Reverse && card.ctype = Reverse)
  || (pile.ctype = Skip && card.ctype = Skip)
  || (pile.ctype = DrawTwo && card.ctype = DrawTwo)
  || (pile.number = Some 6 && card.number = Some 9)
