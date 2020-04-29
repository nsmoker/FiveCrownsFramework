package game

import game.Suit.Suit

case class Card(value:Int, suit:Suit)

object Card
{
  def allCards: List[Card] =
  {
    val init = List(Suit.Diamonds, Suit.Stars, Suit.Clubs, Suit.Spades, Suit.Hearts).flatMap { s =>
      for(i <- 3 to 13) yield
        {
        Card(i, s)
      }}
    val jokers: List[Card] = List(Card(50, null),Card(50, null),Card(50, null),Card(50, null),Card(50, null),Card(50, null))
    (init ++ init ++ jokers)
  }
}
