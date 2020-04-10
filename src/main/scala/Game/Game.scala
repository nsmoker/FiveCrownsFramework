package Game

import Game.Suit.Suit

import scala.collection.mutable.Stack
import scala.util.Random

class Game (val numPlayers: Int)
{
  //num players (immutable)
  //draw deck (mutable, stack, starts at 116, deal 3 to each player)
  //discard deck (mutable, stack, starts at 0, push discards on as they occur)
  //list of players (immutable)

  private var drawDeck: Stack[Card] = Stack.empty[Card].pushAll(Random.shuffle(Card.allCards))
  private var discardDeck: Stack[Card] = Stack.empty
  private var players: List[Player] = List.fill(numPlayers) (new Player("Joe"))
  private var hasMatch: Boolean = false

  def deal(numCards: Int) =
    {
      for(i <- 0 until numCards)
      {
        players.foreach(p => p.addToHand(drawDeck.pop))
      }
    }

  def playRound(round: Int) =
    {
      players.foreach(_.emptyHand)
      deal(round)
      while(!hasMatch)
      {
        players.foreach(_.takeTurn)
      }
      //deal
      //go by player, draw and discard

    }

  def checkMatch(p: Player, indexes: List[Int]): Boolean =
    {
      val cards = indexes.map(p.hand(_)).sortBy(_.value)
      //same suit AND run
      //same value suit doesnt matter
      val checkValue: Int = cards.head.value
      if(cards.forall(c => {c.value == checkValue})) true
      else
        {
          val checkSuit: Suit = cards.head.suit
          if(cards.forall(c => {c.suit == checkSuit}))
            {
              for(c <- 0 until cards.length-1)
                {
                  if(cards(c).value - cards(c+1).value != 1) false
                }
              true
            }
          else false
        }
    }
  def checkHand(p: Player, matches: List[List[Int]]): Boolean = matches.forall(checkMatch(p,_))
}
