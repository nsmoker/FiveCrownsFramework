package game

import game.Suit.Suit

import scala.collection.mutable.{ArrayBuffer, Stack}
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
  private var firstMatch: Player = _

  def test(): Unit = {
    players = List(new Player("jim"))
    players.head.addToHand(Card(10, Suit.Clubs))
    players.head.addToHand(Card(10, Suit.Clubs))
    players.head.addToHand(Card(10, Suit.Clubs))
  }

  def deal(numCards: Int): Unit =
    {
      for(i <- 0 until numCards)
      {
        players.foreach(p => p.addToHand(drawDeck.pop))
      }
    }

  def draw(drawFrom: String, p: Player): Unit = drawFrom match {
    case "discard" => p.addToHand(discardDeck.pop())
    case "deck" => p.addToHand(drawDeck.pop)
    case _ => println("Error: How did this even happen?")
  }

  def discard(p: Player, i: Int): Unit = discardDeck.push(p.removeFromHand(i))

  def playGame(): Unit = {
    for(roundNum <- 3 to 13) {
      playRound(roundNum)
    }
  }

  def playRound(round: Int): Unit =
    {
      players.foreach(_.emptyHand)
      deal(round)
      while(!hasMatch)
      {
        players.foreach(_.takeTurn(this))
      }
      val remaining = players.filterNot(_ != firstMatch)
      remaining.foreach(_.takeTurn(this))
      remaining.foreach(_.tallyScore(round))
      hasMatch = false

    }

  def checkMatch(p: Player, indexes: List[Int]): Boolean =
    {
      val cards = indexes.map(p.hand(_)).sortBy(_.value)
      //same suit AND run
      //same value suit doesnt matter
      val checkValue: Int = cards.head.value
      if(cards.forall(c => {c.value == checkValue})) {
        p.addToSubHand(indexes)
        if(!hasMatch) {hasMatch = true}
        true
      }
      else
        {
          val checkSuit: Suit = cards.head.suit
          if(cards.forall(c => {c.suit == checkSuit}))
            {
              for(c <- 0 until cards.length-1)
                {
                  if(cards(c).value - cards(c+1).value != 1) false
                }
              p.addToSubHand(indexes)
              if(!hasMatch) hasMatch = true
              true
            }
          else false
        }
    }
  def checkHand(p: Player, matches: List[List[Int]]): Boolean = {
    if(matches.forall(checkMatch(p,_))) {
      hasMatch = true
      firstMatch = p
      true
    } else false
  }
}

object Game {
  def apply(numPlayers: Int): Game = new Game(numPlayers)

  val cardValToName: Map[Int, String] = {
    List((3, "Three"), (4, "Four"), (5, "Five"), (6, "Six"), (7, "Seven"), (8, "Eight"), (9, "Nine"), (10, "Ten"),
      (11, "Jack"), (12, "Queen"), (13, "King"), (50, "Joker")).toMap
  }

  val cardSuitToName: Map[Suit, String] = {
    List((Suit.Clubs, "Clubs"), (Suit.Diamonds, "Diamonds"), (Suit.Hearts, "Hearts"), (Suit.Spades, "Spades"), (Suit.Stars, "Stars"), (null, "")).toMap
  }

  def cardNameMap(card: Card): String = cardValToName(card.value) + (if(card.value == 50) "" else " of ") + cardSuitToName(card.suit)
}