package game

import game.Suit.Suit

import scala.collection.mutable.{ArrayBuffer, Stack}
import scala.util.Random

class Game(val numPlayers: Int) {
  //num players (immutable)
  //draw deck (mutable, stack, starts at 116, deal 3 to each player)
  //discard deck (mutable, stack, starts at 0, push discards on as they occur)
  //list of players (immutable)

  private var drawDeck: Stack[Card] = Stack.empty[Card].pushAll(Random.shuffle(Card.allCards))
  private var discardDeck: Stack[Card] = Stack.empty
  private var players: List[Player] = List.fill(numPlayers)(new Player("Joe"))
  private var _hasMatch: Boolean = false
  private var firstMatch: Player = _

  def hasMatch: Boolean = {
    _hasMatch
  }

  def discardIsEmpty: Boolean = discardDeck.isEmpty

  def getDiscard: Vector[Card] = discardDeck.toVector

  def peekDiscard: Card = discardDeck.top

  def test(roundNum: Int): Unit = {
    players = List(new Player("jim"), new Player("bob"))
    for (i <- 1 to roundNum) {
      players.head.addToHand(Card(10, Suit.Clubs))
    }
    players.tail.head.addToHand(Card(4, Suit.Spades))
    players.tail.head.addToHand(Card(4, Suit.Stars))
    players.tail.head.addToHand(Card(10, Suit.Stars))
  }

  def deal(numCards: Int): Unit = {
    for (i <- 0 until numCards) {
      players.foreach(p => draw("deck", p))
    }
  }

  def draw(drawFrom: String, p: Player): Unit = {
    if (drawDeck.isEmpty) {
      drawDeck = discardDeck.clone()
      discardDeck.clear()
    }
    drawFrom match {
      case "discard" => p.addToHand(discardDeck.pop())
      case "deck" => p.addToHand(drawDeck.pop)
      case _ => println("Error: How did this even happen?")
    }
  }

//  def generateSim(roundNum: Int): SimRound = {
//    new SimRound(roundNum)
//  }

  def discard(p: Player, i: Int): Unit = discardDeck.push(p.removeFromHand(i))

  def playGame(): Unit = {
    for (roundNum <- 3 to 13) {
      playRound(roundNum)
    }
    val winner = players.minBy(_.score)
    println(s"Congratuations ${winner.name} you have won with a score of ${winner.score}!")
  }

  def playRound(round: Int): Unit = {
    players.foreach(_.emptyHand)
    deal(round)
    while (!_hasMatch) {
      players.foreach(p => if (!_hasMatch) p.takeTurn(this, round))
    }
    val remaining = players.filter(_ != firstMatch)
    remaining.foreach(_.takeTurn(this, round))
    remaining.foreach(_.tallyScore(round))
    _hasMatch = false
  }

  def simIsMatch(hand: Vector[Card], roundNum: Int): Boolean = {
    if (hand.length < 3) {
      false
    } else {
      val sorted = hand.sortBy(_.value)
      val checkValue = sorted.head.value
      if(sorted.forall(_.value == checkValue)) true else {
        val checkSuit: Suit = sorted.head.suit
        if (sorted.forall(c => {
          c.suit == checkSuit
        })) {
          var isStraight = true
          for (c <- 0 until sorted.length - 1) {
            if (sorted(c).value - sorted(c + 1).value != 1) isStraight = false
          }
          isStraight
        }
        else false
      }
    }
  }

  def possibleCards(hand: Vector[Card], discard: Vector[Card]): Vector[Card] = Card.allCards.diff(hand ++ discard).toVector

  def checkMatch(p: Player, indexes: List[Int], roundNum: Int): Boolean = {
    println(_hasMatch)
    if (indexes.length < 3) {
      println("A match requires 3 or more cards.")
      false
    }
    else {
      val cards = indexes.map(p.hand(_)).filter(c => c.value != 50 && c.value != roundNum).sortBy(_.value)
      println(cards.toList)
      val checkValue: Int = cards.head.value
      if (cards.forall(c => {
        c.value == checkValue
      })) {
        p.addToSubHand(indexes)
        if (!_hasMatch) {
          firstMatch = p
          _hasMatch = true
        } else {
          println("reached else")
          firstMatch = p
          indexes.foreach(p.removeFromHand)
        }
        true
      }
      else {
        val checkSuit: Suit = cards.head.suit
        if (cards.forall(c => {
          c.suit == checkSuit
        })) {
          for (c <- 0 until cards.length - 1) {
            if (cards(c).value - cards(c + 1).value != 1) false
          }
          p.addToSubHand(indexes)
          if (!_hasMatch) {
            firstMatch = p
            _hasMatch = true
          }
          true
        }
        else false
      }
    }
  }

  def checkHand(p: Player, matches: List[List[Int]], roundNum: Int): Boolean = {
    if (matches.forall(checkMatch(p, _, roundNum))) {
      _hasMatch = true
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

  def cardNameMap(card: Card): String = cardValToName(card.value) + (if (card.value == 50) "" else " of ") + cardSuitToName(card.suit)
}