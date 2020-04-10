package Game

import scala.collection.mutable.ArrayBuffer

class Player(val name: String)
{
  private var _score = 0
  private var _hand: ArrayBuffer[Card] = ArrayBuffer.empty

  def score: Int =
  {
    _score
  }

  def hand: ArrayBuffer[Card] =
  {
    _hand
  }

  def addToHand(c: Card): Unit =
    {
      _hand.append(c)
    }
  def removeFromHand(i: Int): Unit =
    {
      _hand.remove(i)
    }
  def emptyHand: Unit =
    {
      _hand.empty
    }
  def takeTurn: Unit =
    {
      ???
    }
}
