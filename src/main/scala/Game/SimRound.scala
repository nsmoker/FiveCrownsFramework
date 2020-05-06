//package game
//
//import scala.collection.mutable
//import scala.util.Random
//
//class SimRound(roundNum: Int) {
//  private var discardDeck: mutable.Stack[Card] = mutable.Stack.empty
//  private val otherHand: = _
//
//  def simDraw(knownCards: List[Card]): Card = {
//    val possible = Card.allCards.diff(knownCards)
//    possible(Random.nextInt(possible.length))
//  }
//
//  def simDiscard(p: Player, i: Int): Unit = discardDeck.push(p.removeFromHand(i))
//}
