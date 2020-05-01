package agent

import agent.NodeAction.NodeAction
import game.Card

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class ChanceNode extends Node {
  var knownCards: ArrayBuffer[Card] = _
  var parent: Node = _
  var children: ArrayBuffer[Node] = _
  var score: Double = _
  var actionType: NodeAction = NodeAction.Chance

  def genCard: Card = {
    val possibilites = Card.allCards.diff(knownCards)
    possibilites(Random.nextInt(possibilites.length))
  }
}
