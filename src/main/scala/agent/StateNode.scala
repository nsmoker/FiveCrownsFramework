package agent

import agent.NodeAction.NodeAction

import scala.collection.mutable.ArrayBuffer
import game._

class StateNode(val hand: Vector[Card], val discard: Vector[Card], val parent: Node, val game: Game,
                 val actionType: NodeAction, val hasMatched: Boolean, val turn: Int, val weight: Double) extends Node {
  var score: Double = _
  var visits: Int = _
  val children: ArrayBuffer[Node] = ArrayBuffer.empty
}

object StateNode {
  def apply(parent: Node, turn: Int, hand: Vector[Card], discard: Vector[Card], game: Game, hasMatched: Boolean,
            actionType: NodeAction, weight: Double): StateNode = {
    new StateNode(hand, discard, parent, game, actionType, hasMatched, turn, weight)
  }
}


// Discard Pile
// Your hand
// If chance node, probs of children
// Turn Phase
// Whose turn it is
// Score
// Need a heuristic that prioritizes wins
// 