package agent

import agent.NodeAction.NodeAction
import game.Card

import scala.collection.mutable.ArrayBuffer

class ChanceNode(val hand: Vector[Card], val discard: Vector[Card], val parent: Node,
                 val actionType: NodeAction, val hasMatched: Boolean, val turn: Int) extends Node {
  val children: ArrayBuffer[Node] = ArrayBuffer.empty
  val childProbs: ArrayBuffer[Double] = ArrayBuffer.empty
  var score: Double = _
  var visits: Int = _
}

object ChanceNode {
  def apply(hand: Vector[Card], discard: Vector[Card], parent: Node,
            actionType: NodeAction, hasMatched: Boolean, turn: Int): ChanceNode = {
    new ChanceNode(hand, discard, parent, actionType, hasMatched, turn)
  }
}