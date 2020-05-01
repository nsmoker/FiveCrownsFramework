package agent

import agent.NodeAction.NodeAction

import scala.collection.mutable
import game._

import scala.collection.mutable.ArrayBuffer

class StateNode extends Node {
  var parent: Node = _
  var children: mutable.ArrayBuffer[Node] = _
  var turn: Int = _
  var state: ArrayBuffer[Card] = _
  var game: Game = null
  var score: Double = _
  var visits: Int = _
  var hasMatched: Boolean = false
  var actionType: NodeAction = _

  def hasParent: Boolean = parent != null

  def genCard: Card = ???
}

object StateNode {
  def apply(parent: Node, turn: Int, state: ArrayBuffer[Card], game: Game, hasMatched: Boolean, actionType: NodeAction): StateNode = {
    val ret = new StateNode
    ret.parent = parent
    ret.turn = turn
    ret.state = state
    ret.game = game
    ret.hasMatched = hasMatched
    ret.actionType = actionType
    ret
  }
}