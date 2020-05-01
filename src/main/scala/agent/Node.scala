package agent

import agent.NodeAction.NodeAction
import game.Card

import scala.collection.mutable.ArrayBuffer

trait Node {
  var parent: Node
  var children: ArrayBuffer[Node]
  var score: Double
  var actionType: NodeAction

  def genCard: Card
}