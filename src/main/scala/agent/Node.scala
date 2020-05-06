package agent

import agent.NodeAction.NodeAction
import game.Card

import scala.collection.mutable.ArrayBuffer

trait Node {
  val hand: Vector[Card]
  val discard: Vector[Card]
  val parent: Node
  val children: ArrayBuffer[Node]
  var score: Double
  var visits: Int
  val hasMatched: Boolean
  val actionType: NodeAction
  val turn: Int

  def isLeaf: Boolean = children.isEmpty
  def hasParent: Boolean = parent != null
}