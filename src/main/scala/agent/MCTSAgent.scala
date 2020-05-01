package agent
import game.{Card, Game, Player}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class MCTSAgent(val game: Game) extends Agent {
  var roundNum: Int = 3
  var p: Player = null
  val knownCards: ArrayBuffer[Card] = ArrayBuffer.empty
  val discard: mutable.Stack[Card] = mutable.Stack.empty

  def getDraw(timeDue: Long): Boolean = {
    game.generateSim(roundNum)
    val root = new StateNode
    root.game = game
    root.parent = null
    root.children = ArrayBuffer.empty
    root.state = p
    root.score = 0
    root.turn = 1
    root.visits = 0

    val start = System.currentTimeMillis()

    while(start + timeDue > System.currentTimeMillis()) {

    }
  }

  private def selection(root: StateNode): StateNode = {
    val childrenWithGen = root.children.map {n =>
      if(n.isInstanceOf[StateNode]) {
        n
      } else if(n.isInstanceOf[ChanceNode]) {
        val gen = n.genCard
        val turn = if(n.actionType == NodeAction.Discard) -root.turn else root.turn
        val newState = root.state.addOne(gen)
        val newAction = if(root.actionType == NodeAction.Draw) NodeAction.Discard else NodeAction.Draw
        val sn = StateNode(n, turn, newState, game, ???, newAction)
      }
    }.asInstanceOf[List[StateNode]]
    val node = childrenWithGen.maxBy(uct)
    if (node == root) root
    else if (node.children.nonEmpty) selection(node)
    else node
  }

  //

  private def expansion(node: StateNode): Node = {
    if ((node.actionType == NodeAction.Discard && node.hasMatched) || (node.turn == -1 && node.hasMatched)) node
    else {
      node.sim.game.getValidMoves(node.sim.state, node.sim.turn).forEach((move: Position) => {
        def foo(move: Position) = {
          val resultBoard = node.sim.game.simulateMove(node.sim.state, move)
          val newSim = new SimData
          newSim.game = node.sim.game
          newSim.turn = otherPlayer(move.getPiece.getOwner)
          newSim.state = resultBoard
          val newNode = new Node
          newNode.sim = newSim
          newNode.parent = node
          newNode.children = new ArrayList[Node]
          node.children.add(newNode)
        }

        foo(move)
      })
      node.children.get(rnd.nextInt(node.children.size))
    }
  }


  def getDiscard(game: Game, timeDue: Long): Int = {



    val retValue = null
    knownCards.addOne(retValue)
    ???
  }

  def getMatch(game: Game, timeDue: Long): List[Int] = ???

  private def uct(n: StateNode): Double = {
    val wi = n.score.toDouble
    val ni = n.visits.toDouble
    val c = 1.41
    val t = n.parent.visits.toDouble
    wi / ni + c * Math.sqrt(Math.log(t) / ni)
  }
}

















/*
Draw: Compare weighted average of draw deck outcome with discard deck outcome. Both will involve very complicated random playouts.
Discard: Compare average of discard results

Things the AI knows:
The AI knows what its hand is. The AI knows that anything it has discarded it not in the draw deck until the draw deck is switched with the discard deck.
The AI knows every card in the discard pile, and they know the order.

Random playouts will involve: having a simulated other player making simulated random actions, and also having you take simulated random actions.
Keeping in mind that the simulated other player will have a random hand. That random hand will have to consist
 */


























