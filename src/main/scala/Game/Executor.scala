package game

import agent.{ChanceNode, MCTSAgent, NodeAction, StateNode}
import game.Game

object Executor extends App {
  val g = Game(2)
  val mcts = new MCTSAgent(g)
  mcts.p = g.players.head
  mcts.p.agent = mcts
  g.playGame()
  //g.playRound(6)

//  val testHand = Card.allCards.take(6).toVector
//  val root = StateNode(null, 1, testHand, Vector.empty, g, false, NodeAction.Draw)
//  val rootChild = ChanceNode(testHand, Vector.empty, root, NodeAction.Chance, false, 1)
//  val asd = StateNode(rootChild, 1, testHand.prepended(Card(10, Suit.Clubs)).prepended(Card(13, Suit.Hearts)), Vector.empty, g, false, NodeAction.Discard)
//  root.children.addOne(rootChild)
//  rootChild.children.addOne(asd)
//  rootChild.childProbs.addOne(1.0 / 50.0)
//  var start = 0
//  while(start < 1000) {
//    val store = mcts.expansion(mcts.selection(asd))
//    val sim = mcts.simulation(store)
//    val back = mcts.backPropagation(sim, store)
//    start += 1
//  }
//  (asd.children.foreach(x => println(x.score, x.visits, x.score/x.visits, x.hand)))
}