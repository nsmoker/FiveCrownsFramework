package game

import agent.{ChanceNode, MCTSAgent, NodeAction, StateNode}
import game.Game

object Executor extends App {
  val g = Game(2)
//
  val mcts = new MCTSAgent(g)
//
  mcts.p = g.players.head

//  mcts.p.addToHand(Card(3, Suit.Clubs))
//  mcts.p.addToHand(Card(4, Suit.Clubs))
//  mcts.p.addToHand(Card(5, Suit.Clubs))
//  mcts.p.addToHand(Card(9, Suit.Diamonds))
//  mcts.p.addToHand(Card(12, Suit.Stars))
//  mcts.p.addToHand(Card(11, Suit.Spades))
//
//  println(mcts.bestSubMatch(mcts.p.hand.toVector))
//
  mcts.p.agent = mcts

  g.playRound(6)

//  val testHand = Card.allCards.take(3).toVector
//
//  val root = StateNode(null, 1, testHand, Vector.empty, g, false, NodeAction.Draw)
//
//  val rootChild = ChanceNode(testHand, Vector.empty, root, NodeAction.Chance, false, 1)
//
//  val asd = StateNode(rootChild, 1, testHand.prepended(Card(10, Suit.Clubs)), Vector.empty, g, false, NodeAction.Discard)
//
//  root.children.addOne(rootChild)
//
//  rootChild.children.addOne(asd)
//  rootChild.childProbs.addOne(1.0 / 50.0)
//
//  val store = mcts.expansion(mcts.selection(root))
//
//  val sim = mcts.simulation(store)
//
//  val back = mcts.backPropagation(sim, store)
//
//  println("sim: " + sim)
//
//  println("root score" + root.score)
//
//
//  //val store1 = mcts.selection(root)
//
//  //println(store1.actionType, store1.hand, store1.parent.hand)
//
//  println(store.actionType, store.parent.actionType, store.hand, store.discard)
}