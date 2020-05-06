package agent

import agent.NodeAction.NodeAction
import game.{Card, Game, Player}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class MCTSAgent(val game: Game) extends Agent {
  var roundNum: Int = 3
  var p: Player = null

  def getDraw(timeDue: Long): Boolean = {
    val root: Node = StateNode(null, 1, p.hand.toVector, game.getDiscard, game, game.hasMatch, NodeAction.Draw, 1.0)
    val start = System.currentTimeMillis()
    while (start + timeDue > System.currentTimeMillis()) {
      val guessNode = selection(root)
      val expand = expansion(guessNode)
      val res = simulation(expand)
      backPropagation(res, expand)
    }
    val bestDraw = root.children.minBy(n => n.score / n.visits)
    bestDraw.actionType == NodeAction.Chance
  }

  def getDiscard(timeDue: Long): Int = {
    val root: Node = StateNode(null, 1, p.hand.toVector, game.getDiscard, game, game.hasMatch, NodeAction.Discard, 1.0)
    val start = System.currentTimeMillis()
    while (start + timeDue > System.currentTimeMillis()) {
      val guessNode = selection(root)
      val expand = expansion(guessNode)
      val res = simulation(expand)
      backPropagation(res, expand)
    }
    val bestDraw = root.children.minBy(n => n.score / n.visits)
    p.hand.indexOf(bestDraw.discard.head)
  }

  private def bestSubMatch(hand: Vector[Card]): Vector[Card] = {
    val byNum = hand.groupBy(_.value).values.filter(_.length >= 3).map(x => x.filter(y => y.value != 50 && y.value != roundNum))
    val bySuit = hand.groupBy(_.suit).values
    val ps = bySuit.map(s => (3 to s.length).flatMap(s.combinations).map(_.sortBy(c => c.value)))
    val straights = ps.map(s => s.filter(game.simIsMatch(_, roundNum)))
  }


  private def uct(n: Node): Double = {
    val wi = n.score.toDouble
    val ni = n.visits.toDouble
    val c = 1.41
    val t = n.parent.visits.toDouble
    wi / ni + c * Math.sqrt(Math.log(t) / ni)
  }

  private def cardProb(hand: Vector[Card], discard: Vector[Card]): Double = 1.0 / (56.0 - hand.length - discard.length)

  private def selection(root: Node): Node = if (root.isLeaf) root else selection(root.children.maxBy(uct))

  private def expansion(node: Node): Node = {
    if (!node.isLeaf) {
      throw new RuntimeException("Expansion: Node must be a leaf.")
    } else {
      if (node.hasMatched && node.turn == -1 && node.actionType == NodeAction.Draw) node
      else node match {
        case sn: StateNode =>
          if (sn.actionType == NodeAction.Draw) {
            val discardNode = StateNode(sn, sn.turn, sn.hand.prepended(sn.discard.head),
              sn.discard, game, sn.hasMatched, NodeAction.Discard, 1.0)
            val drawDeckNode = ChanceNode(sn.hand, sn.discard, sn, NodeAction.Chance, sn.hasMatched, sn.turn, 1.0)
            sn.children.addAll(List(discardNode, drawDeckNode))
            if (Random.nextInt(1) == 0) discardNode else drawDeckNode
          } else {
            val nodes = sn.hand.map(c => StateNode(sn, -sn.turn, sn.hand.diff(List(c)), sn.discard.prepended(c),
              game, game.simIsMatch(sn.hand.filter(_ != c), roundNum), NodeAction.Draw, 1.0))
            sn.children.addAll(nodes)
            nodes(Random.nextInt(nodes.length))
          }
        case cn: ChanceNode =>
          val possibleCards = Card.allCards.diff(cn.hand ++ cn.discard)
          val nodes = possibleCards.map(c => StateNode(cn, cn.turn, cn.hand.prepended(c), cn.discard, game,
            cn.hasMatched, NodeAction.Draw, cardProb(cn.hand, cn.discard)))
          cn.children.addAll(nodes)
          cn.childProbs.addAll(ArrayBuffer.fill(cn.children.length)(1.0 / possibleCards.length.toDouble))
          nodes(Random.nextInt(nodes.length))
      }
    }
  }

  private def simulation(node: Node): Double = {
    def aux(hand: Vector[Card], discard: Vector[Card], action: NodeAction,
            hasMatched: Boolean, turn: Int): Double = {
      if (hasMatched && turn == -1 && action == NodeAction.Draw) {
        hand.foldLeft(0.0)((s, c) => s + c.value)
      } else if (turn == 1) {
        if (action == NodeAction.Draw || action == NodeAction.Chance) {
          val res = Random.nextInt(1) //Randomize decision to draw from discard pile or draw deck
          if (res == 1) {
            val possibles = game.possibleCards(hand, discard)
            aux(hand.prepended(possibles(Random.nextInt(possibles.length))), discard,
              NodeAction.Discard, hasMatched, turn)
          } else aux(hand.prepended(discard.head), discard.tail, NodeAction.Discard, hasMatched, turn)
        } else {
          val res = Random.nextInt(hand.length)
          val resCard = hand(res)
          aux(hand.diff(List(resCard)), discard.prepended(resCard), NodeAction.Draw, ???, -turn) //todo: check for a match
        }
      } else {
        if (action == NodeAction.Draw || action == NodeAction.Chance) {
          val res = Random.nextInt(1)
          if (res == 1) aux(hand, discard, NodeAction.Discard, hasMatched, turn)
          else aux(hand, discard.tail, NodeAction.Discard, hasMatched, turn)
        } else {
          val possibles = game.possibleCards(hand, discard)
          val didMatch = Random.nextInt(1000) == 50 // todo: Better probability calculation or increasing percentage (won't fix)
          aux(hand, discard.prepended(possibles(Random.nextInt(possibles.length))), NodeAction.Draw, didMatch, -turn)
        }
      }
    }

    aux(node.hand, node.discard, node.actionType, node.hasMatched, node.turn)
  }

  private def backPropagation(score: Double, node: Node): Node = {
    node match {
      case sn: StateNode =>
        sn.score += 1
        sn.visits += 1
      case cn: ChanceNode =>
        cn.children.indices.map(i => cn.children(i).score * cn.childProbs(i)).sum / cn.children.length.toDouble
    }
    if (node.hasParent) {
      backPropagation(score, node.parent)
    } else {
      node
    }
  }
}