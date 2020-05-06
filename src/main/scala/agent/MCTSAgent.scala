package agent

import agent.NodeAction.NodeAction
import game.{Card, Game, Player}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class MCTSAgent(val game: Game) extends Agent {
  var roundNum: Int = 3
  var p: Player = null

  def getDraw(timeDue: Long): Boolean = {
    println("Starting MCTS Draw . . .")
    val root: Node = StateNode(null, 1, p.hand.toVector, game.getDiscard, game, game.hasMatch, NodeAction.Draw)
    val start = System.currentTimeMillis()
    while (start + timeDue > System.currentTimeMillis()) {
      //println("Loop completed")
      val guessNode = selection(root)
      //println(guessNode.hand.length)
      val expand = expansion(guessNode)
      //println(expand.hand.length)
      val res = simulation(expand)
      backPropagation(res, expand)
    }
    //root.children.foreach(n => println(n.score / n.visits))
    val bestDraw = root.children.minBy(n => n.score / n.visits)
    println("MCTS Draw Done")
    bestDraw.actionType == NodeAction.Chance
  }

  def getDiscard(timeDue: Long): Int = {
    println("Starting MCTS Discard . . .")
    val root: Node = StateNode(null, 1, p.hand.toVector, game.getDiscard, game, game.hasMatch, NodeAction.Discard)
    val start = System.currentTimeMillis()
    while (start + timeDue > System.currentTimeMillis()) {
      //println("Loop completed")
      //println("Root's hand: " + root.hand)
      //println("Root's children's hands: " + root.children.map(_.hand))
      //println("Root's children's scores: " + root.children.map(_.score))
      //println("Root's children count: " + root.children.length)
      val guessNode = selection(root)
      //println(guessNode.hand.length)
      val expand = expansion(guessNode)
      //println(expand.hand.length)
      val res = simulation(expand)
      backPropagation(res, expand)
    }
    val bestDiscard = root.children.minBy(n => n.score / n.visits)
    if (bestDiscard.hand.isEmpty) {
      game.checkMatch(p, p.hand.indices.toList, roundNum)
    } else if (root.hand.length - bestDiscard.hand.length >= 3) {
      val dif = root.hand.diff(bestDiscard.hand)
      val inds = dif.map(root.hand.indexOf(_))
      game.checkMatch(p, inds.toList, roundNum)
    }
    println("MCTS Discard Done")
    p.hand.indexOf(bestDiscard.discard.head)
  }

  def bestSubMatch(hand: Vector[Card]): Vector[Card] = {
    val byNum = hand.groupBy(_.value).values.toVector.filter(_.length >= 3).map(x => x.filter(y => y.value != 50 && y.value != roundNum))
    val bySuit = hand.groupBy(_.suit).values.toVector
    val ps = bySuit.flatMap(s => (3 to s.length).flatMap(s.combinations).map(_.sortBy(c => c.value)))
    val straights = ps.filter(game.simIsMatch(_, roundNum))
    knapSack(hand, byNum ++ straights, Vector.empty)
  }

  private def knapSack(hand: Vector[Card], seqs: Vector[Vector[Card]], ret: Vector[Card]): Vector[Card] = {
    if (hand.isEmpty || seqs.isEmpty) {
      ret
    } else {
      if (seqs.head.diff(hand).nonEmpty) {
        knapSack(hand, seqs.tail, ret)
      } else {
        val caseWith = knapSack(hand.diff(seqs.head), seqs.tail, ret ++ seqs.head)
        val caseWithout = knapSack(hand, seqs.tail, ret)
        val sumWith = caseWith.foldLeft(0.0)((x, y) => x + y.value)
        val sumWithout = caseWithout.foldLeft(0.0)((x, y) => x + y.value)
        if (sumWith >= sumWithout) ret ++ caseWith else ret ++ caseWithout
      }
    }
  }


  private def uct(n: Node): Double = {
    val wi = n.score.toDouble
    val ni = n.visits.toDouble
    if (ni == 0) Double.MaxValue else {
      val c = 1.41
      val t = n.parent.visits.toDouble
      -(wi / ni + c * Math.sqrt(Math.log(t) / ni))
    }
  }

  def selection(root: Node): Node = if (root.isLeaf) root else selection(root.children.maxBy(uct))

  def expansion(node: Node): Node = {
    if (!node.isLeaf) {
      throw new RuntimeException("Expansion: Node must be a leaf.")
    } else {
      if (node.hasMatched && node.turn == -1 && node.actionType == NodeAction.Draw) node
      else node match {
        case sn: StateNode =>
          if (sn.actionType == NodeAction.Draw) {
            val discardNode = if (sn.discard.nonEmpty) StateNode(sn, sn.turn, sn.hand.prepended(sn.discard.head),
              sn.discard.tail, game, sn.hasMatched, NodeAction.Discard) else null
            val drawDeckNode = ChanceNode(sn.hand, sn.discard, sn, NodeAction.Chance, sn.hasMatched, sn.turn)
            sn.children.addOne(drawDeckNode)
            if (discardNode != null) sn.children.addOne(discardNode)
            if (Random.nextInt(2) == 0 && discardNode != null) discardNode else drawDeckNode
          } else {
            val nodes = sn.hand.map(c => {
              if (!sn.hasMatched) {
                val won = game.simIsMatch(sn.hand.diff(List(c)), roundNum)
                StateNode(sn, -sn.turn, if (won) Vector.empty else sn.hand.diff(List(c)), sn.discard.prepended(c),
                  game, won, NodeAction.Draw)
              } else {
                val best = bestSubMatch(sn.hand.diff(List(c)))
                StateNode(sn, -sn.turn, sn.hand.diff(best.prepended(c)), sn.discard.prepended(c), game, node.hasMatched,
                  NodeAction.Draw)
              }
            })
            sn.children.addAll(nodes)
            nodes(Random.nextInt(nodes.length))
          }
        case cn: ChanceNode =>
          val possiblesPreShuffle = Card.allCards.diff(cn.hand ++ cn.discard)
          val possibleCards = if (possiblesPreShuffle.nonEmpty) possiblesPreShuffle else Card.allCards.diff(cn.hand)
          val nodes = possibleCards.map(c => StateNode(cn, cn.turn, cn.hand.prepended(c),
            if (possiblesPreShuffle.isEmpty) Vector.empty else cn.discard, game, cn.hasMatched, NodeAction.Discard))
          cn.children.addAll(nodes)
          //println(possibleCards.length)
          cn.childProbs.addAll(ArrayBuffer.fill(cn.children.length)(1.0 / possibleCards.length.toDouble))
          nodes(Random.nextInt(nodes.length))
      }
    }
  }

  def simulation(node: Node): Double = {
    def aux(hand: Vector[Card], discard: Vector[Card], action: NodeAction,
            hasMatched: Boolean, turn: Int): Double = {
      if (hasMatched && turn == -1 && action == NodeAction.Draw) {
        val score = hand.foldLeft(0.0)((s, c) => s + c.value)
        //println("Sim score: "+ score)
        score
      } else if (turn == 1) {
        if (action == NodeAction.Draw || action == NodeAction.Chance) {
          val res = Random.nextInt(2) //Randomize decision to draw from discard pile or draw deck
          if (res == 1 || discard.isEmpty) {
            val possiblesBeforeShuffle = game.possibleCards(hand, discard)
            val possibles = if (possiblesBeforeShuffle.nonEmpty) possiblesBeforeShuffle else game.possibleCards(hand, Vector.empty)
            aux(hand.prepended(possibles(Random.nextInt(possibles.length))), if (possiblesBeforeShuffle.nonEmpty) discard else Vector.empty,
              NodeAction.Discard, hasMatched, turn)
          } else aux(hand.prepended(discard.head), discard.tail, NodeAction.Discard, hasMatched, turn)
        } else {
          val res = Random.nextInt(hand.length)
          val resCard = hand(res)
          if (!hasMatched) {
            val complete = game.simIsMatch(hand.diff(List(resCard)), roundNum)
            aux(if (complete) hand.empty else hand.diff(List(resCard)), discard.prepended(resCard), NodeAction.Draw, complete, -turn)
          } else {
            val best = bestSubMatch(hand.diff(List(resCard)))
            aux(hand.diff(best ++ List(resCard)), discard.prepended(resCard), NodeAction.Draw, hasMatched, -turn)
          }
        }
      } else {
        if (action == NodeAction.Draw || action == NodeAction.Chance) {
          val res = Random.nextInt(2)
          if (res == 1) aux(hand, discard, NodeAction.Discard, hasMatched, turn)
          else aux(hand, discard.tail, NodeAction.Discard, hasMatched, turn)
        } else {
          val possibles = game.possibleCards(hand, discard)
          val didMatch = Random.nextInt(1000) == 5 // todo: Better probability calculation or increasing percentage (won't fix)
          aux(hand, if (possibles.nonEmpty) discard.prepended(possibles(Random.nextInt(possibles.length))) else Vector.empty, NodeAction.Draw, didMatch, -turn)
        }
      }
    }

    aux(node.hand, node.discard, node.actionType, node.hasMatched, node.turn)
  }

  def backPropagation(score: Double, node: Node): Node = {
    //println("score and node in backprop: " + score, node)
    node match {
      case sn: StateNode =>
        sn.score += score
      case cn: ChanceNode =>
        if(cn.children.isEmpty) cn.score = score else {
          cn.score += cn.children.indices.map(i => cn.children(i).score * cn.childProbs(i)).sum / cn.children.length.toDouble
        }
        //println("score and node in backprop chance: " + score, node)
        //println("children length: " + cn.children.length.toDouble)
    }
    node.visits += 1
    if (node.hasParent) {
      backPropagation(node.score, node.parent)
    } else {
      node
    }
  }
}