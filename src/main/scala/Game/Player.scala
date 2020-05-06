package game

import agent.Agent

import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn.readLine

class Player(val name: String, val isAI: Boolean) {
  private var _score = 0
  private var _hand: ArrayBuffer[Card] = ArrayBuffer.empty
  private var subHand: ArrayBuffer[Card] = ArrayBuffer.empty
  var agent: Agent = null

  def score: Int = {
    _score
  }

  def hand: ArrayBuffer[Card] = {
    _hand
  }

  def tallyScore(roundNum: Int): Unit = {
    _score += (_hand --= subHand).map(card => if (card.value == roundNum) 20 else card.value).sum
  }

  def addToSubHand(is: List[Int]): Unit = is.foreach(i => subHand.addOne(hand(i)))

  def addToHand(c: Card): Unit = {
    _hand.append(c)
  }

  def removeFromHand(i: Int): Card = {
    _hand.remove(i)
  }

  def emptyHand: Unit = {
    _hand.clear()
  }

  def interpretMatch(game: Game, roundNum: Int): Boolean = {
    var input = ""
    val buff = ArrayBuffer.empty[Int]
    while (input.trim != "done") {
      input = readLine("Please enter the number of the card you wish to add to the match, and then press enter.")
      if (input.trim != "done") buff.addOne(input.trim.toInt-1)
    }
    val result = game.checkMatch(this, buff.toList, roundNum)
    println("finished matching" + result)
    result
  }

  def printHand: Unit = {
    println((for (i <- hand.indices) yield {
      (i + 1).toString + ": " + Game.cardNameMap(hand(i)) + "\n"
    }).flatten.mkString)
  }

  def handleDraw(game: Game, selectedDeck: String, hasDrawn: Boolean): Boolean = {
    if (selectedDeck.trim == "deck" && !hasDrawn) {
      game.draw("deck", this)
      true
    }
    else if (selectedDeck.trim == "discard" && !hasDrawn) {
      game.draw("discard", this)
      true
    }
    else if (hasDrawn) {
      println("You have already drawn a card this round.")
      false
    } else {
      println("The world has ended. Please alert Dr. Horn.")
      false
    }
  }

  def handleDiscard(game: Game, card: Int, hasDrawn: Boolean, hasDiscarded: Boolean): Boolean = {
    if (hasDrawn && !hasDiscarded) {
      game.discard(this, card)
      true
    }
    else if (!hasDrawn) {
      println("You must draw a card before you discard.")
      false
    }
    else if (hasDiscarded) {
      println("You have already discarded.")
      false
    }
    else {
      println("The world has ended. Please alert Dr. Horn.")
      false
    }
  }

  def takeTurn(game: Game, roundNum: Int): Unit = if(isAI) takeTurnAI(game, roundNum) else takeTurnHuman(game, roundNum)

  def takeTurnAI(game: Game, roundNum: Int): Unit = {
    val drawRes = agent.getDraw(5000)
    if(drawRes) {
      handleDraw(game, "deck", false)
    } else {
      handleDraw(game, "discard", false)
    }
    val discardRes = agent.getDiscard(5000)
    handleDiscard(game, discardRes, true, false)
  }

  def takeTurnHuman(game: Game, roundNum: Int): Unit = {
    var endTurn = false
    var hasDrawn = false
    var hasDiscarded = false
    while (!hasDiscarded) {
      printHand
      val input = readLine("Type 'draw from deck' to draw from the deck\n" +
        "Type 'draw from discard' to draw from the discard pile\n" +
        "type 'discard' followed by the number of the card to discard, separated by a space (this will end your turn)\n" +
        "Type 'match' to check if your entire hand is a match \n" + (if(!game.discardIsEmpty)
        s"The top card of the discard is a ${Game.cardNameMap(game.peekDiscard)}" else ""))
      input.trim match {
        case d if d.split(" ").head == "draw" =>
          val outcome = handleDraw(game, d.split(" ").last, hasDrawn)
          if (outcome) hasDrawn = true
        case discard if discard.split(" ").head == "discard" =>
          val outcome = handleDiscard(game, discard.split(" ").last.toInt - 1, hasDrawn, hasDiscarded)
          if (outcome) hasDiscarded = true
        case "match" =>
          if (game.checkMatch(this, hand.indices.toList, roundNum)) {
            println("You have a match!")
            endTurn = true
            hasDiscarded = true
          }
          else {
            println("Sorry, you do not have match")
          }
        case _ => println("Sorry, we didn't recognize your command. Please enter valid input.")
      }
    }
    while (!endTurn) {
      printHand
      val input = readLine("Would you like to create a match? Type 'all' to match your entire hand, or 'match' to create a match with a subset of your hand. If you type match, you will be prompted to enter the number of each card. If you do not want to make a match, simply type 'no'.\n").trim
      if (input == "all" && game.checkMatch(this, hand.indices.toList, roundNum)) {
        println("You have a match!")
        endTurn = true
      } else if (input == "all" && !game.checkMatch(this, hand.indices.toList, roundNum)) {
        println("Sorry, there was no match found. Your turn is over.")
        endTurn = true
      }
      else if(input == "match") {
        if(game.hasMatch) {
          interpretMatch(game, roundNum)
        }
        else {
          println("You can only do a partial match on the final turn of the round.")
        }
      }
      if (input == "no") {
        endTurn = true
        println("Your turn is over.")
      }
      if (input != "all" && input != "match" && input != "no") {
        println("Sorry, we didn't recognize your command. Please enter valid input.")
      }
    }

  }
}
