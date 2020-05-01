package agent

import game._

trait Agent {
  var responded = true

  def getDraw(game: Game, timeDue: Long): Boolean
  def getDiscard(game: Game, timeDue: Long): Int
  def getMatch(game: Game, timeDue: Long): List[Int]
  //def getMove(game: Game, timeDue: Long): Position
}
