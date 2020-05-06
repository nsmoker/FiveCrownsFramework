package agent

import game._

trait Agent {
  var responded = true
  val game: Game

  def getDraw(timeDue: Long): Boolean
  def getDiscard(timeDue: Long): Int
}
