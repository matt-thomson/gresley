package uk.co.mattthomson.coursera.ggp.gresley.moveselector

import akka.actor.Props
import uk.co.mattthomson.coursera.ggp.gresley.gdl.GameDescription

trait MoveSelectorPropsFactory {
  def forGame(game: GameDescription): Seq[Props]
}
