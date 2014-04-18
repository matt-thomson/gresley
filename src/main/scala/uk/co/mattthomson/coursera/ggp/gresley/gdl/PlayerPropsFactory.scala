package uk.co.mattthomson.coursera.ggp.gresley.gdl

import akka.actor.Props

trait PlayerPropsFactory {
  def forGame(game: GameDescription): Props
}
