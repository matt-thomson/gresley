package uk.co.mattthomson.coursera.ggp.gresley.player

import akka.actor.Props
import uk.co.mattthomson.coursera.ggp.gresley.player.single.SequentialPlanningPlayer
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{GameDescription, PlayerPropsFactory}

class PlayerPropsFactoryImpl extends PlayerPropsFactory {
  override def forGame(game: GameDescription) =
    if (game.roles.size == 1) Props[SequentialPlanningPlayer]
    else Props[RandomPlayer]
}
