package uk.co.mattthomson.coursera.ggp.gresley.player

import akka.actor.Props
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{GameDescription, PlayerPropsFactory}
import uk.co.mattthomson.coursera.ggp.gresley.player.single.SequentialPlanningPlayer
import uk.co.mattthomson.coursera.ggp.gresley.player.multiple.MinimaxPlayer

class PlayerPropsFactoryImpl extends PlayerPropsFactory {
  override def forGame(game: GameDescription) =
    if (game.roles.size == 1) Props[SequentialPlanningPlayer]
    else if (game.roles.size == 2) Props[MinimaxPlayer]
    else Props[RandomPlayer]
}
