package uk.co.mattthomson.coursera.ggp.gresley.moveselector

import akka.actor.Props
import uk.co.mattthomson.coursera.ggp.gresley.gdl.GameDescription
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.single.SequentialPlanningMoveSelector
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.multiple.AlphaBetaMoveSelector

class MoveSelectorPropsFactoryImpl extends MoveSelectorPropsFactory {
  override def forGame(game: GameDescription) =
    if (game.roles.size == 1) Props[SequentialPlanningMoveSelector]
    else Props[AlphaBetaMoveSelector]
}
