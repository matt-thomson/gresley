package uk.co.mattthomson.coursera.ggp.gresley.moveselector

import akka.actor.Props
import uk.co.mattthomson.coursera.ggp.gresley.gdl.GameDescription
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.single.SequentialPlanningMoveSelector
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.multiple.AlphaBetaMoveSelector
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.heuristic.IterativeDeepeningMoveSelector

class MoveSelectorPropsFactoryImpl extends MoveSelectorPropsFactory {
  private val moveSelectors = List(Props[AlphaBetaMoveSelector], Props[IterativeDeepeningMoveSelector])

  override def forGame(game: GameDescription) =
    if (game.roles.size == 1) Props[SequentialPlanningMoveSelector] :: moveSelectors
    else moveSelectors
}
