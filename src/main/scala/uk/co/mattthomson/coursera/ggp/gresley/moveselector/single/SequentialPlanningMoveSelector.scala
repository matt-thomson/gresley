package uk.co.mattthomson.coursera.ggp.gresley.moveselector.single

import uk.co.mattthomson.coursera.ggp.gresley.gdl.{Action, GameState}
import akka.actor.{Actor, ActorLogging}
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.{SelectedMove, Play, Initialized, Initialize}
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.TimeoutChecker
import org.joda.time.DateTime

class SequentialPlanningMoveSelector extends Actor with ActorLogging with TimeoutChecker {
  override def receive: Receive = {
    case Initialize(game, role, endTime) =>
      val (actions, value) = bestPlan(game.initialState, role, endTime)
      log.info(s"Found plan with value $value")

      sender ! Initialized(actions)

    case Play(_, state, _, _, metadata) =>
      val actions = metadata.asInstanceOf[Map[GameState, Action]]
      val chosenAction = actions(state)

      log.info(s"Chosen action: $chosenAction")
      sender ! SelectedMove(chosenAction)
  }

  private def bestPlan(state: GameState, role: String, endTime: DateTime): (Map[GameState, Action], Int) = {
    checkStillRunning(endTime)

    if (state.isTerminal) (Map(), state.value(role))
    else state.legalActions(role).map { action =>
      val newState = state.update(Map(role -> action))
      val (otherActions, value) = bestPlan(newState, role, endTime)
      (Map(state -> action) ++ otherActions, value)
    }.maxBy(_._2)
  }
}
