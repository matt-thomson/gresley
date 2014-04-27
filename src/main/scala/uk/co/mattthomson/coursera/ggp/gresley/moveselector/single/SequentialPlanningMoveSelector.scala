package uk.co.mattthomson.coursera.ggp.gresley.moveselector.single

import uk.co.mattthomson.coursera.ggp.gresley.gdl.{Action, GameState}
import akka.actor.{Actor, ActorLogging}
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.{SelectedMove, Play, Initialized, Initialize}

class SequentialPlanningMoveSelector extends Actor with ActorLogging {
  override def receive: Receive = {
    case Initialize(game, role) =>
      val (actions, value) = bestPlan(game.initialState, role)
      log.info(s"Plan with value $value: ${actions.mkString("\n")}")

      sender ! Initialized(actions)

    case Play(_, _, _, playerState) =>
      val actions = playerState.asInstanceOf[Seq[Action]]

      log.info(s"Chosen action: ${actions.head}")
      sender ! SelectedMove(actions.head, actions.tail)
  }

  private def bestPlan(state: GameState, role: String): (List[Action], Int) = {
    if (state.isTerminal) (Nil, state.value(role))
    else state.legalActions(role).map { action =>
      val newState = state.update(Map(role -> action))
      val (otherActions, value) = bestPlan(newState, role)
      (action :: otherActions, value)
    }.maxBy(_._2)
  }
}
