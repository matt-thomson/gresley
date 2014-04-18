package uk.co.mattthomson.coursera.ggp.gresley.player.single

import uk.co.mattthomson.coursera.ggp.gresley.player.Player
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{GameDescription, Action, GameState}
import akka.actor.ActorRef

class SequentialPlanningPlayer extends Player[Seq[Action]] {
  override def initialize(game: GameDescription, role: String): Seq[Action] = {
    val (actions, value) = bestPlan(game.initialState, role)
    log.info(s"Plan with value $value: ${actions.mkString("\n")}")
    actions
  }

  override def play(state: GameState, role: String, source: ActorRef, playerState: Seq[Action]) = {
   source ! playerState.head
   playerState.tail
  }

  private def bestPlan(state: GameState, role: String): (List[Action], Int) = {
    if (state.isTerminal) (Nil, state.value(role))
    else state.legalActions(role).map { action =>
      val newState = state.update(List(action))
      val (otherActions, value) = bestPlan(newState, role)
      (action :: otherActions, value)
    }.maxBy(_._2)
  }
}
