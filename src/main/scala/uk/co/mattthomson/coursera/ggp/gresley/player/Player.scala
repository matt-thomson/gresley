package uk.co.mattthomson.coursera.ggp.gresley.player

import akka.actor._
import org.joda.time.DateTime
import scala.concurrent.duration._
import scala.util.Random
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{GameDescription, GameState}
import uk.co.mattthomson.coursera.ggp.gresley.player.Player._
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.PlayersMoved
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Action
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Play
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.NewGame
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Initialized
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Initialize
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.SelectMove
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.SelectedMove

class Player(moveSelectorProps: Seq[Props]) extends Actor with ActorLogging {
  import context.dispatcher

  override def receive = {
    case NewGame(game, role, source, timeout) =>
      val moveSelectors = moveSelectorProps.map(context.actorOf)
      moveSelectors.foreach(_ ! Initialize(game, role))

      context.system.scheduler.scheduleOnce(timeout - 2.seconds, self, Timeout)
      context.become(awaitInitialize(game, role, moveSelectors.zip(moveSelectorProps).toMap, Map(), source))
  }

  private def awaitInitialize(game: GameDescription,
                              role: String,
                              moveSelectors: Map[ActorRef, Props],
                              metadatas: Map[Props, Any],
                              source: ActorRef): Receive = {
    case Initialized(metadata) =>
      sender ! PoisonPill

      val props = moveSelectors.get(sender)
      val updatedMoveSelectors = props.fold(moveSelectors)(p => moveSelectors - sender)
      val updatedMetadatas = props.fold(metadatas)(p => metadatas + (p -> metadata))

      context.become(awaitInitialize(game, role, updatedMoveSelectors, updatedMetadatas, source))
      
    case Timeout =>
      moveSelectors.keys.foreach(_ ! PoisonPill)
      source ! Ready
      context.become(handle(game, role, game.initialState, metadatas))
  }

  private def handle(game: GameDescription, role: String, state: GameState, metadatas: Map[Props, Any]): Receive = {
    case PlayersMoved(moves) =>
      val actions = game.roles.zip(moves).toMap
      context.become(handle(game, role, state.update(actions), metadatas))

    case SelectMove(source, timeout) =>
      log.info(s"Current state:\n${state.trueFacts.mkString("\n")}")
      log.info(s"Legal actions:\n${state.legalActions(role).mkString("\n")}")

      val moveSelectors = metadatas.map { case (props, metadata) =>
        val moveSelector = context.actorOf(props)
        moveSelector ! Play(game, state, role, DateTime.now().plus(timeout.toMillis), metadata)
        (moveSelector, props)
      }.toMap

      context.system.scheduler.scheduleOnce(timeout - 2.seconds, self, Timeout)
      context.become(awaitMove(game, role, state, moveSelectors, metadatas, None, source))
  }

  private def awaitMove(game: GameDescription,
                        role: String,
                        state: GameState,
                        moveSelectors: Map[ActorRef, Props],
                        metadatas: Map[Props, Any],
                        bestAction: Option[(Props, Action)],
                        source: ActorRef): Receive = {
    case SelectedMove(action) =>
      val props = moveSelectors.get(sender)

      val updatedBestAction = props.fold(bestAction)(p => bestAction match {
        case Some((oldProps, _)) => if (moveSelectorProps.indexOf(p) <= moveSelectorProps.indexOf(oldProps)) Some((p, action)) else bestAction
        case None => Some((p, action))
      })

      context.become(awaitMove(game, role, state, moveSelectors, metadatas, updatedBestAction, source))

    case Timeout =>
      moveSelectors.keys.foreach(_ ! PoisonPill)
      val chosenAction = bestAction match {
        case Some((_, action)) => action
        case None => Random.shuffle(state.legalActions(role)).head
      }

      log.info(s"Playing $chosenAction")
      source ! chosenAction

      context.become(handle(game, role, state, metadatas))
  }
}

object Player {
  def apply(moveSelectorProps: Seq[Props]): Player = new Player(moveSelectorProps)

  case class Initialize(game: GameDescription, role: String)

  case class Initialized(metadata: Any)

  case object Ready {
    override def toString = "ready"
  }

  case class Play(game: GameDescription, state: GameState, role: String, endTime: DateTime, metadata: Any)

  case class SelectedMove(action: Action)

  case object Timeout
}