package uk.co.mattthomson.coursera.ggp.gresley.player

import org.scalatest.{BeforeAndAfter, FlatSpec}
import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.{ActorRef, Actor, Props, ActorSystem}
import scala.concurrent.duration._
import uk.co.mattthomson.coursera.ggp.gresley.gdl._
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.{SelectMove, NewGame, GamesInProgress}
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.MoveSelectorPropsFactory
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Ready

class GameManagerSpec extends TestKit(ActorSystem("TestActorSystem")) with FlatSpec with ImplicitSender with BeforeAndAfter {
  "The manager" should "respond to an info message" in {
    val manager = system.actorOf(Props(new GameManager(_ => new DummyPlayer, new DummyMoveSelectorPropsFactory)))
    
    manager ! Info
    expectMsg("((name gresley) (status available))")
  }

  it should "respond to a start message" in {
    val manager = system.actorOf(Props(new GameManager(_ => new DummyPlayer, new DummyMoveSelectorPropsFactory)))
    
    val id = startGame(manager)

    manager ! GamesInProgress
    expectMsg(List(id))
  }

  it should "respond to a play message" in {
    val manager = system.actorOf(Props(new GameManager(_ => new DummyPlayer, new DummyMoveSelectorPropsFactory)))
    
    val id = startGame(manager)

    manager ! Play(id, None)
    expectMsg(Action("left", Nil))
  }

  it should "respond to a stop message" in {
    val manager = system.actorOf(Props(new GameManager(_ => new DummyPlayer, new DummyMoveSelectorPropsFactory)))
    
    val id = startGame(manager)

    manager ! Stop(id, List(Action("left", Nil), Action("right", Nil)))
    expectMsg("done")

    manager ! GamesInProgress
    expectMsg(Nil)
  }

  it should "respond to an abort message" in {
    val manager = system.actorOf(Props(new GameManager(_ => new DummyPlayer, new DummyMoveSelectorPropsFactory)))

    val id = startGame(manager)

    abortGame(manager, id)

    manager ! GamesInProgress
    expectMsg(Nil)
  }

  private def startGame(manager: ActorRef): String = {
    val game = GameDescription("(role black)")
    val id = s"id-${System.nanoTime()}"
    manager ! Start(id, "black", game, 1.second, 2.seconds)
    expectMsg(Ready)

    id
  }

  private def abortGame(manager: ActorRef, id: String) {
    manager ! Abort(id)
    expectMsg("done")
  }
}

class DummyPlayer extends Actor {
  override def receive: Receive = {
    case NewGame(_, _, source, _) => source ! Ready
    case SelectMove(source, _, _) => source ! Action("left", Nil)
  }
}

class DummyMoveSelectorPropsFactory extends MoveSelectorPropsFactory {
  override def forGame(game: GameDescription) = Seq(Props[DummyMoveSelector])
}