package uk.co.mattthomson.coursera.ggp.gresley.player

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.{Actor, Props, ActorSystem}
import org.scalatest.FlatSpec
import scala.concurrent.duration._
import uk.co.mattthomson.coursera.ggp.gresley.gdl.GameDescription
import uk.co.mattthomson.coursera.ggp.gresley.player.Player._
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Action
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Play
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.NewGame
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Initialized
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Initialize
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.SelectMove

class PlayerSpec extends TestKit(ActorSystem("TestActorSystem")) with FlatSpec with ImplicitSender {
  private val game = GameDescription("(role black) (legal black left)")

  "A player" should "initialize" in {
    val player = system.actorOf(Props(new Player(Seq(Props[DummyMoveSelector]))))

    player ! NewGame(game, "black", self, 3.seconds)
    expectMsg(Ready)
  }

  it should "select a move" in {
    val player = system.actorOf(Props(new Player(Seq(Props[DummyMoveSelector]))))

    player ! NewGame(game, "black", self, 3.seconds)
    expectMsg(Ready)

    player ! SelectMove(self, 3.seconds)
    expectMsg(Action("hi", Nil))
  }

  it should "keep player state up to date" in {
    val player = system.actorOf(Props(new Player(Seq(Props[DummyMoveSelector]))))

    player ! NewGame(game, "black", self, 3.seconds)
    expectMsg(Ready)

    player ! SelectMove(self, 3.seconds)
    expectMsg(Action("hi", Nil))

    player ! SelectMove(self, 3.seconds)
    expectMsg(Action("hi", Nil))
  }

  it should "respond with the result of the first actor in the list" in {
    val player = system.actorOf(Props(new Player(Seq(
      Props[DummyMoveSelector],
      Props(new DelayMoveSelector(50.milliseconds))
    ))))

    player ! NewGame(game, "black", self, 3.seconds)
    expectMsg(Ready)

    player ! SelectMove(self, 3.seconds)
    expectMsg(Action("hi", Nil))
  }

  it should "respond with the result of the first actor after a delay" in {
    val player = system.actorOf(Props(new Player(Seq(
      Props(new DelayMoveSelector(50.milliseconds)),
      Props[DummyMoveSelector]
    ))))

    player ! NewGame(game, "black", self, 3.seconds)
    expectMsg(Ready)

    player ! SelectMove(self, 3.seconds)
    expectMsg(Action("hello", Nil))
  }

  it should "skip over in the event of a timeout" in {
    val player = system.actorOf(Props(new Player(Seq(
      Props(new DelayMoveSelector(1.1.seconds)),
      Props[DummyMoveSelector]
    ))))

    player ! NewGame(game, "black", self, 3.seconds)
    expectMsg(Ready)

    player ! SelectMove(self, 3.seconds)
    expectMsg(Action("hi", Nil))

    player ! SelectMove(self, 3.seconds)
    expectMsg(Action("hi", Nil))
  }

  it should "fall back to a random move if all time out" in {
    val player = system.actorOf(Props(new Player(Seq(
      Props(new DelayMoveSelector(1.1.seconds))
    ))))

    player ! NewGame(game, "black", self, 3.seconds)
    expectMsg(Ready)

    player ! SelectMove(self, 3.seconds)
    expectMsg(Action("left", Nil))
  }
}

class DummyMoveSelector extends Actor {
  def receive = {
    case Initialize(game, role) => sender ! Initialized(())
    case Play(game, _, "black", _) => sender ! SelectedMove(Action("hi", Nil))
  }
}

class DelayMoveSelector(delay: FiniteDuration) extends Actor {
  def receive = {
    case Initialize(game, role) => sender ! Initialized(())
    case Play(game, _, "black", count) =>
      Thread.sleep(delay.toMillis)
      sender ! SelectedMove(Action("hello", Nil))
  }
}
