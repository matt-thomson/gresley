package uk.co.mattthomson.coursera.ggp.gresley.servlet

import org.scalatra.test.scalatest.ScalatraFlatSpec
import org.scalatest.flatspec._
import org.scalatest.matchers._
import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.ActorSystem

class GresleyServletSpec extends TestKit(ActorSystem("TestActorSystem")) with ScalatraFlatSpec with should.Matchers with ImplicitSender {
  addServlet(new GresleyServlet(system, null), "/")

  "The servlet" should "say hello" in {
    get("/") {
      status should equal (200)
      body should include ("Hello")
    }
  }

  it should "parse protocol messages and send to the manager" in {
    post("/", body = "(info)") {
      status should equal (200)
      body should be ("((name gresley) (status available))")
    }
  }

  it should "cope with messages in form params" in {
    post("/", body = "(info)", headers = Map("Content-Type" -> "application/x-www-form-urlencoded")) {
      status should equal (200)
      body should be ("((name gresley) (status available))")
    }
  }

  it should "handle empty messages" in {
    post("/") {
      status should equal (204)
    }
  }
}
