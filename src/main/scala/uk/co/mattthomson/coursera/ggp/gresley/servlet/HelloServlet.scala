package uk.co.mattthomson.coursera.ggp.gresley.servlet

import org.scalatra.ScalatraServlet

class HelloServlet extends ScalatraServlet {
  get("/") {
    "Hello, my name is Gresley."
  }
}
