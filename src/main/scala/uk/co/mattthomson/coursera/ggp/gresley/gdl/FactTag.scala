package uk.co.mattthomson.coursera.ggp.gresley.gdl

trait FactTag

case class SimpleFactTag(c: Class[_ <: Fact]) extends FactTag

case class NamedFactTag(c: Class[_ <: Fact], name: String) extends FactTag

object FactTag {
  implicit def apply(c: Class[_ <: Fact]): FactTag = SimpleFactTag(c)
}