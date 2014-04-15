package uk.co.mattthomson.coursera.ggp.gresley.gdl

class GameState(private val facts: Set[Fact]) {
  private def canEqual(other: Any): Boolean = other.isInstanceOf[GameState]

  override def equals(other: Any): Boolean = other match {
    case that: GameState =>
      (that canEqual this) &&
        facts == that.facts
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(facts)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
