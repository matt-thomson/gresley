package uk.co.mattthomson.coursera.ggp.gresley.player.heuristic

import uk.co.mattthomson.coursera.ggp.gresley.gdl.GameState

class MobilityHeuristicSearchPlayer(depthLimit: Int) extends HeuristicSearchPlayer(depthLimit) {
  override protected def intermediateStateValue(state: GameState, role: String): Int = state.legalActions(role).size
}
