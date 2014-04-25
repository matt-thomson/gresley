package uk.co.mattthomson.coursera.ggp.gresley.player.heuristic

import uk.co.mattthomson.coursera.ggp.gresley.gdl.GameState

class BoundedDepthSearchPlayer(depthLimit: Int) extends HeuristicSearchPlayer(depthLimit) {
  override protected def intermediateStateValue(state: GameState, role: String) = state.value(role)
}
