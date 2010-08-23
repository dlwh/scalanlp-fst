package scalanlp.fst

import scalanlp.math.Semiring

/**
 * Given a weighted automaton, computers the expected number of visits to states in another unweighted automaton.
 * This can be used for approximating one automaton (the weighted one), by another (the unweighted one), by computing
 * weights and normalizing appropriately.
 *
 *  @author dlwh
 */
object ExpectedCounts {
  def counts[@specialized(Double) W:Semiring, S1,S2,T:Alphabet](weighted: Automaton[W,S1,T],
                                                                template: Automaton[W,S2,T]) = {
    val (inter, index) = (weighted & template).relabelWithIndex;
    // compute posterior probability of being in any given state.
    val flow = Pruning.calculateStateFlow(inter);
    val ring = implicitly[Semiring[W]];
    val res = template.makeMap(ring.zero);
    for( (indexedState, score) <- flow) {
      res(index.get(indexedState)._2) = ring.plus(res(index.get(indexedState)._2), score)
    }

    res;
  }
}