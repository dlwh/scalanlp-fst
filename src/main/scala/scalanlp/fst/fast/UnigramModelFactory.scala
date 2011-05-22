package scalanlp.fst.fast

/**
 * 
 * @author dlwh
 */

trait UnigramModelFactory[T] { this: AutomatonFactory[T] =>
 class UnigramModel extends Automaton {
    override def finalWeight(s: Int) = ring.one

    lazy val finalWeights = Array.fill(numStates) {ring.one}

    def initialWeight = ring.one

    def initialState = 0;

    def numStates = 1;

    val theArcs = encoder.fillSparseArrayMap(mkSparseVector(numStates));
    for(i <- 0 until index.size if i != epsilonIndex) {
      theArcs.getOrElseUpdate(i)(0) = ring.one;
    }

    def arcsFrom(s: Int, ch: Int) = {
      theArcs(ch);
    }

    def arcsFrom(s: Int) = {
      theArcs;
    }

  }
}