package scalanlp.fst

import scalanlp.math.Semiring
import collection.mutable.HashMap

/**
 * Given a weighted automaton, computes the expected number of visits to states in another unweighted automaton.
 * This can be used for approximating one automaton (the weighted one), by another (the unweighted one), by computing
 * weights and normalizing appropriately.
 *
 *  @author dlwh
 */
object ExpectedCounts {
  def counts[W:Semiring:ClassManifest, S1,S2,T:Alphabet](weighted: Automaton[W,S1,T],
                                                                template: Automaton[W,S2,T]) = {
    import ApproximatePartitioner._;
    val (indexedTemplate,templateIndex) =  template.relabelWithIndex;
    val (inter, index) = (Minimizer.minimize(weighted) & indexedTemplate).relabelWithIndex;
    // compute posterior probability of being in any given state.
    val forward = Distance.allPathDistances(inter);
    val backward = Distance.allPathDistances(inter.reverse)
    val ring = implicitly[Semiring[W]];

    val scores = Array.fill(templateIndex.size, templateIndex.size)(new HashMap[T,W] {
      override def default(k: T) = ring.zero;
    });

    inter.breadthFirstSearch{ case Arc(from, to, label, weight) =>
      val (_, srcIndex, _) = index.get(from);
      val (_, sinkIndex, _) = index.get(to);
      for {
        fScore <- forward.get(from);
        bScore <- backward.get(to)
      } {
        if(weight != ring.zero && fScore != ring.zero && bScore != ring.zero) {
          val posterior = ring.times(fScore,ring.times(weight,bScore));
          scores(srcIndex)(sinkIndex)(label) = ring.plus(scores(srcIndex)(sinkIndex)(label), posterior);
        }
      }
    }

    val res = new HashMap[(S2,S2,T), W] {
      override def initialSize = 2 * templateIndex.size * templateIndex.size * 20;
    }
    for(i <- 0 until templateIndex.size; j <- 0 until templateIndex.size) {
      val src = templateIndex.get(i);
      val sink = templateIndex.get(j);
      val map = scores(i)(j);
      for( (t,w) <- map)
        res((src,sink,t)) = w;
    }

    res;

  }
}