package scalanlp.fst.fast

/*
 Copyright 2010 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/



import scalanlp.math.Semiring
import collection.mutable.HashMap

/**
 * Given a weighted automaton, computes the expected number of visits to states in another unweighted automaton.
 * This can be used for approximating one automaton (the weighted one), by another (the unweighted one), by computing
 * weights and normalizing appropriately.
 *
 *  @author dlwh
 */
trait ExpectedCounts[T] { this: AutomatonFactory[T] =>
  def expectedCounts(weighted: Automaton, template: Automaton) = {
    val inter = weighted & template;
    // compute posterior probability of being in any given state.
    val forward = allPathDistances(inter);
    val backward = allPathDistances(inter.reverse)

    val scores = Array.fill(template.numStates, template.numStates)(encoder.mkSparseHashVector(ring.zero));

    breadthFirstSearch(inter) { (from, to, label, weight) =>
      val srcIndex = inter.underlyingRightState(from);
      val sinkIndex = inter.underlyingRightState(to);
      val fScore = forward(from)
      val bScore = backward(to)
      if(weight != ring.zero && fScore != ring.zero && bScore != ring.zero) {
        val posterior = ring.times(fScore,ring.times(weight,bScore));
        scores(srcIndex)(sinkIndex)(label) = ring.plus(scores(srcIndex)(sinkIndex)(label), posterior);
      }
    }

    scores;
  }
}
