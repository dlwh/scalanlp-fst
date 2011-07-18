package scalanlp.fst

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
import scalanlp.collection.mutable.AutoUpdater
import collection.mutable

/**
 * Given a weighted automaton, computes the expected number of visits to states in another unweighted automaton.
 * This can be used for approximating one automaton (the weighted one), by another (the unweighted one), by computing
 * weights and normalizing appropriately.
 *
 *  @author dlwh
 */
object ExpectedCounts {
  def counts[W, S1,S2,T](weighted: Automaton[W,S1,T],
                         template: Automaton[W,S2,T])(implicit ring: Semiring[W], alphabet: Alphabet[T]) = {
    val inter = (weighted & template);
    // compute posterior probability of being in any given state.
    val forward = Distance.allPathDistances(inter)
    val backward = Distance.allPathDistances(inter.reverse)

    // TODO; make this fast, and not ugly.
    type TMap = AutoUpdater[mutable.Map[T,W],T,W];
    type S2TMap = AutoUpdater[mutable.Map[S2,TMap],S2,TMap];
//    type S2S2TMap = AutoUpdater[mutable.Map[S2,S2TMap],S2,S2TMap];

    // S2 -> S2 -> T -> W
    val scores = AutoUpdater[collection.mutable.Map[S2,S2TMap],S2,S2TMap](collection.mutable.Map(),
                                        AutoUpdater(collection.mutable.Map(),
                                                    AutoUpdater(collection.mutable.Map[T,W](),ring.zero)))

    inter.edges foreach { case Arc(from, to, label, weight) =>
      val srcIndex = from._2;
      val sinkIndex = to._2;
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

    scores.mapValues(_.theMap.mapValues(_.theMap));
  }
}

