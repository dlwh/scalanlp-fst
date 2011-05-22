package scalanlp.fst;
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



import scalanlp.math._
import scalala.collection.sparse.DefaultArrayValue
;

/**
* Used to remove arcs from automata.
*/
object Pruning {
  /**
   *
   * For probabilistic automata (with partition 1) it's the score of posterior probability of being in any given state.
   *
   */
  def calculateStateFlow[W:Semiring:ClassManifest:DefaultArrayValue,State,T](auto: Automaton[W,State,T]): Map[State,W] = {
    val ring = implicitly[Semiring[W]];
    val forward = Distance.singleSourceShortestDistances(auto);
    val backward = Distance.singleSourceShortestDistances(auto.reverse)
    val combined = for {
      (s,f) <- forward;
      if f != ring.zero
      b = backward.getOrElse(s,ring.zero)
      if b != ring.zero
    } yield {
      (s,ring.times(f,b));
    }

    combined withDefaultValue ring.zero;
  }

  /**
  * prunes arcs that are below some posterior probability/weight of being visited.
  */
  def prune[W:Semiring:ClassManifest:DefaultArrayValue,State,T:Alphabet](auto: Automaton[W,State,T], belowThreshold: W=>Boolean): Automaton[W,State,T] = {
    val zero = implicitly[Semiring[W]].zero;

    val fb = calculateStateFlow(auto);
    val validStates = Set.empty ++ { for( (s,w) <- fb if !belowThreshold(w)) yield s }
    val initWeights = auto.initialStateWeights.filter { case (k,v) => validStates(k) };
    val newFinalWeights = auto.finalStateWeights.filter { case(k,v) => validStates(k) };

    val newArcs = for( a@Arc(from,to,label,w) <- auto.allEdges
      if validStates(from) && validStates(to) ) 
      yield a;
      
    val imInitWeights = Map.empty ++ initWeights withDefaultValue(zero);
    val imFinalWeights = Map.empty ++ newFinalWeights withDefaultValue(zero);
    Automaton.automaton(imInitWeights,imFinalWeights)(newArcs:_*);
  }
}
