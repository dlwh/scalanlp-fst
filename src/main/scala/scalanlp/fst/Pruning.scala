package scalanlp.fst;

import scalanlp.math._;

/**
* Used to remove arcs from automata.
*/
object Pruning {
  /**
   *
   * For probabilistic automata (with partition 1) it's the score of posterior probability of being in any given state.
   *
   */
  def calculateStateFlow[W:Semiring:ClassManifest,State,T](auto: Automaton[W,State,T]): Map[State,W] = {
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
  def prune[W:Semiring:ClassManifest,State,T:Alphabet](auto: Automaton[W,State,T], belowThreshold: W=>Boolean): Automaton[W,State,T] = {
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
