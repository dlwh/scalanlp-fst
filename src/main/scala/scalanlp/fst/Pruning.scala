package scalanlp.fst;

import scalanlp.math._;

object Pruning {
  def calculateStateFlow[W:Semiring,State,In,Out](auto: Transducer[W,State,In,Out]): Map[State,W] = {
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

    combined withDefaultValue auto.ring.zero;
  }

  def prune[W:Semiring,State,In:Alphabet,Out:Alphabet](auto: Transducer[W,State,In,Out], belowThreshold: W=>Boolean): Transducer[W,State,In,Out] = {
    val zero = implicitly[Semiring[W]].zero;

    val fb = calculateStateFlow(auto);
    val validStates = Set.empty ++ { for( (s,w) <- fb if !belowThreshold(w)) yield s }
    val initWeights = auto.initialStateWeights.filter { case (k,v) => validStates(k) };
    val newFinalWeights = auto.finalStateWeights.filter { case(k,v) => validStates(k) };

    val newArcs = for( a@Arc(from,to,in,out,w) <- auto.allEdges
      if validStates(from) && validStates(to) ) 
      yield a;
      
    val imInitWeights = Map.empty ++ initWeights withDefaultValue(zero);
    val imFinalWeights = Map.empty ++ newFinalWeights withDefaultValue(zero);
    Transducer.transducer(imInitWeights,imFinalWeights)(newArcs:_*);
  }
}
