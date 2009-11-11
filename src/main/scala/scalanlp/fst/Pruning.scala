package scalanlp.fst;

import scalanlp.math._;

object Pruning {
  def calculateStateFlow[W,State,In,Out](auto: Transducer[W,State,In,Out]): Map[State,W] = {
    val forward = auto.allPathDistances;
    val backward = auto.reverse.allPathDistances;
    val combined = for {
      (s,f) <- forward;
      if f != auto.ring.zero
      b = backward.getOrElse(s,auto.ring.zero)
      if b != auto.ring.zero
    } yield {
      (s,auto.ring.times(f,b));
    }

    combined withDefaultValue auto.ring.zero;
  }

  def prune[W,State,In,Out](auto: Transducer[W,State,In,Out], belowThreshold: W=>Boolean): Transducer[W,State,In,Out] = {
    val fb = calculateStateFlow(auto);
    val validStates = Set.empty ++ { for( (s,w) <- fb if !belowThreshold(w)) yield s }
    val initWeights = auto.initialStateWeights.filter { case (k,v) => validStates(k) };
    val newFinalWeights = auto.finalStateWeights.filter { case(k,v) => validStates(k) };

    val newArcs = for( a@Arc(from,to,in,out,w) <- auto.allEdges
      if validStates(from) && validStates(to) ) 
      yield a;
      
    val imInitWeights = Map.empty ++ initWeights withDefaultValue(auto.ring.zero);
    val imFinalWeights = Map.empty ++ newFinalWeights withDefaultValue(auto.ring.zero);
    import auto._;
    Transducer.transducer(imInitWeights,imFinalWeights)(newArcs:_*)(auto.ring,inAlpha,outAlpha);
  }
}
