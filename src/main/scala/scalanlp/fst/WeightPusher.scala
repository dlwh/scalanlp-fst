package scalanlp.fst

import util.MapMaker
import scalanlp.math.{Semiring, WLDSemiring}

/**
 * 
 * @author dlwh
 */

trait WeightPusher[-CC,+CC2] extends AutomatonTransformer[CC,CC2]

object WeightPusher {
  implicit def basicWeightPusher[CC,CCR, W,State,T](implicit reverser: Reverser[CC,CCR],
                                                    distance: Distance[CCR,W,State],
                                                    ev: CC<:<Automaton[W,State,T],
                                                    evR: CCR<:<Automaton[W,State,T],
                                                    ring: WLDSemiring[W]):  WeightPusher[CC,Automaton[W,State,T]] = {
    new WeightPusher[CC,Automaton[W,State,T]] {
      def apply(auto: CC) = {
        val autoCC = ev(auto);
        import autoCC._;
        import ring._;
        val rev : CCR = reverser(auto);
        val costs = distance.allPathDistances(rev); // \sum_{state q in final} weight(path(p,q))
        val initWeights = initialStateWeights map { case (k,v) => (k,times(v,costs(k))) } withDefaultValue (ring.zero)
        val finalWeights = {for( (s,w) <- rev.initialStateWeights;
                                d = costs(s);
                                if d != zero)
        yield (s,leftDivide(d,w))} withDefaultValue (ring.zero)

        // re-reverse and reweight
        val arcs = {
          for(Arc(to,from,label,w) <- rev.edges;
              d = costs(from);
              if d != zero)
          yield Arc(from,to,label,leftDivide(d,times(w,costs(to))));
        }

        Automaton.automaton(initWeights,finalWeights)(arcs.toIndexedSeq:_*);

      }
    }

  }
}
