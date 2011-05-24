package scalanlp.newfst

import scalanlp.math.WLDSemiring
import scalanlp.collection.mutable.AutoUpdater

/**
 * 
 * @author dlwh
 */
trait Determinizer[-CC,+CC2] extends AutomatonTransformer[CC,CC2] {
  def apply(cc: CC):CC2
}

object Determinizer {
  implicit def basicDeterminizer[W:WLDSemiring,S,T]:Determinizer[Automaton[W,S,T],Automaton[W,Map[S,W],T]] = {
    new Determinizer[Automaton[W,S,T],Automaton[W,Map[S,W],T]] {
      val ring = implicitly[WLDSemiring[W]];
      import ring._;
      def apply(outer: Automaton[W, S, T]) = new Automaton[W,Map[S,W],T] {
        val initialStateWeights = {
          Map(outer.initialStateWeights -> ring.one);
        }

        def finalWeight(s: Map[S, W]) = {
          val weights = for { (state,v) <- s.iterator } yield ring.times(v,outer.finalWeight(state));
          weights.foldLeft(ring.zero)(ring.plus);
        }

        def edgesFrom(map: Map[S, W]) = {
          import collection.mutable._;
          val labeledWeights = Map[T,W]();
          val labeledStates = AutoUpdater(Map[T, Map[S,W]](),Map[S,W]());

          for((s,v) <- map;
              Arc(_,to,label,w) <- outer.edgesFrom(s)) {
            // sum over all the different ways to follow arc with label label to state t
            val newTo = labeledStates(label)
            val cur = newTo.getOrElse(to,zero);
            newTo(to) = plus(cur,times(w,v));

            val wcur = labeledWeights.getOrElse(label,zero);
            labeledWeights(label) = plus(wcur,times(w,v));
          }

          // normalize by w
          val arcs = for((label,newState) <- labeledStates.iterator;
                         w = labeledWeights(label)) yield {
            newState.transform { (innerState,v) =>
              leftDivide(w,v);
            }
            Arc(map, collection.immutable.Map() ++ newState,label,w);
          }

          arcs
        }
      }
    }
  }
}