package scalanlp.fst

import scalanlp.math.WLDSemiring
import scalanlp.collection.mutable.AutoUpdater
import collection.immutable.BitSet

/**
 * 
 * @author dlwh
 */
trait Determinizer[-CC,+CC2] extends AutomatonTransformer[CC,CC2] {
  def apply(cc: CC):CC2
}

object Determinizer {
  implicit def bitsetDeterminizer[T] = new Determinizer[Automaton[Boolean,Int,T],Automaton[Boolean,BitSet,T]] {
    def apply(cc: Automaton[Boolean, Int, T]) = new Automaton[Boolean,BitSet,T] {
      val initialStateWeights = Map(BitSet() ++ cc.initialStateWeights.filter(_._2).keys -> true);

      def finalWeight(s: BitSet) = {
        s.exists(cc.finalWeight _)
      }

      def edgesFrom(map: BitSet) = {
        import collection.mutable._;
        val edgesByDestination =  AutoUpdater(Map[T,scala.collection.mutable.BitSet](),collection.mutable.BitSet());
        for(s <- map; Arc(_,to,label,w) <- cc.edgesFrom(s) if w) {
          edgesByDestination(label) += to;
        }
        for( (label,to) <- edgesByDestination.iterator) yield Arc(map, (collection.immutable.BitSet() ++ to): collection.immutable.BitSet, label, true);
      }
    }
  }


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