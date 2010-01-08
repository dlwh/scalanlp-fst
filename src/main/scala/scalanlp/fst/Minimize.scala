package scalanlp.fst;

import scala.collection.mutable.HashMap;
import scala.collection.mutable.ArrayBuffer;
import scalanlp.collection.mutable.ArrayMap;

import scalanlp.math._;

object Minimizer {
  // two states are potentially equivalent if they have the same follow set (nextStates)
  // and are equivalent if their final states are similar, and their individual
  // arcs have similar weights.
  case class EquivalenceInfo[W,T](val finalWeight: W, val arcs: HashMap[(Int,T),W]);

  type Partition[W,State,T] = (EquivalenceInfo[W,T],Set[State]);

  trait Partitioner[W,State,T] {
    /**
    * Given a set of partitions, split or merge these partitions into better partitions.
    */
    def repartition(partitions: Iterable[Partition[W,State,T]]): Seq[Partition[W,State,T]];
  }

  /**
  * Performs minimization of the autmaton. This will only minimize deterministic
  * automata, but for NFAs it does the "right thing", handling ambiguous transitions
  * in a reasonable manner.
  *
  * This is the labeled generalization of Ullman's 1967 algorithm to weighted
  * automata with extra support for weighted NFAs.
  */
  def minimize[W,State,T](trans: Automaton[W,State,T])
                              (implicit ring: Semiring[W],
                              partitioner: Partitioner[W,State,T],
                              alphabet: Alphabet[T]) = {
    new MinimizeWorker[W,State,T](trans)(partitioner,ring,alphabet).minimize;
  }
  

  // Worker just prevents a lot of passing around of states and types.
  private class MinimizeWorker[W,State,T](trans: Automaton[W,State,T])
                                              (implicit partitioner: Partitioner[W,State,T],
                                                ring: Semiring[W],
                                                alpha: Alphabet[T]) {
    private val edgesByOrigin = trans.allEdgesByOrigin;

    def minimize = {
      // initialpartition:
      val partitions = new ArrayBuffer[Partition[W,State,T]];
      partitions ++= partition(Set.empty ++ edgesByOrigin.keysIterator,_ => 0);

      val partitionOf:scala.collection.mutable.Map[State,Int] = trans.makeMap(-1);
      for( (partition,index) <- partitions.zipWithIndex;
          state <- partition._2) {
        partitionOf(state) = index;
      }

      var changed = true;
      while(changed) {
        changed = false;
        for( ((oldEquiv,part),index) <- partitions.zipWithIndex) {
          // try regrouping these edges by their follow sets
          val newPartitions = partition(part,partitionOf);

          // if the partitions changed, move the partitions
          if(newPartitions.size > 1) {
            //println("split" + index + partition + newPartitions);
            changed = true;
            // leave one partition where it is.
            val iter = newPartitions.iterator;
            partitions(index) = iter.next;

            // update other partitions to end of array
            for( pair@(equiv,p) <- iter) {
              val newIndex = partitions.size;
              partitions += pair;
              //println(partitions,newIndex);
              for(s <- p) {
                partitionOf(s) = newIndex;
              }
            }
          } else {
            // in case any of the other partitions split, update
            // this will always execute on the last run.
            partitions(index) = newPartitions.iterator.next;
          }
        }
      }

      //println(partitions.zipWithIndex);

      /* post condition check:
      for{ 
        ((equivInfo,partition),index) <- partitions.zipWithIndex
        s <- partition
        _ = assert(equivInfo._1 == finalWeight(s));
        stuff@((toPart,in,out),w) <- equivInfo._2
      } {
        var mass = ring.zero;
        for( Arc(_,to,_,_,myW) <- edgesMatching(s,in,out)) {
          if(partitions(toPart)._2.contains(to))
            mass = ring.plus(mass,myW);
        }
        assert(mass == w,(s,stuff,mass,w).toString);
      }
      */


      val initWeights = new ArrayMap[W] {
        override def defValue = ring.zero;
      };
      val newFinalWeights = new ArrayMap[W] {
        override def defValue = ring.zero;
      };
      type NewArc = scalanlp.fst.Arc[W,Int,T];
      val arcs = new ArrayBuffer[NewArc];

      for( (s,w) <- trans.initialStateWeights) {
        initWeights(partitionOf(s)) = ring.plus(initWeights(partitionOf(s)),w);
      }
      for( ((equivInfo,partition),index) <- partitions.zipWithIndex) {
        newFinalWeights(index) = equivInfo.finalWeight;
        for( ( (to,label),w) <- equivInfo.arcs) {
          arcs += new NewArc(index,to,label,w);
        }
      }
        
      val imInitWeights = Map.empty ++ initWeights withDefaultValue(ring.zero);
      val imFinalWeights = Map.empty ++ newFinalWeights withDefaultValue(ring.zero);
      Automaton.intAutomaton(imInitWeights,imFinalWeights)(arcs:_*);
    }

    private def partition(states: Set[State], parter: State=>Int) = {
      val partitions = states.groupBy{ state =>
        val myEdges = edgesByOrigin.getOrElse(state,Seq.empty);
        val edgesWithoutStates = aggregateArcs(myEdges,parter);
        EquivalenceInfo(trans.finalWeight(state),edgesWithoutStates);
      };

      partitioner.repartition(partitions);
    }

    private def aggregateArcs(arcs : Seq[Arc[W,State,T]], destMapper: State=>Int) = {
      val edgeMap = new HashMap[(Int,T),W]() {
        override def initialSize = arcs.length * 2;
        override def default(i: (Int,T)) = ring.zero;
      }
      for( Arc(_,to,label,w) <- arcs) {
        edgeMap( (destMapper(to),label)) = ring.plus(edgeMap(destMapper(to),label),w);
      }
      edgeMap
    }
  }

}

import Minimizer._;

object IdentityPartitioner {
  implicit def partitioner[W,State,T] = new IdentityPartitioner[W,State,T];

  class IdentityPartitioner[W,State,T] extends Partitioner[W,State,T] {
    def repartition(partitions: Iterable[Partition[W,State,T]]) = partitions.toSeq;
  }
}

/**
* Remerges states that the semiring isCloseTo returns true for.
*/
object ApproximatePartitioner {

  implicit def partitioner[W:Semiring,State,T] = new ApproxPartitioner[W,State,T];

  class ApproxPartitioner[W:Semiring,State,T] extends Partitioner[W,State,T] {
    val ring = implicitly[Semiring[W]];
    import ring._;
    def repartition(partitions: Iterable[Partition[W,State,T]]) = {
      val iter = partitions.iterator;

      val newPartitions = new ArrayBuffer[Partition[W,State,T]];
      newPartitions += iter.next

      for( part1@(p1,states1) <- iter) {
        val index = newPartitions.indexWhere { case (p2,_) =>
          closeTo(p1.finalWeight,p2.finalWeight) && p1.arcs.forall { case (arc,w) => 
            closeTo(w,p2.arcs(arc))
          }
        }
        if(index == -1) {
          newPartitions += part1;
        } else {
          val (equiv,states2) = newPartitions(index);
          newPartitions(index) =  (equiv,states2 ++ states1);
        }
      }

      newPartitions
    }
  }


}
