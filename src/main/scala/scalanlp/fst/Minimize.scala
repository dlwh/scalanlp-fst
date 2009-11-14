package scalanlp.fst;

import scala.collection.mutable.HashMap;
import scala.collection.mutable.OpenHashMap;
import scala.collection.mutable.ArrayBuffer;
import scalanlp.collection.mutable.ArrayMap;

import scalanlp.math._;

object Minimizer {
  // two states are potentially equivalent if they have the same follow set (nextStates)
  // and are equivalent if their final states are similar, and their individual
  // arcs have similar weights.
  case class EquivalenceInfo[W,In,Out](val finalWeight: W, val arcs: OpenHashMap[(Int,In,Out),W]);

  type Partition[W,State,In,Out] = (EquivalenceInfo[W,In,Out],Set[State]);

  trait Partitioner[W,State,In,Out] {
    /**
    * Given a set of partitions, split or merge these partitions into better partitions.
    */
    def repartition(partitions: Iterable[Partition[W,State,In,Out]]): Seq[Partition[W,State,In,Out]];
  }

  /**
  * Performs minimization of the autmaton. This will only minimize deterministic
  * automata, but for NFAs it does the "right thing", handling ambiguous transitions
  * in a reasonable manner.
  *
  * This is the labeled generalization of Ullman's 1967 algorithm to weighted
  * automata with extra support for weighted NFAs.
  */
  def minimize[W,State,In,Out](trans: Transducer[W,State,In,Out])
                              (implicit ring: Semiring[W],
                              partitioner: Partitioner[W,State,In,Out]): Transducer[W,Int,In,Out] = {
    new MinimizeWorker(trans).minimize;
  }
  

  // Worker just prevents a lot of passing around of states and types.
  private class MinimizeWorker[W,State,In,Out](trans: Transducer[W,State,In,Out])
                                              (implicit partitioner: Partitioner[W,State,In,Out],
                                                ring: Semiring[W]) {
    private val edgesByOrigin = trans.allEdgesByOrigin;

    def minimize = {
      // initialpartition:
      val partitions = new ArrayBuffer[Partition[W,State,In,Out]];
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
      type NewArc = scalanlp.fst.Arc[W,Int,In,Out];
      val arcs = new ArrayBuffer[NewArc];

      for( (s,w) <- trans.initialStateWeights) {
        initWeights(partitionOf(s)) = ring.plus(initWeights(partitionOf(s)),w);
      }
      for( ((equivInfo,partition),index) <- partitions.zipWithIndex) {
        newFinalWeights(index) = equivInfo.finalWeight;
        for( ( (to,in,out),w) <- equivInfo.arcs) {
          arcs += new NewArc(index,to,in,out,w);
        }
      }
        
      val imInitWeights = Map.empty ++ initWeights withDefaultValue(ring.zero);
      val imFinalWeights = Map.empty ++ newFinalWeights withDefaultValue(ring.zero);
      Transducer.intTransducer(imInitWeights,imFinalWeights)(arcs:_*)(ring,trans.inAlpha,trans.outAlpha);
    }

    private def partition(states: Set[State], parter: State=>Int) = {
      val partitions = states.groupBy{ state =>
        val myEdges = edgesByOrigin.getOrElse(state,Seq.empty);
        val edgesWithoutStates = aggregateArcs(myEdges,parter);
        EquivalenceInfo(trans.finalWeight(state),edgesWithoutStates);
      };

      partitioner.repartition(partitions);
    }

    private def aggregateArcs(arcs : Seq[Arc[W,State,In,Out]], destMapper: State=>Int) = {
      val edgeMap = new OpenHashMap[(Int,In,Out),W](arcs.length * 2) {
        override def default(i: (Int,In,Out)) = ring.zero;
      }
      for( Arc(_,to,in,out,w) <- arcs) {
        edgeMap( (destMapper(to),in,out)) = ring.plus(edgeMap(destMapper(to),in,out),w);
      }
      edgeMap
    }
  }

}

import Minimizer._;

object IdentityPartitioner {
  implicit def partitioner[W,State,In,Out] = new IdentityPartitioner[W,State,In,Out];

  class IdentityPartitioner[W,State,In,Out] extends Partitioner[W,State,In,Out] {
    def repartition(partitions: Iterable[Partition[W,State,In,Out]]) = partitions.toSeq;
  }
}

/**
* Remerges states that the semiring isCloseTo returns true for.
*/
object ApproximatePartitioner {

  implicit def partitioner[W:Semiring,State,In,Out] = new ApproxPartitioner[W,State,In,Out];

  class ApproxPartitioner[W:Semiring,State,In,Out] extends Partitioner[W,State,In,Out] {
    val ring = implicitly[Semiring[W]];
    import ring._;
    def repartition(partitions: Iterable[Partition[W,State,In,Out]]) = {
      val iter = partitions.iterator;

      val newPartitions = new ArrayBuffer[Partition[W,State,In,Out]];
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
