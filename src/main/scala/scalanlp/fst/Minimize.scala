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


import scala.collection.mutable.HashMap;
import scala.collection.mutable.ArrayBuffer;


import scalanlp.math._
import util.MapMaker
import scalanlp.collection.mutable.{AutoUpdater, ArrayMap}
;

/**
* Minimization routines for automata.
*
* @author dlwh
*/
object Minimizer {
  // two states are potentially equivalent if they have the same follow set (nextStates)
  // and are equivalent if their final states are similar, and their individual
  // arcs have similar weights.
  case class EquivalenceInfo[W,T](finalWeight: W, arcs: HashMap[(Int,T),W]);

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
                              mm: MapMaker[Automaton[W,State,T],State,Int]) = {
    new MinimizeWorker[W,State,T](trans).minimize;
  }
  

  // Worker just prevents a lot of passing around of states and types.
  private class MinimizeWorker[W,State,T](trans: Automaton[W,State,T])
                                         (implicit partitioner: Partitioner[W,State,T],
                                          ring: Semiring[W],
                                          mm: MapMaker[Automaton[W,State,T],State,Int]) {
    private val edgesByOrigin = trans.edges.groupBy(_.source).mapValues(_.toIndexedSeq);

    def minimize = {
      // initialpartition:
      val partitions = new ArrayBuffer[Partition[W,State,T]];
      partitions ++= partition(trans.states.toSet, _ => 0);

      val partitionOf = AutoUpdater(mm.mkMap(trans),sys.error("??"));
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


      val initWeights = ArrayBuffer.fill(partitions.size)(ring.zero)
      val newFinalWeights = ArrayBuffer.fill(partitions.size)(ring.zero);
      type NewArc = Arc[W,Int,T];
      val arcs = Array.fill(partitions.size){Iterable.empty[NewArc]};

      for( (s,w) <- trans.initialStateWeights if w != ring.zero) {
        initWeights(partitionOf(s)) = ring.plus(initWeights(partitionOf(s)),w);
      }
      for( ((equivInfo,partition),index) <- partitions.zipWithIndex) {
        newFinalWeights(index) = equivInfo.finalWeight;
        arcs(index) = {for( ( (to,label),w) <- equivInfo.arcs iterator ) yield new NewArc(index,to,label,w)}.toIndexedSeq
      }
        
      new IntAutomaton(arcs, initWeights.toIndexedSeq,newFinalWeights.toIndexedSeq);
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
