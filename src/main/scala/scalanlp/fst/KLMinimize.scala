package scalanlp.fst;

import scala.collection.mutable.ArrayBuffer;
import scalanlp.collection.mutable._;
import scalanlp.math._;
import Math._;
import Numerics._;

object KLMinimize {
  import Transducer._;
  import scala.collection.mutable.HashMap;
  case class EquivalenceInfo[In,Out](finalWeight: Double,arcs: HashMap[(Int,In,Out),Double]);

  def minimize[State,In,Out](auto: Transducer[Double,State,In,Out]): Transducer[Double,Int,In,Out] = {
    import auto._;
    val edgesByOrigin = allEdgesByOrigin;

    def aggregateArcs(arcs : Seq[Arc], destMapper: State=>Int) = {
      val edgeMap = new HashMap[(Int,In,Out),Double] {
        override def default(i: (Int,In,Out)) = ring.zero;
      }
      for( Arc(_,to,in,out,w) <- arcs) {
        edgeMap( (destMapper(to),in,out)) = ring.plus(edgeMap(destMapper(to),in,out),w);
      }
      edgeMap
    }

    // initialpartition:
    // two state are equivalent if they have equivalent outgoing arc
    // labels *and* their final weight is the same.
    val initialPartitions = allStates.groupBy{ state =>
      val myEdges = edgesByOrigin.getOrElse(state,Seq.empty);
      val edgesWithoutStates = aggregateArcs(myEdges,_ => 0);
      EquivalenceInfo(finalWeight(state),edgesWithoutStates)
    };
    val partitions = new ArrayBuffer[(EquivalenceInfo[In,Out],Set[State])];
    partitions ++= klMerge(initialPartitions.toSeq);

    val partitionOf:scala.collection.mutable.Map[State,Int] = makeMap(-1);
    for( (partition,index) <- partitions.zipWithIndex;
        state <- partition._2) {
      partitionOf(state) = index;
    }

    var changed = true;
    while(changed) {
      changed = false;
      for( ((oldEquiv,partition),index) <- partitions.zipWithIndex) {
        // try regrouping these edges by their follow sets
        val tryPartitions = partition.groupBy { state =>
          val myEdges = edgesByOrigin.getOrElse(state,Seq.empty).view;
          val edgesWithoutStates = aggregateArcs(myEdges,partitionOf );
          EquivalenceInfo(finalWeight(state),edgesWithoutStates);
        }
        assert(tryPartitions.size > 0);

        val newPartitions = klMerge(tryPartitions.toSeq);

        // if it changed, move the partitions
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
      _ = assert(equivInfo.finalWeight == finalWeight(s));
      stuff@((toPart,in,out),w) <- equivInfo.arcs
    } {
      var mass = ring.zero;
      for( Arc(_,to,_,_,myW) <- edgesMatching(s,in,out)) {
        if(partitions(toPart)._2.contains(to))
          mass = ring.plus(mass,myW);
      }
      assert(mass == w,(s,stuff,mass,w).toString);
    }
    */


    val initWeights = new HashMap[Int,Double] {
      override def default(i: Int) = ring.zero;
    };
    val newFinalWeights = new HashMap[Int,Double] {
      override def default(i: Int) = ring.zero;
    };
    type NewArc = scalanlp.fst.Arc[Double,Int,In,Out];
    val arcs = new ArrayBuffer[NewArc];

    for( (s,w) <- initialStateWeights) {
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
    intTransducer(imInitWeights,imFinalWeights)(arcs:_*);
  }

  def computeKLDivergence[In,Out](p1: EquivalenceInfo[In,Out], p2: EquivalenceInfo[In,Out]) = {
    val extraMass1 = p1.arcs.valuesIterator.toSeq.min/10;
    val extraMass2 = p2.arcs.valuesIterator.toSeq.min/10;
    val p1Total = logSum(p1.arcs.valuesIterator.toSeq);
    val p2Total = logSum(p2.arcs.valuesIterator.toSeq);
    val p1SmoothedTotal = logSum(logSum(p1.arcs.valuesIterator.toSeq),log(p1.arcs.size) +extraMass1);
    val p2SmoothedTotal = logSum(logSum(p2.arcs.valuesIterator.toSeq),log(p2.arcs.size)  + extraMass2);

    def lp1Smooth(w1: Double) = {
      logSum(w1,extraMass1) - p1SmoothedTotal;
    }
    def lp2Smooth(w2: Double) = {
      logSum(w2,extraMass2) - p2SmoothedTotal;
    }
    def lp1(w1: Double) = {
      w1 - p1Total;
    }
    def lp2(w2: Double) = {
      w2 - p2Total
    }
    var kl = 0.0;
    var smoothedKL = 0.0;
    for( (tuple,w1) <- p1.arcs;
      w2 = p2.arcs.getOrElse(tuple,-1.0/0.0)
    ) {
      kl += Math.exp(lp1(w1)) * (lp1(w1) - lp2(w2));
      smoothedKL += Math.exp(lp1Smooth(w1)) * (lp1Smooth(w1) - lp2Smooth(w2));
    }

    (kl,smoothedKL);
  }


  val ARBITRARY_CUTOFF = 1.0/0.0;

  def klMerge[State,In,Out](partitions: Seq[(EquivalenceInfo[In,Out],Set[State])]) = {
    val newPartitions = new ArrayBuffer[(EquivalenceInfo[In,Out],Set[State])];
    newPartitions += partitions(0);
    for( part1@(p1,states1) <- partitions) {

      val (bestPartition:Int,score) = {
        (for( ((p2,states2),index) <- newPartitions.iterator.zipWithIndex) yield {
          val (kl,_) = computeKLDivergence(p1,p2);
          val (rKl,_) = computeKLDivergence(p2,p1);
          (index:Int,kl + rKl)
        }).foldLeft( (-1,ARBITRARY_CUTOFF) ) ( (best,next) =>
          if(next._2 <= best._2) next else best 
        )
      }

      // either keep this partition or merge it with the other one.
      if(score.isInfinite) {
        newPartitions += part1;
      } else {
        val current = newPartitions(bestPartition);
        newPartitions(bestPartition) =  (current._1,current._2 ++ states1);
      }
    }
    newPartitions
  }


  def printKLDivergences[State,In,Out](partitions: Seq[(EquivalenceInfo[In,Out],Set[State])]) = {
    for( (p1,states1) <- partitions;
      (p2,states2) <- partitions
      if states1 != states2
    ) {
      println(states1 + " cross " + states2);
      println(p1 + " vs. " + p2);
      val (kl,smoothedKL) = computeKLDivergence(p1,p2);

      println("Raw: " + kl + "  Smoothed: "+ smoothedKL);
      println();
      println();
    }
  }

}
