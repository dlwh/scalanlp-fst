package scalanlp.fst;
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


import scala.collection.mutable.ArrayBuffer;
import scalanlp.collection.mutable._;
import scalanlp.math._;
import math._;
import scalala.library.Numerics._;

/**
* KLMinimize will merge two states if the induced distribution of suffixes
* starting at those states are similar. (Or rather, if the minimization
* algorithm finds that they might be.)
*
* @author dlwh
*/
object KLMinimize {
  import Minimizer._;

  implicit def partitioner[State,T] = new KLPartitioner[State,T](3.0);

  class KLPartitioner[State,T](cutoff: Double) extends Partitioner[Double,State,T] {
    def repartition(partitions: Iterable[Partition[Double,State,T]]) = {
      val iter = partitions.iterator;

      val newPartitions = new ArrayBuffer[Partition[Double,State,T]];
      newPartitions += iter.next

      for( part1@(p1,states1) <- iter) {

        val (bestPartition:Int,score) = {
          (for( ((p2,states2),index) <- newPartitions.iterator.zipWithIndex) yield {
            val (kl,_) = computeKLDivergence(p1,p2);
            val (rKl,_) = computeKLDivergence(p2,p1);
            (index:Int,kl + rKl)
          }).foldLeft( (-1,cutoff) ) ( (best,next) =>
            if(next._2 <= best._2) next else best 
          )
        }

        // either keep this partition or merge it with the other one.
        if(score == cutoff) {
          newPartitions += part1;
        } else {
          val current = newPartitions(bestPartition);
          newPartitions(bestPartition) =  (current._1,current._2 ++ states1);
        }
      }
      newPartitions
    }
  }

  def computeKLDivergence[T](p1: EquivalenceInfo[Double,T], p2: EquivalenceInfo[Double,T]) = {
    // how much smoothing
    val extraMass1 = try { p1.arcs.valuesIterator.toSeq.min/10 } catch { case _ => math.log(1E-4) }
    val extraMass2 = try { p2.arcs.valuesIterator.toSeq.min/10 } catch { case _ => math.log(1E-4) };

    // how much total mass is there
    val p1Total = logSum(p1.finalWeight +: p1.arcs.valuesIterator.toSeq);
    val p2Total = logSum(p2.finalWeight +: p2.arcs.valuesIterator.toSeq);

    // smoothed total Mass
    val p1SmoothedTotal = logSum(logSum(p1.arcs.valuesIterator.toSeq),log(p1.arcs.size) +extraMass1);
    val p2SmoothedTotal = logSum(logSum(p2.arcs.valuesIterator.toSeq),log(p2.arcs.size)  + extraMass2);

    def logProb1Smooth(w1: Double) = {
      logSum(w1,extraMass1) - p1SmoothedTotal;
    }
    def logProb2Smooth(w2: Double) = {
      logSum(w2,extraMass2) - p2SmoothedTotal;
    }
    def logProb1(w1: Double) = {
      w1 - p1Total;
    }
    def logProb2(w2: Double) = {
      w2 - p2Total
    }

    def logRatio(w1:Double, w2: Double) = {
      logProb1(w1) - logProb2(w2);
    }

    def logRatioSmooth(w1:Double, w2: Double) = {
      logProb1Smooth(w1) - logProb2Smooth(w2);
    }

    var kl = math.exp(logProb1(p1.finalWeight)) * logRatio(p1.finalWeight,p2.finalWeight);
    var smoothedKL = math.exp(logProb1Smooth(p1.finalWeight)) * logRatioSmooth(p1.finalWeight,p2.finalWeight);
    for( (tuple,w1) <- p1.arcs;
      w2 = p2.arcs.getOrElse(tuple,-1.0/0.0)
    ) {
      kl += math.exp(logProb1(w1)) * (logProb1(w1) - logProb2(w2));
      smoothedKL += math.exp(logProb1Smooth(w1)) * (logProb1Smooth(w1) - logProb2Smooth(w2));
    }

    println("LLL" + kl);
    (kl,smoothedKL);
  }



}
