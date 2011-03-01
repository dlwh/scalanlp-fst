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

import scalanlp.math._;
import scala.reflect.OptManifest;
import scala.runtime.ScalaRunTime;
import scalala.Scalala._;
import scalala.tensor.adaptive._;
import scalala.tensor.sparse._;
import scalala.tensor.dense._;
import scalanlp.collection.mutable.ArrayMap;
import scalala.tensor.counters.LogCounters.{logSum=>_,_};
import java.util.Arrays


import scalanlp.util.Index
import it.unimi.dsi.fastutil.chars.Char2IntOpenHashMap
import collection.mutable.{ArrayBuffer, Seq => MSeq}

class CharIndex(beginningUnigram: Char, chars:Set[Char]) extends Index[Char] {
  val map = new Char2IntOpenHashMap(chars.size + 1);
  map.defaultReturnValue(-1);
  val objects = new ArrayBuffer[Char](chars.size + 1);
  var _size = 0;
  map.put(beginningUnigram, _size);
  objects += beginningUnigram;
  _size += 1;
  for(c <- chars) {
    if(map.put(c, _size) == -1) {
      objects += c
      _size += 1;
    }
  }

  override def size = _size;

  def iterator = objects.iterator

  def pairs = (objects zipWithIndex).iterator

  def unapply(i: Int) = Some(objects(i));

  def apply(t: Char) = map.get(t);
}

/*
 * Encodes the sufficient statistics for an automaton that has p(character|position) up to some max length.
 *
 * @param acceptableTs : only learn bigram histories that contain (only) these chars
 * @param acceptableBigrams: only learn bigrams histories that are these bigrams.
 */
class PositionalUnigramSemiring(maxPosition: Int, chars: Set[Char], beginningUnigram: Char, cheatOnEquals: Boolean=false)
                                                       (implicit alpha: Alphabet[Char], man: OptManifest[Char]) {

  case class Elem(counts: MSeq[AdaptiveVector], positionScores: Array[Double], totalProb: Double) {
    def decode: Seq[LogDoubleCounter[Char]] = {
      val result = counts.map { v => aggregate(v.activeElements.map { (iv:(Int,Double)) => (charIndex.get(iv._1),iv._2) } )};
      result;
    }
  }

  val charIndex = new CharIndex(beginningUnigram, chars);
  val beginningUnigramId = 0;

  private def mkAdaptiveVector = {
    val r = new AdaptiveVector(charIndex.size);
    r.default = Double.NegativeInfinity;
    r
  }

  private def logAddInPlace2D(to: MSeq[AdaptiveVector], from: MSeq[AdaptiveVector], scale: Double=0.0) {
    var i = 0;
    while(i < maxPosition) {
      if(from(i) != null) {
        if(to(i) == null) {
          to(i) = from(i) + scale;
        } else {
          logAddInPlace(to(i),from(i),scale);
        }
      }
      i +=1 ;
    }
  }


  private def logAdd(to: AdaptiveVector,  from: AdaptiveVector, scale: Double=0.0) = {
    val ret = to.copy;
    logAddInPlace(ret,from,scale);
    ret;
  }

  private def logAddInPlace(to: AdaptiveVector, from: AdaptiveVector, scale: Double=0.0) {
    if (scale != Double.NegativeInfinity) {
      from.innerVector match {
        case from: SparseVector =>
          var offset = 0;
          // this just iterates over the array, but we can't pay the boxing penalty
          while(offset < from.used) {
            val k = from.index(offset);
            val v = from.data(offset);
            to(k) = logSum(to(k),v+scale);
            offset += 1;
          }
          //println(from.used * 1.0 / from.size);
        case from: DenseVector =>
          to.densify();
          var offset = 0;
          while(offset < from.size) {
            val k = offset
            val v = from(offset);
            if(v != Double.NegativeInfinity) to(k) = logSum(to(k),v+scale);
            offset += 1;
          }
      }
    }
  }

  private def mnorm(x: AdaptiveVector, y: AdaptiveVector): Boolean = (x == null && y == null) || {
    var i = 0;
    if(x != null && y != null) {
      while(i < x.size) {
        if(!Semiring.LogSpace.doubleIsLogSpace.closeTo(x(i),y(i))) return false
        i += 1;
      }
    } else if(x != null && y == null) {
      while(i < x.size) {
        if(!Semiring.LogSpace.doubleIsLogSpace.closeTo(x(i),Double.NegativeInfinity)) return false
        i += 1;
      }
    } else {
      while(i < y.size) {
        if(!Semiring.LogSpace.doubleIsLogSpace.closeTo(y(i),Double.NegativeInfinity)) return false
        i += 1;
      }
    }
    true
  }

  implicit val ring: Semiring[Elem] = new Semiring[Elem] {

    override def closeTo(x: Elem, y: Elem) = {
      import Semiring.LogSpace.doubleIsLogSpace;
      val ret = doubleIsLogSpace.closeTo(x.totalProb, y.totalProb) &&
               (cheatOnEquals || ((x.counts zip y.counts forall {case (x,y) => mnorm(x,y)}) &&
                (x.positionScores zip y.positionScores forall {case (x,y) => Semiring.LogSpace.doubleIsLogSpace.closeTo(x,y)})
               ))
                true
      ret
    }


    override def maybe_+=(x:Elem, y: Elem) = if(x.totalProb == Double.NegativeInfinity) (plus(y,zero),closeTo(zero,y)) else {
      import scalanlp.math.Semiring.LogSpace.doubleIsLogSpace
      if(doubleIsLogSpace.closeTo(x.totalProb,logSum(x.totalProb,y.totalProb))) {
        (x,true)
      } else {
        var p = 0;
        while(p < maxPosition) {
          x.positionScores(p) = logSum(x.positionScores(p),y.positionScores(p));
          p += 1;
        }
        logAddInPlace2D(x.counts,y.counts)
        val newTotalProb = logSum(x.totalProb,y.totalProb);
        (x.copy(totalProb=newTotalProb),false);
      }
    }




    def plus(x: Elem, y: Elem) = {
      val newProb = logSum(x.totalProb,y.totalProb);
      
      val counts = x.counts.map(arr => if(arr eq null) null else arr.copy);
      logAddInPlace2D(counts,y.counts)

      val positionScores = Array.tabulate(maxPosition){ p => logSum(x.positionScores(p),y.positionScores(p)) };

      Elem(counts, positionScores, newProb);
    }


    def times(x: Elem, y: Elem) = {
      val newProb = x.totalProb + y.totalProb;

      val counts = x.counts.map { arr =>
        if(arr eq null) null else arr + y.totalProb value
      };
      for( i <- 0 until maxPosition;
          if x.positionScores(i) != Double.NegativeInfinity;
           j <- 0 until (maxPosition - i)) {
         if(y.counts(j) != null) {
           if(counts(i+j) == null) {
             counts(i+j) = y.counts(j) + x.positionScores(i) value;
           } else {
             logAddInPlace(counts(i+j),y.counts(j),x.positionScores(i));
           }
         }
      }

      val positionScores = Array.fill(maxPosition)(Double.NegativeInfinity);
      var i = 0;
      while(i < maxPosition) {
        if(x.positionScores(i) != Double.NegativeInfinity) {
          var j = 0;
          while(j < maxPosition - i) {
            positionScores(i+j) = logSum(positionScores(i+j),x.positionScores(i) + y.positionScores(j));
            j+= 1;
          }
        }
        i += 1;
      }

      val r = Elem(counts,  positionScores, newProb);
      r
    }

    def closure(x: Elem) = {
      import Semiring.LogSpace.doubleIsLogSpace.{closure=>logClosure};
      val p_* = logClosure(x.totalProb);

      // counts. We're just going to assume that we're only closing over counts in position 0
      // XXX TODO
      val counts = Array.fill(maxPosition)(mkAdaptiveVector);
      val x0 = if(x.counts(0) == null) mkAdaptiveVector else x.counts(0);
      counts(0) := x0;
      val total = logSum(x0.toArray);
      for( i <- 1 until maxPosition) {
        counts(i) := x0 + total * i;
      }

      var positionScores : Array[Double] = counts.map(a => logSum(a.toArray)).take(maxPosition-1);
      positionScores = Array(logClosure(x.positionScores(0))) ++ positionScores;

      val r = Elem(counts,positionScores,p_*);
      r
    }

    val one = {
      val arr = Array.fill(maxPosition)(Double.NegativeInfinity);
      arr(0) = 0.0;
      Elem(new Array[AdaptiveVector](maxPosition),arr,0.0);
    }

    val zero = Elem(new Array[AdaptiveVector](maxPosition),Array.fill(maxPosition)(Double.NegativeInfinity),-1.0/0.0);
  }

  def promote[S](a: Arc[Double,S,Char]) = {
    val counts = new Array[AdaptiveVector](maxPosition);
    val posScores = new Array[Double](maxPosition);
    Arrays.fill(posScores,Double.NegativeInfinity);
    if (a.label != implicitly[Alphabet[Char]].epsilon) {
      counts(0) = mkAdaptiveVector;
      counts(0)(charIndex(a.label)) = a.weight;
      posScores(1) = a.weight;
    } else {
      posScores(0) = a.weight;
    }
    Elem(counts, posScores, a.weight);
  }

  def promoteOnlyWeight(w: Double) = if(w == Double.NegativeInfinity) ring.zero else {
    val posScores = new Array[Double](maxPosition);
    Arrays.fill(posScores,Double.NegativeInfinity);
    val counts = new Array[AdaptiveVector](maxPosition);
    counts(0) = mkAdaptiveVector;
    counts(0)(beginningUnigramId) = w;
    posScores(1) = w;
    Elem(counts, posScores, w);
  }

}
