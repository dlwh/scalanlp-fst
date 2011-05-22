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

import scalala.tensor.sparse._
import scalanlp.math.Semiring
import scalanlp.util.Index
import scalanlp.tensor.sparse.OldSparseVector
import scalala.library.Numerics._
import scalala.tensor.Counter
;

/**
* Encodes the sufficient statistics for a unigram model of an automaton.
*/
class UnigramSemiring[@specialized(Char) T:Alphabet](chars: Set[T], beginningUnigram:T, cheatOnEquals:Boolean=false) {

  val charIndex = Index[T]();
  val beginningUnigramId = charIndex.index(beginningUnigram)
  for( ch <- chars) { charIndex.index(ch) }

  case class Elem(totalProb: Double, counts: OldSparseVector) {
    def decode: Counter[T,Double] = {
      Counter( counts.activeIterator.map { (iv:(Int,Double)) => (charIndex.get(iv._1),iv._2)});
    }
  }

  private def mnorm(x: OldSparseVector, y: OldSparseVector): Boolean = {
    var i = 0;
    var ok = true;
    //if(x.used != y.used) false
    //else {
      while(i < x.size && ok) {
        if(!Semiring.LogSpace.doubleIsLogSpace.closeTo(x(i),y(i))) ok = false;
        i += 1;
      }
      ok
    //}
  }

  implicit val ring: Semiring[Elem] = new Semiring[Elem] {

    override def closeTo(x: Elem, y: Elem) = {
      import Semiring.LogSpace.doubleIsLogSpace;
      val ret = doubleIsLogSpace.closeTo(x.totalProb, y.totalProb) &&
                (cheatOnEquals || mnorm(x.counts,y.counts))
      ret
    }

    def plus(x: Elem, y: Elem) = {
      val newProb = logSum(x.totalProb,y.totalProb);
      
      val counts = x.counts.copy;
      logAddInPlace(counts,y.counts)
      Elem(newProb, counts);
    }

    override def maybe_+=(x:Elem, y: Elem) = if(x.totalProb == Double.NegativeInfinity) (plus(y,zero),closeTo(zero,y)) else {
      import scalanlp.math.Semiring.LogSpace.doubleIsLogSpace
      if(doubleIsLogSpace.closeTo(x.totalProb,logSum(x.totalProb,y.totalProb))) {
        (x,true)
      } else {
        logAddInPlace(x.counts,y.counts);
        (Elem(logSum(x.totalProb,y.totalProb),x.counts),false);
      }
    }


    def times(x: Elem, y: Elem) = {
      val newProb = x.totalProb + y.totalProb;

      val counts = x.counts + y.totalProb;
      counts(beginningUnigramId) = Double.NegativeInfinity;
      logAddInPlace(counts,y.counts,x.totalProb);

      val r = Elem(newProb, counts);
      r
    }

    def closure(x: Elem) = {
      import Semiring.LogSpace.doubleIsLogSpace.{closure=>logClosure};
      val p_* = logClosure(x.totalProb);

      // counts. We're just going to assume that we're only closing over counts in position 0
      // XXX TODO
      val counts = x.counts + 2 * p_*;

      val r = Elem(p_*,counts);
      r
    }

    val one = {
      Elem(0.0, mkOldSparseVector);
    }

    val zero = Elem(-1.0/0.0, mkOldSparseVector);

  }


  def promote[S](a: Arc[Double,S,T]) = {
    val counts = mkOldSparseVector;
    if (a.label != implicitly[Alphabet[T]].epsilon) {
      counts(charIndex(a.label)) = a.weight;
    }
    Elem(a.weight, counts);
  }

  def promoteOnlyWeight(w: Double) = if(w == Double.NegativeInfinity) ring.zero else {
    val vec = mkOldSparseVector;
    vec(beginningUnigramId) = w;
    Elem(w, vec);
  }

  private def mkOldSparseVector = {
    val r = new OldSparseVector(charIndex.size);
    r.default = Double.NegativeInfinity;
    r
  }

  private def logAdd(to: OldSparseVector, from: OldSparseVector, scale: Double=0.0) = {
    val ret = to.copy;
    logAddInPlace(ret,from,scale);
    ret;
  }


  private def logAddInPlace(to: OldSparseVector, from: OldSparseVector, scale: Double=0.0) {
    if (scale != Double.NegativeInfinity) {
      var offset = 0;
      while( offset < from.activeSize) {
        val k = from.indexAt(offset);
        val v  = from.valueAt(offset);
        to(k) = logSum(to(k),v + scale);
        offset += 1;
      }
    }
  }

}
