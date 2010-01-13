package scalanlp.fst;

import scalanlp.math._;
import scalanlp.math.Numerics._;
import scala.runtime.ScalaRunTime;
import scalala.Scalala.{iArrayToVector=>_, _};
import scalala.tensor.dense._;
import scalanlp.collection.mutable.ArrayMap;
import scalanlp.counters.LogCounters._;

import scalanlp.util.Index;

/**
 *
 * @param acceptableTs : only learn bigram histories that contain (only) these chars
 * @param acceptableBigrams: only learn bigrams histories that are these bigrams.
 */
class PositionalUnigramSemiring[@specialized("Char") T:Alphabet](maxPosition: Int, chars: Set[T], beginningUnigram: T) {

  case class Elem(counts: Seq[DenseVector], positionScores: Seq[Double], totalProb: Double) {
    def decode: Seq[LogDoubleCounter[T]] = {
      val result = counts.map { v => aggregate(v.map { (iv:(Int,Double)) => (charIndex.get(iv._1),iv._2) } )};
      result;
    }
  }

  val charIndex = Index[T]();
  val beginningUnigramId = charIndex(beginningUnigram)
  for( ch <- chars) { charIndex(ch) }

  private def mkDenseVector = {
    val r = new DenseVector(charIndex.size);
    r += Double.NegativeInfinity;
    r
  }

  private def logAddInPlace2D(to: Seq[DenseVector], from: Seq[DenseVector], scale: Double=0.0) {
    for(i <- 0 until maxPosition) {
      logAddInPlace(to(i),from(i),scale);
    }
  }


  private def logAdd(to: DenseVector, from: DenseVector, scale: Double=0.0) = {
    val ret = to.copy;
    logAddInPlace(ret,from,scale);
    ret;
  }


  private def logAddInPlace(to: DenseVector, from: DenseVector, scale: Double=0.0) {
    if (scale != Double.NegativeInfinity) {
      var k = 0;
      while( k < charIndex.size) {
        to(k) = logSum(to(k),from(k) + scale);
        k += 1;
      }
    }
  }

  implicit val ring: Semiring[Elem] = new Semiring[Elem] {

    override def closeTo(x: Elem, y: Elem) = {
      import Semiring.LogSpace.doubleIsLogSpace;
      val ret = doubleIsLogSpace.closeTo(x.totalProb, y.totalProb) &&
                 // x.trigramCounts.size == y.trigramCounts.size &&
                 true
      ret
    }

    def plus(x: Elem, y: Elem) = {
      val newProb = logSum(x.totalProb,y.totalProb);
      
      val counts = x.counts.map(_.copy);
      logAddInPlace2D(counts,y.counts)

      val positionScores = Array.tabulate(maxPosition){ p => logSum(x.positionScores(p),y.positionScores(p)) };

      Elem(counts, positionScores, newProb);
    }


    def times(x: Elem, y: Elem) = {
      val newProb = x.totalProb + y.totalProb;

      val counts = x.counts.map { _ + y.totalProb value };
      for( i <- 0 until maxPosition;
           j <- 0 until (maxPosition - i)) {
         logAddInPlace(counts(i+j),y.counts(j),x.positionScores(i));
      }

      val positionScores = Array.fill(maxPosition)(Double.NegativeInfinity);
      for( i <- 0 until maxPosition;
          j <- 0 until (maxPosition - i)) {
        positionScores(i+j) = logSum(positionScores(i+j),x.positionScores(i) + y.positionScores(j));
      }

      val r = Elem(counts,  positionScores, newProb);
      r
    }

    def closure(x: Elem) = {
      import Semiring.LogSpace.doubleIsLogSpace.{closure=>logClosure};
      val p_* = logClosure(x.totalProb);

      // counts. We're just going to assume that we're only closing over counts in position 0
      // XXX TODO
      val counts = Array.fill(maxPosition)(mkDenseVector);
      counts(0) := x.counts(0);
      val total = logSum(x.counts(0).toArray);
      for( i <- 1 until maxPosition) {
        counts(i) := x.counts(0) + total * i;
      }

      var positionScores : Seq[Double] = counts.map(a => logSum(a.toArray)).take(maxPosition-1);
      positionScores = Seq(logClosure(x.positionScores(0))) ++ positionScores;

      val r = Elem(counts,positionScores,p_*);
      r
    }

    val one = {
      val arr = Array.fill(maxPosition)(Double.NegativeInfinity);
      arr(0) = 0.0;
      Elem(Array.fill(maxPosition)(mkDenseVector),arr,0.0);
    }

    val zero = Elem(Array.fill(maxPosition)(mkDenseVector),Array.fill(maxPosition)(Double.NegativeInfinity),-1.0/0.0);
  }

  def promote[S](a: Arc[Double,S,T]) = {
    val counts = Array.fill(maxPosition)(mkDenseVector);
    val posScores = Array.fill(maxPosition)(Double.NegativeInfinity);
    if (a.label != implicitly[Alphabet[T]].epsilon) {
      counts(0)(charIndex(a.label)) = a.weight;
      posScores(1) = a.weight;
    } else {
      posScores(0) = a.weight;
    }
    Elem(counts, posScores, a.weight);
  }

  def promoteOnlyWeight(w: Double) = if(w == Double.NegativeInfinity) ring.zero else {
    val arr = Array.fill(maxPosition)(Double.NegativeInfinity);
    val counts = Array.fill(maxPosition)(mkDenseVector);
    counts(0)(beginningUnigramId) = w;
    arr(1) = w;
    Elem(counts, arr, w);
  }

}