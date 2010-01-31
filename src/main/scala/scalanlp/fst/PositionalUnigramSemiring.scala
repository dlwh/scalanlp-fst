package scalanlp.fst;

import scalanlp.math._;
import scalanlp.math.Numerics._;
import scala.runtime.ScalaRunTime;
import scalala.Scalala.{iArrayToVector=>_, _};
import scalala.tensor.adaptive._;
import scalala.tensor.sparse._;
import scalala.tensor.dense._;
import scalanlp.collection.mutable.ArrayMap;
import scalanlp.counters.LogCounters._;
import scala.collection.mutable.{Seq=>MSeq};

import scalanlp.util.Index;

/**
 *
 * @param acceptableTs : only learn bigram histories that contain (only) these chars
 * @param acceptableBigrams: only learn bigrams histories that are these bigrams.
 */
class PositionalUnigramSemiring[@specialized("Char") T:Alphabet](maxPosition: Int, chars: Set[T], beginningUnigram: T, cheatOnEquals: Boolean=false) {

  case class Elem(counts: Seq[AdaptiveVector], positionScores: MSeq[Double], totalProb: Double) {
    def decode: Seq[LogDoubleCounter[T]] = {
      val result = counts.map { v => aggregate(v.activeElements.map { (iv:(Int,Double)) => (charIndex.get(iv._1),iv._2) } )};
      result;
    }
  }

  val charIndex = Index[T]();
  val beginningUnigramId = charIndex(beginningUnigram)
  for( ch <- chars) { charIndex(ch) }

  private def mkAdaptiveVector = {
    val r = new AdaptiveVector(charIndex.size);
    r.default = Double.NegativeInfinity;
    r
  }

  private def logAddInPlace2D(to: Seq[AdaptiveVector], from: Seq[AdaptiveVector], scale: Double=0.0) {
    for(i <- 0 until maxPosition) {
      logAddInPlace(to(i),from(i),scale);
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

  private def mnorm(x: AdaptiveVector, y: AdaptiveVector): Boolean = {
    var i = 0;
    while(i < x.size) {
      if(!Semiring.LogSpace.doubleIsLogSpace.closeTo(x(i),y(i))) return false
      i += 1;
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
        for(p <- 0 until maxPosition) {
          x.positionScores(p) = logSum(x.positionScores(p),y.positionScores(p));
        }
        logAddInPlace2D(x.counts,y.counts)
        val newTotalProb = logSum(x.totalProb,y.totalProb);
        (x.copy(totalProb=newTotalProb),false);
      }
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
          if x.positionScores(i) != Double.NegativeInfinity;
           j <- 0 until (maxPosition - i)) {
         logAddInPlace(counts(i+j),y.counts(j),x.positionScores(i));
      }

      val positionScores = Array.fill(maxPosition)(Double.NegativeInfinity);
      for( i <- 0 until maxPosition;
          if x.positionScores(i) != Double.NegativeInfinity;
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
      val counts = Array.fill(maxPosition)(mkAdaptiveVector);
      counts(0) := x.counts(0);
      val total = logSum(x.counts(0).toArray);
      for( i <- 1 until maxPosition) {
        counts(i) := x.counts(0) + total * i;
      }

      var positionScores : MSeq[Double] = counts.map(a => logSum(a.toArray)).take(maxPosition-1);
      positionScores = MSeq(logClosure(x.positionScores(0))) ++ positionScores;

      val r = Elem(counts,positionScores,p_*);
      r
    }

    val one = {
      val arr = Array.fill(maxPosition)(Double.NegativeInfinity);
      arr(0) = 0.0;
      Elem(Array.fill(maxPosition)(mkAdaptiveVector),arr,0.0);
    }

    val zero = Elem(Array.fill(maxPosition)(mkAdaptiveVector),Array.fill(maxPosition)(Double.NegativeInfinity),-1.0/0.0);
  }

  def promote[S](a: Arc[Double,S,T]) = {
    val counts = Array.fill(maxPosition)(mkAdaptiveVector);
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
    val counts = Array.fill(maxPosition)(mkAdaptiveVector);
    counts(0)(beginningUnigramId) = w;
    arr(1) = w;
    Elem(counts, arr, w);
  }

}