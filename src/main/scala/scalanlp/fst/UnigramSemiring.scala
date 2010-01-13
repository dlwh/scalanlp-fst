package scalanlp.fst

import scalala.Scalala.{iArrayToVector=>_, _};
import scalala.tensor.dense._
import scalanlp.counters.LogCounters._;
import scalanlp.math.Semiring
import scalanlp.math.Numerics._;import scalanlp.util.Index


class UnigramSemiring[@specialized("Char") T:Alphabet](chars: Set[T], beginningUnigram:T) {

  val charIndex = Index[T]();
  val beginningUnigramId = charIndex(beginningUnigram)
  for( ch <- chars) { charIndex(ch) }

  case class Elem(counts: DenseVector, totalProb: Double) {
    def decode: LogDoubleCounter[T] = {
      aggregate( counts.map { (iv:(Int,Double)) => (charIndex.get(iv._1),iv._2)});
    }
  }

  private def mnorm(x: DenseVector, y: DenseVector): Boolean = {
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
                mnorm(x.counts,y.counts)
      ret
    }

    def plus(x: Elem, y: Elem) = {
      val newProb = logSum(x.totalProb,y.totalProb);
      
      val counts = x.counts.copy;
      logAddInPlace(counts,y.counts)
      Elem(counts, newProb);
    }


    def times(x: Elem, y: Elem) = {
      val newProb = x.totalProb + y.totalProb;

      val counts = x.counts + y.totalProb value;
      counts(beginningUnigramId) = Double.NegativeInfinity;
      logAddInPlace(counts,y.counts,x.totalProb);

      val r = Elem(counts, newProb);
      r
    }

    def closure(x: Elem) = {
      import Semiring.LogSpace.doubleIsLogSpace.{closure=>logClosure};
      val p_* = logClosure(x.totalProb);

      // counts. We're just going to assume that we're only closing over counts in position 0
      // XXX TODO
      val counts = x.counts + 2 * p_*;

      val r = Elem(counts,p_*);
      r
    }

    val one = {
      Elem(mkDenseVector,0.0);
    }

    val zero = Elem(mkDenseVector,-1.0/0.0);

  }


  def promote[S](a: Arc[Double,S,T]) = {
    val counts = mkDenseVector;
    if (a.label != implicitly[Alphabet[T]].epsilon) {
      counts(charIndex(a.label)) = a.weight;
    }
    Elem(counts, a.weight);
  }

  def promoteOnlyWeight(w: Double) = if(w == Double.NegativeInfinity) ring.zero else {
    val vec = mkDenseVector;
    vec(beginningUnigramId) = w;
    Elem(vec, w);
  }

  private def mkDenseVector = {
    val r = new DenseVector(charIndex.size);
    r += Double.NegativeInfinity;
    r
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

}
