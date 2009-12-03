package scalanlp.fst;

import scalanlp.math._;
import Numerics.logSum;
import scalala.Scalala._;
import scala.collection.mutable._;
import scalanlp.counters.LogCounters._;

object BigramSemiring {
  case class Elem(counts: LogDoubleCounter[Seq[Char]], totalProb: Double, active: LogDoubleCounter[Char]) {
  }

  implicit val ring: Semiring[Elem] = new Semiring[Elem] {
    def plus(x: Elem, y: Elem) = {
      val newProb = logSum(x.totalProb,y.totalProb);
      val newCounts = x.counts.copy;
      for( (k,v) <- y.counts) {
        newCounts(k) = logSum(newCounts(k),v);
      }

      val newActive = x.active.copy;
      for( (k,v) <- y.active) {
        newActive(k) = logSum(newActive(k),v);
      }

      Elem(newCounts, newProb, newActive);
    }

    def times(x: Elem, y: Elem) = {
      val newProb = x.totalProb + y.totalProb;
      val newCounts = (x.counts + y.totalProb).value;
      for( (k,v) <- y.counts) {
        newCounts(k) = logSum(newCounts(k),v + x.totalProb);
      }

      for((xc,xprob) <- x.active; 
          (yc,yprob) <- y.active) {
        val s = ArrayBuffer(xc,yc)
        newCounts(s) = logSum(newCounts(s),xprob + yprob); 
      }
           
      val active = if(y.active.size != 0) {
        (y.active + x.totalProb).value
      } else {
        (x.active + y.totalProb).value;
      }

      val r = Elem(newCounts,newProb,active);
      r
    }

    def closure(x: Elem) = {
      import Semiring.LogSpace.doubleIsLogSpace.{closure=>logClosure};
      val p_* = logClosure(x.totalProb);
      val newCounts = x.counts.copy;
      for((x1,p1) <- x.active;
          (x2,p2) <- x.active) {
        val s = ArrayBuffer(x1,x2)
        newCounts(s) = logSum(newCounts(s),p1+p2);
      }
      
      Elem( newCounts + (2 * p_*) value, p_*, x.active + (p_* - x.totalProb) value);
    }

    val one = Elem(LogDoubleCounter[Seq[Char]](),0.0,LogDoubleCounter());
    val zero = Elem(LogDoubleCounter[Seq[Char]](),-1.0/0.0,LogDoubleCounter());
  }

  def promote[S](a: Arc[Double,S,Char,Char]) = {
    assert(a.in == a.out);
    val counts = LogDoubleCounter[Seq[Char]]();
    counts(ArrayBuffer(a.in)) = 0.0;
    val active = LogDoubleCounter[Char]();
    active(a.in) = a.weight;
    Elem(counts,a.weight,active);
  }

  def promoteOnlyWeight(w: Double) = {
    val counts = LogDoubleCounter[Seq[Char]]();
    val active = LogDoubleCounter[Char]();
    active('#') = w;
    Elem(counts,w,active);
  }

}
