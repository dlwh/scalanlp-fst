package scalanlp.fst;

import scalanlp.math._;
import Numerics.logSum;
import scalala.Scalala._;
import scala.collection.mutable.ArrayBuffer;
import scalanlp.counters.LogCounters._;

object BigramSemiring {
  case class Elem(counts: LogPairedDoubleCounter[Char,Char], totalProb: Double, emptyActive: Double, active: LogDoubleCounter[Char]) {
  }

  val epsilon = Alphabet.zeroEpsCharBet.epsilon;

  implicit val ring: Semiring[Elem] = new Semiring[Elem] {

    override def closeTo(x: Elem, y: Elem) = {
      import Semiring.LogSpace.doubleIsLogSpace;
      (doubleIsLogSpace.closeTo(x.totalProb, y.totalProb) &&
      x.counts.forall { case (k,v) => doubleIsLogSpace.closeTo(v,y.counts(k))})
    }

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

      Elem(newCounts, newProb, logSum(x.emptyActive,y.emptyActive), newActive);
    }

    def times(x: Elem, y: Elem) = {
      val newProb = x.totalProb + y.totalProb;
      val newCounts = (x.counts + y.totalProb).value;
      for( (k1,k2,v) <- y.counts.triples) {
        newCounts(k1,k2) = logSum(newCounts(k1,k2),v + x.totalProb);
      }

      for((xc,xprob) <- x.active; 
          (yc,yprob) <- y.active) {
        newCounts(xc,yc) = logSum(newCounts(xc,yc),xprob + yprob); 
      }
           
      val active = if(y.active.size != 0) {
        val r = (y.active + x.totalProb).value

        if(y.emptyActive != Double.NegativeInfinity) {
          for( (xc,xprob) <- x.active) {
            r(xc) = logSum(r(xc),xprob + y.emptyActive);
          }
        }

        r;
      } else {
        (x.active + y.totalProb).value;
      }
      

      val r = Elem(newCounts,newProb,y.emptyActive + x.emptyActive,active);
      r
    }

    def closure(x: Elem) = {
      import Semiring.LogSpace.doubleIsLogSpace.{closure=>logClosure};
      val p_* = logClosure(x.totalProb);
      val newCounts = x.counts.copy;

      for((x1,p1) <- x.active;
          (x2,p2) <- x.active) {
        newCounts(x1,x2) = logSum(newCounts(x1,x2),p1+p2);
      }
      val newActive = (x.active + (p_* - x.totalProb)).value;
      
      Elem(newCounts + (2 * p_*) value, p_*, 2 * p_*, newActive);
    }

    val one = Elem(LogPairedDoubleCounter[Char,Char](),0.0,0.0,LogDoubleCounter());
    val zero = Elem(LogPairedDoubleCounter[Char,Char](),-1.0/0.0,-1.0/0.0,LogDoubleCounter());
  }

  def promote[S](a: Arc[Double,S,Char,Char]) = {
    assert(a.in == a.out);
    val counts = LogPairedDoubleCounter[Char,Char]();
    val active = LogDoubleCounter[Char]();
    if(a.in != epsilon)
      active(a.in) = a.weight;
    val emptyScore = if(a.in == epsilon) a.weight else Double.NegativeInfinity;
    Elem(counts,a.weight,emptyScore,active);
  }

  def promoteOnlyWeight(w: Double) = {
    val counts = LogPairedDoubleCounter[Char,Char]();
    val active = LogDoubleCounter[Char]();
    active('#') = w;
    Elem(counts,w,Double.NegativeInfinity,active);
  }

}
