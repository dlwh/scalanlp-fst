package scalanlp.fst;

import scalanlp.math._;
import Numerics.logSum;
import scalala.Scalala._;
import scala.collection.mutable.ArrayBuffer;
import scalanlp.counters.LogCounters._;

object BigramSemiring {
  case class Elem(leftFrontier: LogDoubleCounter[Char],
                  counts: LogPairedDoubleCounter[Char,Char],
                  totalProb: Double,
                  emptyActive: Double,
                  rightFrontier: LogDoubleCounter[Char]) {
  }

  val epsilon = Alphabet.zeroEpsCharBet.epsilon;

  implicit val ring: Semiring[Elem] = new Semiring[Elem] {

    override def closeTo(x: Elem, y: Elem) = {
      import Semiring.LogSpace.doubleIsLogSpace;
      
      (doubleIsLogSpace.closeTo(x.totalProb, y.totalProb) &&
      doubleIsLogSpace.closeTo(x.emptyActive, y.emptyActive) &&
      x.counts.forall { case (k,v) => doubleIsLogSpace.closeTo(v,y.counts(k))} &&
      x.leftFrontier.forall { case (k,v) => doubleIsLogSpace.closeTo(v,y.leftFrontier(k))} &&
      x.rightFrontier.forall { case (k,v) => doubleIsLogSpace.closeTo(v,y.rightFrontier(k))} &&
      true )
    }

    def logAdd(a: LogDoubleCounter[Char], b: LogDoubleCounter[Char]) = {
      val newActive = a.copy;
      for( (k,v) <- b) {
        newActive(k) = logSum(newActive(k),v);
      }
      newActive
    }

    def plus(x: Elem, y: Elem) = {
      val newProb = logSum(x.totalProb,y.totalProb);
      val newCounts = x.counts.copy;
      for( (k,v) <- y.counts) {
        newCounts(k) = logSum(newCounts(k),v);
      }

      val rightFrontier = logAdd(x.rightFrontier,y.rightFrontier);
      val leftFrontier = logAdd(x.leftFrontier,y.leftFrontier);
      val emptyActive = logSum(x.emptyActive,y.emptyActive);

      Elem(leftFrontier, newCounts, newProb, emptyActive, rightFrontier);
    }

    def times(x: Elem, y: Elem) = {
      val newProb = x.totalProb + y.totalProb;
      val newCounts = (x.counts + y.totalProb).value;
      for( (k1,k2,v) <- y.counts.triples) {
        newCounts(k1,k2) = logSum(newCounts(k1,k2),v + x.totalProb);
      }

      for((xc,xprob) <- x.rightFrontier; 
          (yc,yprob) <- y.leftFrontier) {
        newCounts(xc,yc) = logSum(newCounts(xc,yc),xprob + yprob); 
      }
           
      val rightFrontier = if(y.rightFrontier.size != 0) {
        val r = (y.rightFrontier + x.totalProb).value

        if(y.emptyActive != Double.NegativeInfinity) {
          for( (xc,xprob) <- x.rightFrontier) {
            r(xc) = logSum(r(xc),xprob + y.emptyActive);
          }
        }

        r;
      } else {
        (x.rightFrontier + y.totalProb).value;
      }

      val leftFrontier = if(x.leftFrontier.size != 0) {
        val r = (x.leftFrontier + y.totalProb).value

        if(x.emptyActive != Double.NegativeInfinity) {
          for( (xc,xprob) <- y.leftFrontier) {
            r(xc) = logSum(r(xc),xprob + y.emptyActive);
          }
        }

        r;
      } else {
        (y.leftFrontier + x.totalProb).value;
      }
      

      val r = Elem(leftFrontier,newCounts,newProb,y.emptyActive + x.emptyActive,rightFrontier);
      r
    }

    def closure(x: Elem) = {
      import Semiring.LogSpace.doubleIsLogSpace.{closure=>logClosure};
      val p_* = logClosure(x.totalProb);
      val newCounts = x.counts.copy;

      for((x1,p1) <- x.rightFrontier;
          (x2,p2) <- x.leftFrontier) {
        newCounts(x1,x2) = logSum(newCounts(x1,x2),p1+p2);
      }
      val leftFrontier = (x.leftFrontier + (p_* - x.totalProb)).value;
      val rightFrontier = (x.rightFrontier + (p_* - x.totalProb)).value;
      
      Elem(leftFrontier, newCounts + (2 * p_*) value, p_*, 2 * p_*, rightFrontier);
    }

    val one = Elem(LogDoubleCounter(), LogPairedDoubleCounter[Char,Char](),0.0,0.0,LogDoubleCounter());
    val zero = Elem(LogDoubleCounter(), LogPairedDoubleCounter[Char,Char](),-1.0/0.0,-1.0/0.0,LogDoubleCounter());
  }

  def promote[S](a: Arc[Double,S,Char]) = {
    val counts = LogPairedDoubleCounter[Char,Char]();
    val active = LogDoubleCounter[Char]();
    if(a.label != epsilon)
      active(a.label) = a.weight;
    val emptyScore = if(a.label == epsilon) a.weight else Double.NegativeInfinity;
    Elem(active, counts,a.weight,emptyScore,active);
  }

  def promoteOnlyWeight(w: Double) = {
    val counts = LogPairedDoubleCounter[Char,Char]();
    val active = LogDoubleCounter[Char]();
    active('#') = w;
    Elem(active, counts,w,Double.NegativeInfinity,active);
  }

}
