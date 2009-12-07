package scalanlp.fst;

import scalanlp.math._;
import Numerics.logSum;
import scalala.Scalala._;
import scala.collection.mutable.ArrayBuffer;
import scalanlp.counters.LogCounters._;

object TrigramSemiring {
  case class Elem(counts: LogPairedDoubleCounter[Gram,Char], totalProb: Double, active: LogDoubleCounter[Gram]) {
  }

  sealed abstract class Gram {
    def length : Int

    def join(g: Gram) = (this,g) match {
    /*
      case (_,t2:Trigram) => t2
      case (_,Noncegram) => this
      case (Noncegram,_) => g
      case (a:Trigram,b: Unigram) => Trigram(a._2,a._3,b._1);
      case (a:Bigram,b: Unigram) => Trigram(a._1,a._2,b._1);
      case (a:Unigram,b: Unigram) => Bigram(a._1,b._1);

      case (a:Trigram,b: Bigram) => Trigram(a._2,b._1,b._2);
      case (a:Bigram,b: Bigram) => Trigram(a._2,b._1,b._2);
      case (a:Unigram,b: Bigram) => Trigram(a._1,b._1,b._2);
      */
      case (_,b: Bigram) => Seq(b)
      case (_:Bigram,b: Unigram) => Seq.empty
      case (a:Unigram,b: Unigram) => Seq(Bigram(a._1,b._1),b);
      case (_,Noncegram) => Seq(this)
      case (Noncegram,_) => Seq(g);
    }
  }

/*
  class Trigram(_1: Char, _2:Char, _3:Char) extends Gram {
    def length = 3;
  }
  */
  case class Bigram(_1: Char, _2:Char) extends Gram {
    def length = 2;
  }
  case class Unigram(_1: Char) extends Gram {
    def length = 1;
  }
  case object Noncegram extends Gram {
    def length = 0;
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

      Elem(newCounts, newProb, newActive);
    }

    def times(x: Elem, y: Elem) = {
      val newProb = x.totalProb + y.totalProb;
      val active = LogDoubleCounter[Gram]();

      val newCounts = (x.counts + y.totalProb).value;
      for( (k1,k2,v) <- y.counts.triples) {
        newCounts(k1,k2) = logSum(newCounts(k1,k2),v + x.totalProb);
      }

      for((xc,xprob) <- x.active; 
          (yc,yprob) <- y.active) {
        yc match {
          case Unigram(ch) =>
            newCounts(xc,ch) = logSum(newCounts(xc,ch),xprob + yprob); 
          case _ =>
        }
        for(suffix <- xc join yc) {
          active(suffix) = logSum(active(suffix),xprob + yprob);
        }
      }
           

      val r = Elem(newCounts,newProb,active);
      r
    }

    def closure(x: Elem) = {
      import Semiring.LogSpace.doubleIsLogSpace.{closure=>logClosure};
      val p_* = logClosure(x.totalProb);
      val newCounts = x.counts.copy;

      val newActive = x.active.like;
      for((x1,p1) <- x.active;
          (x2,p2) <- x.active) { 
        x2 match {
          case Unigram(ch) => newCounts(x1,ch) = logSum(newCounts(x1,ch),p1+p2);
          case _ =>
        }

        for(suffix <- x1 join x2) {
          newActive(suffix) = logSum(newActive(suffix),p1 + p2);
        }
      }
      newActive(Noncegram) = logSum(newActive(Noncegram),2 * p_*);
      
      Elem(newCounts + (2 * p_*) value, p_*, newActive);
    }

    val one = Elem(LogPairedDoubleCounter[Gram,Char](),0.0,LogDoubleCounter());
    one.active(Noncegram) = 0.0;
    val zero = Elem(LogPairedDoubleCounter[Gram,Char](),-1.0/0.0,LogDoubleCounter());
  }

  def promote[S](a: Arc[Double,S,Char,Char]) = {
    assert(a.in == a.out);
    val counts = LogPairedDoubleCounter[Gram,Char]();
    val active = LogDoubleCounter[Gram]();
    if(a.in != epsilon)
      active(Unigram(a.in)) = a.weight;
    else 
      active(Noncegram) = a.weight;
    Elem(counts,a.weight,active);
  }

  def promoteOnlyWeight(w: Double) = {
    val counts = LogPairedDoubleCounter[Gram,Char]();
    val active = LogDoubleCounter[Gram]();
    active(Unigram('#')) = w;
    active(Bigram('#','#')) = w;
    Elem(counts,w,active);
  }

}
