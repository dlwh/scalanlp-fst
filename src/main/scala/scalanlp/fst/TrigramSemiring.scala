package scalanlp.fst;

import scalanlp.math._;
import Numerics.logSum;
import scalala.Scalala._;
import scalala.tensor.sparse._;
import scala.collection.mutable.ArrayBuffer;
import scalanlp.collection.mutable.ArrayMap;
import scalanlp.counters.LogCounters._;

import scalanlp.util.Index;

/**
* 
* @param acceptableChars : only learn bigram histories that contain (only) these chars
*/
class TrigramSemiring(acceptableChars: Set[(Char,Char)]) {
  import TrigramSemiring._;

  val charIndex = Index[EncodedChars]();
  val gramIndex = Index[Gram]();
  for {
    (a,b) <- acceptableChars;
    enc = encode(a,b);
    ug = Unigram(enc)
  } {
    gramIndex(ug);
  }
  val maxAcceptableGram = gramIndex.size;

  def isAcceptableHistoryGram(gI: Int) = gI < maxAcceptableGram;

  private def mkGramCharMap = new ArrayMap[SparseVector] {
    override def default(k: Int) = getOrElseUpdate(k,mkSparseVector);
  }

  private def mkSparseVector = {
    val r = new SparseVector(Int.MaxValue);
    r.default = Double.NegativeInfinity;
    r
  }

  case class Elem(leftFrontier: SparseVector, 
                  counts: ArrayMap[SparseVector],
                  length0Score: Double,
                  length1Chars: SparseVector,
                  totalProb: Double,
                  rightFrontier: SparseVector) {
    def decode = {
      val result = LogPairedDoubleCounter[Gram,EncodedChars]();
      for {
        (xcI,row) <- counts.iterator
        xc = gramIndex.get(xcI);
        (ycI,v) <- row.activeElements
        yc = charIndex.get(ycI)
      } {
        result(xc,yc) = v; 
      }
      result
    }
  }

  val beginningUnigramId = gramIndex(beginningUnigram)
  val beginningBigramId = gramIndex(beginningBigram)
  val NonceId = gramIndex(Noncegram);

  private val epsilon = Alphabet.zeroEpsCharBet.epsilon;

  private def logAddInPlace2D(to: ArrayMap[SparseVector], from: ArrayMap[SparseVector], scale: Double=0.0) {
    for( (k,row) <- from) {
      val old = to(k);
      if(old.activeDomain.size == 0) {
        old := row + scale;
      } else {
        logAddInPlace(old,row,scale);
      }
    }
  }


  private def logAddInPlace(to: SparseVector, from: SparseVector, scale: Double=0.0) {
    for( (k,v) <- from.activeElements) {
      to(k) = logSum(to(k),v+scale);
    }
  }

  implicit val ring: Semiring[Elem] = new Semiring[Elem] {

    override def closeTo(x: Elem, y: Elem) = {
      import Semiring.LogSpace.doubleIsLogSpace;
      val ret = (doubleIsLogSpace.closeTo(x.totalProb, y.totalProb) &&
        x.counts.size == y.counts.size)
      if(!ret && (doubleIsLogSpace.closeTo(x.totalProb, y.totalProb)))
        println("Close to problem!");
      ret
    }

    def plus(x: Elem, y: Elem) = {
      val newProb = logSum(x.totalProb,y.totalProb);
      
      val newCounts = mkGramCharMap;
      for( (k,row) <- x.counts) {
        newCounts(k) = row.copy;
      }
      logAddInPlace2D(newCounts,y.counts)

      val leftFrontier = x.leftFrontier.copy;
      logAddInPlace(leftFrontier,y.leftFrontier);
      val rightFrontier = x.rightFrontier.copy;
      logAddInPlace(rightFrontier,y.rightFrontier);
      val length1Chars = x.length1Chars.copy;
      logAddInPlace(length1Chars,y.length1Chars);

      val length0Score = logSum(x.length0Score,y.length0Score);

      val r = Elem(leftFrontier, newCounts, length0Score, length1Chars, newProb, rightFrontier);
      r
    }


    def times(x: Elem, y: Elem) = {
      val newProb = x.totalProb + y.totalProb;
      val active = mkSparseVector;

      val newCounts = mkGramCharMap;
      logAddInPlace2D(newCounts,x.counts,y.totalProb);
      logAddInPlace2D(newCounts,y.counts,x.totalProb);

/*
      for( (yc,yprob) <- y.active.activeElements) {
        gramIndex.get(yc) match {
          case gr@Unigram(ch) =>
            val chI = charIndex(ch);

            for( (xc,xprob) <- x.active.activeElements) {
              newCounts(xc)(chI) = logSum(newCounts(xc)(chI),xprob + yprob); 
              xc match {
                case NonceId =>
                  active(yc) = logSum(active(yc),xprob + yprob);
                case gramIndex(Unigram(chX)) =>
                  if (isAcceptableHistoryGram(xc) && isAcceptableHistoryGram(yc)) {
                  }
              }
              for(suffix <- gramIndex.get(xc) join gr) {
                active(gramIndex(suffix)) = logSum(active(gramIndex(suffix)),xprob + yprob);
              }
            }
          case Noncegram =>
            logAddInPlace(active,x.active,yprob);
          case ycb: Bigram =>
            active(yc) = logSum(active(yc),x.totalProb + yprob);
        }
      }

      val r = Elem(newCounts,newProb,active);
      r
      */
      // TODO
      x
    }

    def closure(x: Elem) = {
    /*
      import Semiring.LogSpace.doubleIsLogSpace.{closure=>logClosure};
      val p_* = logClosure(x.totalProb);
      val newCounts = mkGramCharMap;
      logAddInPlace2D(newCounts,x.counts)

      val newActive = mkSparseVector;
      for((x1,p1) <- x.active.activeElements;
          x1ctr = newCounts(x1);
          (x2,p2) <- x.active.activeElements) { 
        gramIndex.get(x2) match {
          case xx@Unigram(ch) => 
            val chI = charIndex(ch);
            x1ctr(chI) = logSum(x1ctr(chI),p1+p2);
          case _ => 
        }

        for(suffix <- gramIndex.get(x1) join gramIndex.get(x2)) {
          newActive(gramIndex(suffix)) = logSum(newActive(gramIndex(suffix)),p1 + p2);
        }
      }
      newActive(NonceId) = logSum(newActive(NonceId),2 * p_*);
      val newCounts2 = mkGramCharMap;
      logAddInPlace2D(newCounts2,newCounts,2 * p_*);

      Elem(newCounts2, p_*, newActive);
      */
      x
    }

    val one = Elem(mkSparseVector,mkGramCharMap,0.0,mkSparseVector,0.0,mkSparseVector);
    val zero = Elem(mkSparseVector,mkGramCharMap,-1.0/0.0,mkSparseVector,-1.0/0.0,mkSparseVector);
  }

  def promote[S](a: Arc[Double,S,Char,Char]) = {
    val counts = mkGramCharMap;
    val active = mkSparseVector;
    if(a.in != epsilon || a.out != epsilon) {
      val id = gramIndex(Unigram(encode(a.in,a.out)));
      active(id) = a.weight;
    } 
    val nonceScore = if(a.in != epsilon || a.out != epsilon) {
      Double.NegativeInfinity;
    } else {
      a.weight;
    }
    Elem(active,counts,nonceScore,active,a.weight,active);
  }

  def promoteOnlyWeight(w: Double) = {
    val counts = mkGramCharMap;
    val active = mkSparseVector;
    active(beginningUnigramId) = w;
    active(beginningBigramId) = w;
    val l1active = mkSparseVector;
    l1active(beginningUnigramId) = w;
    Elem(active,counts,Double.NegativeInfinity,l1active,w,active);
  }

}

object TrigramSemiring {
  type EncodedChars = Int

  def encode(ch1: Char, ch2: Char) = ((ch1.toInt << 16)|(ch2)):EncodedChars
  def encodeOne(ch: Char) = encode(ch,ch);
  def decode(cc: EncodedChars) = ((cc >> 16).toChar,(cc & 0XFFFF).toChar);

  sealed abstract class Gram {
    def length : Int

    def join(g: Gram) = (this,g) match {
      case (_,b: Bigram) => Seq(b)
      case (_:Bigram,b: Unigram) => Seq.empty
      case (a:Unigram,b: Unigram) => Seq(Bigram(a._1,b._1),b);
      case (_,Noncegram) => Seq(this)
      case (Noncegram,_) => Seq(g);
    }
  }

  case class Bigram(_1: EncodedChars, _2:EncodedChars) extends Gram {
    def length = 2;
    override def hashCode = _1 * 37 + _2;
  }
  case class Unigram(_1: EncodedChars) extends Gram {
    def length = 1;
    override def hashCode = _1;
  }
  case object Noncegram extends Gram {
    def length = 0;
    override def hashCode = 0;
  }

  // for Automata, we can pass in just chars
  object Bigram {
    def apply(ch1: Char, ch2: Char) = new Bigram(encodeOne(ch1),encodeOne(ch2));
  } 

  // for Automata, we can pass in just chars
  object Unigram {
    def apply(ch1: Char) = new Unigram(encodeOne(ch1));
  } 

  val beginningUnigram = Unigram(encode('#','#'));
  val beginningBigram = Bigram(encode('#','#'),encode('#','#'));
}
