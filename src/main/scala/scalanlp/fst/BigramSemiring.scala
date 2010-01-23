package scalanlp.fst;

import scalanlp.math._;
import scalanlp.math.Numerics._;
import scala.runtime.ScalaRunTime;
import scalala.Scalala._;
import scalala.tensor.sparse._;
import scalanlp.collection.mutable.ArrayMap;
import scalanlp.counters.LogCounters._;

import scalanlp.util.Index;

/**
 *
 * @param acceptableChars : only learn bigram histories that contain (only) these chars
 * @param acceptableBigrams: only learn bigrams histories that are these bigrams.
 */
class BigramSemiring[@specialized("Char") T:Alphabet](acceptableChars: Set[T],
                                                      beginningUnigram: T,
                                                      cheatOnEquals: Boolean=false) {
  val charIndex = Index[T]();
  val beginningUnigramId = charIndex(beginningUnigram);
  for( ab <- acceptableChars) {
    charIndex(ab);
  }
  val maxAcceptableChar = charIndex.size;

  def isAcceptableHistoryChar(gI: Int) = gI < maxAcceptableChar;

  private def mkGramCharMap = new ArrayMap[SparseVector] {
    override final def default(k: Int) = {
      val vec = mkSparseVector;
      update(k,vec)
      vec
    }
  }

  private def copyGramMap(other: ArrayMap[SparseVector]) = {
    val ret = new ArrayMap[SparseVector] {
      override def default(k: Int) = {
        val vec = mkSparseVector
        update(k,vec)
        vec
      }
    }
    for( (i,vec) <- other) {
      ret.update(i,vec.copy)
    }
    ret
  }


  private def mkSparseVector = {
    val r = new SparseVector(Int.MaxValue);
    r.default = Double.NegativeInfinity;
    r
  }

  case class Elem(leftUnigrams: SparseVector,
                  bigramCounts: ArrayMap[SparseVector],
                  length0Score: Double,
                  totalProb: Double,
                  rightUnigrams: SparseVector) {

    private def decodeVector[T](vec: SparseVector, index: Index[T]) = {
      val result = LogDoubleCounter[T]();
      for {
        (ycI, v) <- vec.activeElements;
        yc = index.get(ycI)
      } {
        result(yc) = v;
      }
      result
    }

    override def toString = (
      "Elem(lUni" + decodeVector(leftUnigrams,charIndex) + ",\nlBi="
      + counts + ",\nl0Sc="
      + length0Score
      + totalProb + ",\nrUni="
      + decodeVector(rightUnigrams,charIndex)
    )

    def counts = {
      val result = LogPairedDoubleCounter[T,T]();
      for {
        (xcI, row) <- bigramCounts.iterator;
        xc = charIndex.get(xcI);
        (ycI, v) <- row.activeElements;
        yc = charIndex.get(ycI)
      } {
        result(xc,yc) = v;
      }
      result;

    }

    override lazy val hashCode = ScalaRunTime._hashCode(this);

    /*
     override def equals(o: Any) = o match {
     case null => false
     // close enough for gov't work
     case that: Elem => this.hashCode == that.hashCode && counts.size == that.counts.size && totalProb == that.totalProb
     case _ => false
     }
     */
  }


  private val epsilon = Alphabet.zeroEpsCharBet.epsilon;

  private def logAddInPlace2D(to: ArrayMap[SparseVector], from: ArrayMap[SparseVector], scale: Double=0.0) {
    if (scale != Double.NegativeInfinity) {
      for( (k,row) <- from) {
        if (!to.contains(k)) {
          val old = mkSparseVector;
          var offset = 0;
          while(offset < row.used) {
            val k =  row.index(offset);
            val v =  row.data(offset);
            old(k) = v+scale;
            offset += 1;
          }
          to(k) = old;
        } else {
        val old =  to(k);
          logAddInPlace(old,row,scale);
        }
      }
    }
  }

  private def logAdd(to: SparseVector,from: SparseVector, scale: Double=0.0) = {
    val ret = to.copy;
    logAddInPlace(ret,from,scale);
    ret;
  }

  private def logAddInPlace(to: SparseVector, from: SparseVector, scale: Double=0.0) {
    if (scale != Double.NegativeInfinity) {
      var offset = 0;
      // this just iterates over the array, but we can't pay the boxing penalty
      while(offset < from.used) {
        val k = from.index(offset);
        val v = from.data(offset);
        to(k) = logSum(to(k),v+scale);
        offset += 1;
      }
    }
  }

  private def mnorm(x: SparseVector, y: SparseVector): Boolean = {
    for(i <- x.activeDomain) {
      if(!Semiring.LogSpace.doubleIsLogSpace.closeTo(x(i),y(i))) return false
    }
    true
  }
  
  private def goodEnoughCloseTo(x: Double, y:Double) = {
    x == y || (x == 0 && y.abs < 5E-4) || ( ( (x-y)/x).abs < 5E-4);
  }

  implicit val ring: Semiring[Elem] = new Semiring[Elem] {

    override def closeTo(x: Elem, y: Elem) = {
      import Semiring.LogSpace.doubleIsLogSpace;
      val ret = goodEnoughCloseTo(x.totalProb,y.totalProb) &&
                (cheatOnEquals ||
                  mnorm(x.leftUnigrams,y.leftUnigrams)
                  && mnorm(x.rightUnigrams,y.rightUnigrams)
                 // && mnorm(x.bigramCounts,y.bigramCounts)
                )
      //if(!ret) println(x.totalProb,y.totalProb);
      ret
    }

    def plus(x: Elem, y: Elem) = {
      val newProb = logSum(x.totalProb,y.totalProb);

      val newBigrams = copyGramMap(x.bigramCounts);
      logAddInPlace2D(newBigrams,y.bigramCounts)

      val leftUnigrams = logAdd(x.leftUnigrams,y.leftUnigrams);
      val rightUnigrams = logAdd(x.rightUnigrams,y.rightUnigrams);

      val length0Score = logSum(x.length0Score,y.length0Score);

      Elem(leftUnigrams,  newBigrams,  length0Score,  newProb, rightUnigrams);
    }


    override def maybe_+=(x:Elem, y: Elem) = if(x.totalProb == Double.NegativeInfinity) (y,closeTo(zero,y)) else {
      import scalanlp.math.Semiring.LogSpace.doubleIsLogSpace
      if(doubleIsLogSpace.closeTo(x.totalProb,logSum(x.totalProb,y.totalProb))) {
        (x,true)
      } else {
        logAddInPlace(x.leftUnigrams,y.leftUnigrams);
        logAddInPlace2D(x.bigramCounts,y.bigramCounts);
        logAddInPlace(x.rightUnigrams,y.rightUnigrams);
        val newLength0Score = logSum(x.length0Score,y.length0Score);
        val newTotalProb = logSum(x.totalProb,y.totalProb);
        (x.copy(totalProb=newTotalProb,length0Score=newLength0Score),false);
      }
    }



    def times(x: Elem, y: Elem) = {
      val newProb = x.totalProb + y.totalProb;
      val active = mkSparseVector;

      val newBigrams = mkGramCharMap;
      logAddInPlace2D(newBigrams,x.bigramCounts,y.totalProb)
      logAddInPlace2D(newBigrams,y.bigramCounts,x.totalProb)

      // X Y --> Z (bigram)
      for( (yc,yprob) <- y.leftUnigrams.activeElements;
          (xc,xprob) <- x.rightUnigrams.activeElements) {
        newBigrams(xc)(yc) = logSum(newBigrams(xc)(yc), yprob + xprob);
      }

      // frontier:
      // the left unigram frontier consists of x's old left frontier, plus y's left frontier + x's empty score
      val leftUnigrams = mkSparseVector;
      leftUnigrams := x.leftUnigrams + y.totalProb;
      logAddInPlace(leftUnigrams,y.leftUnigrams,x.length0Score);
      // the right unigram frontier consists of y's old right frontier, plus x's right frontier + y's empty score
      val rightUnigrams = mkSparseVector;
      rightUnigrams := y.rightUnigrams + x.totalProb
      logAddInPlace(rightUnigrams,x.rightUnigrams,y.length0Score);

      val length0Score = x.length0Score + y.length0Score;

      val r = Elem(leftUnigrams,
                   newBigrams,
                   length0Score,
                   newProb,
                   rightUnigrams);
      r
    }

    def closure(x: Elem) = {
      import Semiring.LogSpace.doubleIsLogSpace.{closure=>logClosure};
      val p_* = logClosure(x.totalProb);

      val newBigrams = mkGramCharMap;
      logAddInPlace2D(newBigrams,x.bigramCounts);
      for( (xc,xprob) <- x.rightUnigrams.activeElements;
          (yc,yprob) <- x.leftUnigrams.activeElements) {
        newBigrams(xc)(yc) = logSum(newBigrams(xc)(yc), yprob + xprob);
      }
      val newBigrams2 = mkGramCharMap;
      logAddInPlace2D(newBigrams2,newBigrams,2 * p_*);


      // length0 score is the some of all paths only on epsilon
      val length0score = logClosure(x.length0Score);

      // now the frontier sets. basically, this is the same as for multiplication
      // except we scale by all that paths leading through (i.e. the closure)

      // the left unigram frontier consists of x's old left frontier scaled by p_*
      val leftUnigrams = x.leftUnigrams + p_*;
      // the right unigram frontier consists of x's old right frontier scaled by p_*
      val rightUnigrams = x.rightUnigrams + p_*;

      val r = Elem(leftUnigrams,
                   newBigrams2,
                   length0score,
                   p_*,
                   rightUnigrams);
      r
    }

    val one = Elem(mkSparseVector,mkGramCharMap ,0.0,0.0,mkSparseVector);
    val zero = Elem(mkSparseVector,mkGramCharMap ,-1.0/0.0,-1.0/0.0,mkSparseVector);
  }

  def promote[S](a: Arc[Double,S,T]) = {
    val counts = mkGramCharMap;
    val border = mkSparseVector;
    val active = mkSparseVector
    if (a.label != implicitly[Alphabet[T]].epsilon) {
      val id = charIndex(a.label);
      border(id) = a.weight;
      // It can only be a length-1-spanning character
      // if the char can be a history character
      if (isAcceptableHistoryChar(id)) {
        active(id) = a.weight;
      }
    }
    val nonceScore = if (a != implicitly[Alphabet[T]].epsilon) {
      Double.NegativeInfinity;
    } else {
      a.weight;
    }
    Elem(leftUnigrams = border,
         bigramCounts = counts,
         length0Score = nonceScore,
         totalProb = a.weight,
         rightUnigrams = active
         );
  }

  def promoteOnlyWeight(w: Double) = if(w == Double.NegativeInfinity) ring.zero else {
    val counts = mkGramCharMap;
    val active = mkSparseVector;
    active(beginningUnigramId) = w;
    Elem(active,counts,Double.NegativeInfinity,w,active);
  }

}