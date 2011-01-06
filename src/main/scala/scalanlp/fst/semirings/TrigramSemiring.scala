package scalanlp.fst;
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

import scalanlp.math._;
import scala.runtime.ScalaRunTime;
import scalala.Scalala._;
import scalala.tensor.sparse._;
import scalanlp.collection.mutable.SparseArray;
import scalala.tensor.counters.LogCounters.{logSum => _, _};

import scalanlp.util.Index;

/**
 * Encodes the sufficient statistics for a trigram model of an automaton.
 * 
 *
 * @param acceptableChars : only learn bigram histories that contain (only) these chars
 * @param acceptableBigrams: only learn bigrams histories that are these bigrams.
 */
class TrigramSemiring[@specialized(Char) T:Alphabet](acceptableChars: Set[T],
                                                       acceptableBigrams: Set[(T,T)],
                                                       beginningUnigram: T,
                                                       cheatOnEquals: Boolean=false) {
  import TrigramSemiring._;

  val charIndex = Index[T]();
  charIndex.index(beginningUnigram);
  for( ab <- acceptableChars) {
    charIndex.index(ab);
  }
  val maxAcceptableChar = charIndex.size;

  val gramIndex = Index[Bigram[T]]();
  for( ch <- acceptableChars) {
    val bg1 = Bigram(ch,beginningUnigram);
    val bg2 = Bigram(beginningUnigram,ch);
    gramIndex.index(bg1);
    gramIndex.index(bg2);
  }
  for( (a,b) <- acceptableBigrams) {
    gramIndex.index(new Bigram(a,b));
  }
  val maxAcceptableGram = gramIndex.size;

  def isAcceptableHistoryChar(gI: Int) = gI < maxAcceptableChar && gI >= 0;
  def isAcceptableBigram(gI: Int) = gI < maxAcceptableGram && gI >= 0;

  private def mkGramCharMap = new SparseArray[SparseVector](Int.MaxValue,mkSparseVector,0);

  private def copyGramMap(other: SparseArray[SparseVector]) = {
    val ret = new SparseArray[SparseVector](other.length,mkSparseVector,other.size);
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

  // Yuck.
  case class Elem(leftUnigrams: SparseVector,
                  leftBigrams: SparseVector,
                  bigramCounts: SparseArray[SparseVector],
                  trigramCounts: SparseArray[SparseVector],
                  length0Score: Double,
                  length1Chars: SparseVector,
                  totalProb: Double,
                  rightUnigrams: SparseVector,
                  rightBigrams: SparseVector) {

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
      + decodeVector(leftBigrams,gramIndex) + ",\nbiCnt="
      + decodeBigrams + ",\ntriCnt="
      + decode + ",\nl0Sc="
      + length0Score + ",\nln1Ch="
      + decodeVector(length1Chars,charIndex)+ ",\nprob="
      + totalProb + ",\nrUni="
      + decodeVector(rightUnigrams,charIndex)+ ",\nrBi="
      + decodeVector(rightBigrams,gramIndex)+ ")\n"
    )
    /**
    * Returns a counter of the conditional trigrams we learned.
    */ 
    def decode = {
      val result = LogPairedDoubleCounter[Bigram[T],T]();
      for {
        (xcI,row) <- trigramCounts.iterator
        xc = gramIndex.get(xcI);
        (ycI,v) <- row.activeElements
        yc = charIndex.get(ycI)
      } {
        result(xc,yc) = v; 
      }
      result
    }

    def decodeBigrams = {
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

  val beginningUnigramId = charIndex(beginningUnigram)

  private val epsilon = Alphabet.zeroEpsCharBet.epsilon;

  private def logAddInPlace2D(to: SparseArray[SparseVector], from: SparseArray[SparseVector], scale: Double=0.0) {
    if (scale != Double.NegativeInfinity) {
      for( (k,row) <- from) {
        val old = to.getOrElseUpdate(k);
        if (old.activeDomain.size == 0) {
          var offset = 0;
          while(offset < row.used) {
            val k =  row.index(offset);
            val v =  row.data(offset);
            old(k) = v+scale;
            offset += 1;
          }
        } else {
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

  implicit val ring: Semiring[Elem] = new Semiring[Elem] {

    override def closeTo(x: Elem, y: Elem) = {
      import Semiring.LogSpace.doubleIsLogSpace;
      val ret = doubleIsLogSpace.closeTo(x.totalProb, y.totalProb) &&
                (cheatOnEquals || mnorm(x.leftBigrams,y.leftBigrams)
                  && mnorm(x.leftUnigrams,y.leftUnigrams)
                  && mnorm(x.rightUnigrams,y.rightUnigrams)
                  && mnorm(x.rightBigrams,y.rightBigrams)
                 // && mnorm(x.bigramCounts,y.bigramCounts)
                )
      ret
    }

    def plus(x: Elem, y: Elem) = {
      val newProb = logSum(x.totalProb,y.totalProb);
      
      val newTrigrams = copyGramMap(x.trigramCounts);
      logAddInPlace2D(newTrigrams,y.trigramCounts)


      val newBigrams = copyGramMap(x.bigramCounts);
      logAddInPlace2D(newBigrams,y.bigramCounts)

      val leftUnigrams = logAdd(x.leftUnigrams,y.leftUnigrams);
      val leftBigrams = logAdd(x.leftBigrams,y.leftBigrams);
      val rightUnigrams = logAdd(x.rightUnigrams,y.rightUnigrams);
      val rightBigrams = logAdd(x.rightBigrams,y.rightBigrams);

      val length1Chars = logAdd(x.length1Chars,y.length1Chars);
      val length0Score = logSum(x.length0Score,y.length0Score);

      Elem(leftUnigrams, leftBigrams, newBigrams, newTrigrams, length0Score, length1Chars, newProb, rightUnigrams, rightBigrams);
    }


    def times(x: Elem, y: Elem) = {
      val newProb = x.totalProb + y.totalProb;
      val active = mkSparseVector;

      val newTrigrams = mkGramCharMap;
      logAddInPlace2D(newTrigrams,x.trigramCounts,y.totalProb);
      logAddInPlace2D(newTrigrams,y.trigramCounts,x.totalProb);

      // XX Y --> ZZZ (trigram)
      for( (yc,yprob) <- y.leftUnigrams.activeElements;
          (xc,xprob) <- x.rightBigrams.activeElements) {
        newTrigrams.getOrElseUpdate(xc)(yc) = logSum(newTrigrams.getOrElseUpdate(xc)(yc), yprob + xprob);
      }

      // X YY --> ZZZ (trigram)
      for( (yc,yprob) <- y.leftBigrams.activeElements;
          (xc,xprob) <- x.rightUnigrams.activeElements) {
        val Bigram(y1,y2) = gramIndex.get(yc);
        val newBG = gramIndex(Bigram(charIndex.get(xc),y1));
        if (isAcceptableBigram(newBG)) {
          val encodedY2 = charIndex(y2);
          newTrigrams.getOrElseUpdate(newBG)(encodedY2) = logSum(newTrigrams.getOrElseUpdate(newBG)(encodedY2), yprob + xprob);
        }
      }

      val newBigrams = mkGramCharMap;
      logAddInPlace2D(newBigrams,x.bigramCounts,y.totalProb)
      logAddInPlace2D(newBigrams,y.bigramCounts,x.totalProb)

      // X Y --> Z (bigram)
      for( (yc,yprob) <- y.leftUnigrams.activeElements;
          (xc,xprob) <- x.rightUnigrams.activeElements) {
        newBigrams.getOrElseUpdate(xc)(yc) = logSum(newBigrams.getOrElseUpdate(xc)(yc), yprob + xprob);
      }

      // New active set is:
      // sum of all ways to traverse x with length 1 + traverse y with length 0
      // sum of all ways to traverse y with length 1 + traverse x with length 0
      val activeChars = (x.length1Chars + y.length0Score).value;
      logAddInPlace(activeChars,y.length1Chars,x.length0Score);

      // frontier:
      // the left unigram frontier consists of x's old left frontier, plus y's left frontier + x's empty score
      val leftUnigrams = mkSparseVector;
      leftUnigrams := x.leftUnigrams + y.totalProb;
      logAddInPlace(leftUnigrams,y.leftUnigrams,x.length0Score);
      // the right unigram frontier consists of y's old right frontier, plus x's right frontier + y's empty score
      val rightUnigrams = mkSparseVector;
      rightUnigrams := y.rightUnigrams + x.totalProb
      logAddInPlace(rightUnigrams,x.rightUnigrams,y.length0Score);

      // analagously, except we create extra length things, and such.
      val leftBigrams = mkSparseVector;
      leftBigrams := x.leftBigrams + y.totalProb;
      logAddInPlace(leftBigrams,y.leftBigrams,x.length0Score);
      val rightBigrams = mkSparseVector;
      rightBigrams := y.rightBigrams + x.totalProb
      logAddInPlace(rightBigrams,x.rightBigrams,y.length0Score);

      for( (yc,yprob) <- y.length1Chars.activeElements;
          (xc,xprob) <- x.rightUnigrams.activeElements) {
        val newBG = gramIndex(Bigram(charIndex.get(xc),charIndex.get(yc)));
        if(isAcceptableBigram(newBG)) {
          rightBigrams(newBG) = logSum(rightBigrams(newBG),xprob + yprob);
        }
      }

      for {
        (xc,xprob) <- x.length1Chars.activeElements;
        (yc,yprob) <- y.leftUnigrams.activeElements
        if isAcceptableHistoryChar(xc)
      } {
        // we have to accept any leftbigram we get (unless xc isn't an acceptableChar, which is ensured earlier)
        val newBG = gramIndex(Bigram(charIndex.get(xc),charIndex.get(yc)));
        if(isAcceptableBigram(newBG))
          leftBigrams(newBG) = logSum(leftBigrams(newBG),xprob + yprob);
      }

      val length0Score = x.length0Score + y.length0Score;

      val r = Elem(leftUnigrams, leftBigrams,
                   newBigrams,
                   newTrigrams,
                   length0Score, activeChars,
                   newProb, 
                   rightUnigrams, rightBigrams);
      r
    }

    def closure(x: Elem) = {
      import Semiring.LogSpace.doubleIsLogSpace.{closure=>logClosure};
      val p_* = logClosure(x.totalProb);

      // This is really confusing. For starters, we keep all our old counts
      val newTrigrams = copyGramMap(x.trigramCounts);
      // We now need to create a new trigram for each
      // right unigram <length 1 unigram> <left unigram>
      for( (mid,midscore) <- x.length1Chars.activeElements;
          (beg,begscore) <- x.rightUnigrams.activeElements;
          (end,endscore) <- x.leftUnigrams.activeElements) {
        val newBG = gramIndex(Bigram(charIndex.get(beg),charIndex.get(mid)));
        if (isAcceptableBigram(newBG)) {
          newTrigrams.getOrElseUpdate(newBG)(end) = logSum(newTrigrams(newBG)(end),begscore + midscore + endscore);
        }
      }
      // now we need a trigram for each right bigram and left unigram
      for( (xc,xprob) <- x.rightBigrams.activeElements;
          (yc,yprob) <- x.leftUnigrams.activeElements) {
        newTrigrams.getOrElseUpdate(xc)(yc) = logSum(newTrigrams.getOrElseUpdate(xc)(yc), yprob + xprob);
      }

      // now we need a trigram for each right unigram and left bigram
      for( (yc,yprob) <- x.leftBigrams.activeElements;
          (xc,xprob) <- x.rightUnigrams.activeElements) {
        val Bigram(y1,y2) = gramIndex.get(yc);
        val newBG = gramIndex(Bigram(charIndex.get(xc),y1))
        if (isAcceptableBigram(newBG)) {
          val encodedY2 = charIndex(y2);
          newTrigrams.getOrElseUpdate(newBG)(encodedY2) = logSum(newTrigrams.getOrElseUpdate(newBG)(encodedY2), yprob + xprob);
        }
      }
      // Now we need to scale by 2 * p_*
      val newTrigrams2 = mkGramCharMap;
      logAddInPlace2D(newTrigrams2,newTrigrams,2 * p_*);

      //bigrams: basically, the same idea, just a lot easier
      val newBigrams = mkGramCharMap;
      logAddInPlace2D(newBigrams,x.bigramCounts);
      for( (xc,xprob) <- x.rightUnigrams.activeElements;
          (yc,yprob) <- x.leftUnigrams.activeElements) {
        newBigrams.getOrElseUpdate(xc)(yc) = logSum(newBigrams.getOrElseUpdate(xc)(yc), yprob + xprob);
      }
      val newBigrams2 = mkGramCharMap;
      logAddInPlace2D(newBigrams2,newBigrams,2 * p_*);


      // length0 score is the some of all paths only on epsilon
      val length0score = logClosure(x.length0Score);

      // the length1 chars are the same set, but scaled by the new length0score
      // (length1 is the weight of paths that contain only 1 real character
      val length1 = (x.length1Chars + length0score).value;

      // now the frontier sets. basically, this is the same as for multiplication
      // except we scale by all that paths leading through (i.e. the closure)

      // the left unigram frontier consists of x's old left frontier scaled by p_*
      val leftUnigrams = x.leftUnigrams + p_*;
      // the right unigram frontier consists of x's old right frontier scaled by p_*
      val rightUnigrams = x.rightUnigrams + p_*;

      // analagously for bigrams, except we create new bigrams based on active sets
      val leftBigrams = x.leftBigrams + p_*
      val rightBigrams = x.rightBigrams + p_*;

      for( (yc,yprob) <- length1.activeElements;
          (xc,xprob) <- x.rightUnigrams.activeElements) {
        val newBG = gramIndex(Bigram(charIndex.get(xc),charIndex.get(yc)));
        if(isAcceptableBigram(newBG))
          rightBigrams(newBG) = logSum(rightBigrams(newBG),xprob + yprob);
      }

      for {
        (xc,xprob) <- length1.activeElements
        (yc,yprob) <- x.leftUnigrams.activeElements
        if isAcceptableHistoryChar(yc)
      } {
        val newBG = gramIndex.index(Bigram(charIndex.get(xc),charIndex.get(yc)));
        if(isAcceptableBigram(newBG))
          leftBigrams(newBG) = logSum(leftBigrams(newBG),xprob + yprob);
      }


      val r = Elem(leftUnigrams, leftBigrams,
                   newBigrams2, newTrigrams2,
                   length0score, length1,
                   p_*, 
                   rightUnigrams, rightBigrams);
      r
    }

    val one = Elem(mkSparseVector,mkSparseVector,mkGramCharMap, mkGramCharMap,0.0,mkSparseVector,0.0,mkSparseVector,mkSparseVector);
    val zero = Elem(mkSparseVector,mkSparseVector,mkGramCharMap, mkGramCharMap,-1.0/0.0,mkSparseVector,-1.0/0.0,mkSparseVector,mkSparseVector);
  }

  def promote[S](a: Arc[Double,S,T]) = {
    val counts = mkGramCharMap;
    val border = mkSparseVector;
    val active = mkSparseVector
    if (a.label != implicitly[Alphabet[T]].epsilon) {
      val id = charIndex.index(a.label);
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
    Elem(leftUnigrams = border, leftBigrams = mkSparseVector,
         bigramCounts = counts,
         trigramCounts = counts,
         length0Score = nonceScore, length1Chars = active,
         totalProb = a.weight,
         rightUnigrams = active,
         rightBigrams = mkSparseVector);
  }

  def promoteOnlyWeight(w: Double) = if(w == Double.NegativeInfinity) ring.zero else {
    val counts = mkGramCharMap;
    val active = mkSparseVector;
    val activeBG = mkSparseVector
    active(beginningUnigramId) = w;
    //activeBG(beginningBigramId) = w + w;
    val l1active = mkSparseVector;
    Elem(active,activeBG,counts,counts,Double.NegativeInfinity,l1active,w,active,activeBG);
  }

}

object TrigramSemiring {

  case class Bigram[ T](_1: T, _2:T) {
    def length = 2;
    override val hashCode = _1.hashCode * 37 + _2.hashCode;
  }

  val beginningUnigram = '#';
}
