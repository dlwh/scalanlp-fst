package scalanlp.fst;

import scalanlp.math._;
import Numerics.logSum;
import scalala.Scalala._;
import scalala.tensor.sparse._;
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
  val gramIndex = Index[Bigram]();
  for( (a,b) <- acceptableChars) {
    charIndex(encode(a,b));
  }
  val maxAcceptableGram = charIndex.size;

  def isAcceptableHistoryChar(gI: Int) = gI < maxAcceptableGram;

  private def mkGramCharMap = new ArrayMap[SparseVector] {
    override def default(k: Int) = getOrElseUpdate(k,mkSparseVector);
  }

  private def mkSparseVector = {
    val r = new SparseVector(Int.MaxValue);
    r.default = Double.NegativeInfinity;
    r
  }

  // Yuck.
  case class Elem(leftUnigrams: SparseVector, 
                  leftBigrams: SparseVector,
                  counts: ArrayMap[SparseVector],
                  length0Score: Double,
                  length1Chars: SparseVector,
                  totalProb: Double,
                  rightUnigrams: SparseVector,
                  rightBigrams: SparseVector) {
    def decode = {
      val result = LogPairedDoubleCounter[Bigram,EncodedChars]();
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

  val beginningUnigramId = charIndex(beginningUnigram)
  val beginningBigramId = gramIndex(beginningBigram)

  private val epsilon = Alphabet.zeroEpsCharBet.epsilon;

  private def logAddInPlace2D(to: ArrayMap[SparseVector], from: ArrayMap[SparseVector], scale: Double=0.0) {
    if(scale != Double.NegativeInfinity) {
      for( (k,row) <- from) {
        val old = to(k);
        if(old.activeDomain.size == 0) {
          old := row + scale;
        } else {
          logAddInPlace(old,row,scale);
        }
      }
    }
  }

  private def logAdd(to: SparseVector, from: SparseVector, scale: Double=0.0) = {
    val ret = to.copy;
    logAddInPlace(ret,from,scale);
    ret;
  }


  private def logAddInPlace(to: SparseVector, from: SparseVector, scale: Double=0.0) {
    if(scale != Double.NegativeInfinity)
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

      val leftUnigrams = logAdd(x.leftUnigrams,y.leftUnigrams);
      val leftBigrams = logAdd(x.leftBigrams,y.leftBigrams);
      val rightUnigrams = logAdd(x.rightUnigrams,y.rightUnigrams);
      val rightBigrams = logAdd(x.rightBigrams,y.rightBigrams);

      val length1Chars = logAdd(x.length1Chars,y.length1Chars);
      val length0Score = logSum(x.length0Score,y.length0Score);

      val r = Elem(leftUnigrams, leftBigrams, newCounts, length0Score, length1Chars, newProb, rightUnigrams, rightBigrams);
      r
    }


    def times(x: Elem, y: Elem) = {
      val newProb = x.totalProb + y.totalProb;
      val active = mkSparseVector;

      val newCounts = mkGramCharMap;
      logAddInPlace2D(newCounts,x.counts,y.totalProb);
      logAddInPlace2D(newCounts,y.counts,x.totalProb);

      // XX Y --> ZZZ (trigram)
      for( (yc,yprob) <- y.leftUnigrams.activeElements;
           (xc,xprob) <- x.rightBigrams.activeElements) {
        newCounts(xc)(yc) = logSum(newCounts(xc)(yc), yprob + xprob);
      }

      // X YY --> ZZZ (trigram)
      for( (yc,yprob) <- y.leftBigrams.activeElements;
           (xc,xprob) <- x.rightUnigrams.activeElements) {
        val Bigram(y1,y2) = gramIndex.get(yc);
        val newBG = gramIndex(Bigram(charIndex.get(xc),y1))
        val encodedY2 = charIndex(y2);
        newCounts(newBG)(encodedY2) = logSum(newCounts(newBG)(encodedY2), yprob + xprob);
      }

      // New active set is:
      // sum of all ways to traverse x with length 1 + traverse y with length 0
      // sum of all ways to traverse y with length 1 + traverse x with length 0
      val activeChars = (x.length1Chars + y.length0Score).value;
      logAddInPlace(activeChars,y.length1Chars,x.length0Score);

      // frontier:
      // the left unigram frontier consists of x's old left frontier, plus y's left frontier + x's empty score
      val leftUnigrams = logAdd(x.leftUnigrams,y.leftUnigrams,x.length0Score);
      // the right unigram frontier consists of y's old right frontier, plus x's right frontier + y's empty score
      val rightUnigrams = logAdd(y.rightUnigrams,x.rightUnigrams,y.length0Score);

      // analagously for bigrams, except we create new bigrams based on active sets
      val leftBigrams = logAdd(x.leftBigrams,y.leftBigrams,x.length0Score);
      val rightBigrams = logAdd(y.rightBigrams,x.rightBigrams,y.length0Score);

      for( (yc,yprob) <- y.length1Chars.activeElements;
           (xc,xprob) <- x.rightUnigrams.activeElements) {
        val newBG = gramIndex(Bigram(charIndex.get(xc),charIndex.get(yc)));
        rightBigrams(newBG) = logSum(leftBigrams(newBG),xprob + yprob);
      }

      for {
        (xc,xprob) <- x.length1Chars.activeElements;
        (yc,yprob) <- y.leftUnigrams.activeElements
        if isAcceptableHistoryChar(yc)
      } {
        val newBG = gramIndex(Bigram(charIndex.get(xc),charIndex.get(yc)));
        leftBigrams(newBG) = logSum(rightBigrams(newBG),xprob + yprob);
      }

      val length0Score = x.length0Score + y.length0Score;

      val r = Elem(leftUnigrams, leftBigrams,
                   newCounts,length0Score, activeChars,
                   newProb, 
                   rightUnigrams, rightBigrams);
      r
    }

    def closure(x: Elem) = {
      import Semiring.LogSpace.doubleIsLogSpace.{closure=>logClosure};
      val p_* = logClosure(x.totalProb);

      // This is really confusing. For starters, we keep all our old counts
      val newCounts = mkGramCharMap;
      logAddInPlace2D(newCounts,x.counts)
      // We now need to create a new trigram for each
      // right unigram <length 1 unigram> <left unigram>
      for( (mid,midscore) <- x.length1Chars.activeElements;
           (beg,begscore) <- x.rightUnigrams.activeElements;
           (end,endscore) <- x.leftUnigrams.activeElements) {
        val newBG = gramIndex(Bigram(charIndex.get(beg),charIndex.get(mid)));
        newCounts(newBG)(end) = logSum(newCounts(newBG)(end),begscore + midscore + endscore);
      }
      // now we need a trigram for each right bigram and left unigram
      for( (xc,xprob) <- x.rightBigrams.activeElements;
          (yc,yprob) <- x.leftUnigrams.activeElements) {
        newCounts(xc)(yc) = logSum(newCounts(xc)(yc), yprob + xprob);
      }

      // now we need a trigram for each right unigram and left bigram
      for( (yc,yprob) <- x.leftBigrams.activeElements;
           (xc,xprob) <- x.rightUnigrams.activeElements) {
        val Bigram(y1,y2) = gramIndex.get(yc);
        val newBG = gramIndex(Bigram(charIndex.get(xc),y1))
        val encodedY2 = charIndex(y2);
        newCounts(newBG)(encodedY2) = logSum(newCounts(newBG)(encodedY2), yprob + xprob);
      }
      // Now we need to scale by 2 * p_*
      val newCounts2 = mkGramCharMap;
      logAddInPlace2D(newCounts2,newCounts,2 * p_*);


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
        rightBigrams(newBG) = logSum(rightBigrams(newBG),xprob + yprob);
      }

      for {
        (xc,xprob) <- length1.activeElements
        (yc,yprob) <- x.leftUnigrams.activeElements
        if isAcceptableHistoryChar(yc)
      } {
        val newBG = gramIndex(Bigram(charIndex.get(xc),charIndex.get(yc)));
        leftBigrams(newBG) = logSum(leftBigrams(newBG),xprob + yprob);
      }


      val r = Elem(leftUnigrams, leftBigrams,
                   newCounts2,length0score, length1,
                   p_*, 
                   rightUnigrams, rightBigrams);
      r
    }

    val one = Elem(mkSparseVector,mkSparseVector,mkGramCharMap,0.0,mkSparseVector,0.0,mkSparseVector,mkSparseVector);
    val zero = Elem(mkSparseVector,mkSparseVector,mkGramCharMap,-1.0/0.0,mkSparseVector,-1.0/0.0,mkSparseVector,mkSparseVector);
  }

  def promote[S](a: Arc[Double,S,Char,Char]) = {
    val counts = mkGramCharMap;
    val border = mkSparseVector;
    val active = mkSparseVector
    if(a.in != epsilon || a.out != epsilon) {
      val id = charIndex(encode(a.in,a.out));
      border(id) = a.weight;
      // It can only be a length-1-spanning character
      // if the char can be a history character
      if(isAcceptableHistoryChar(id)) {
        active(id) = a.weight; 
      }
    } 
    val nonceScore = if(a.in != epsilon || a.out != epsilon) {
      Double.NegativeInfinity;
    } else {
      a.weight;
    }
    Elem(leftUnigrams = border, leftBigrams = mkSparseVector,
         counts = counts,
         length0Score = nonceScore, length1Chars = active,
         totalProb = a.weight,
         rightUnigrams = active,
         rightBigrams = mkSparseVector);
  }

  def promoteOnlyWeight(w: Double) = {
    val counts = mkGramCharMap;
    val active = mkSparseVector;
    val activeBG = mkSparseVector
    active(beginningUnigramId) = w;
    activeBG(beginningBigramId) = w + w;
    val l1active = mkSparseVector;
    Elem(active,activeBG,counts,Double.NegativeInfinity,l1active,w,active,activeBG);
  }

}

object TrigramSemiring {
  type EncodedChars = (Char,Char)

  //def encode(ch1: Char, ch2: Char) = ((ch1.toInt << 16)|(ch2)):EncodedChars
  def encode(ch1: Char, ch2: Char) = (ch1,ch2);
  def encodeOne(ch: Char) = encode(ch,ch);
  //def decode(cc: EncodedChars) = ((cc >> 16).toChar,(cc & 0XFFFF).toChar);
  def decode(cc: EncodedChars):(Char,Char) = cc;

  case class Bigram(_1: EncodedChars, _2:EncodedChars) {
    def length = 2;
    override val hashCode = _1.hashCode * 37 + _2.hashCode;
  }

  // for Automata, we can pass in just chars
  object Bigram {
    def apply(ch1: Char, ch2: Char) = new Bigram(encodeOne(ch1),encodeOne(ch2));
  }

  val beginningUnigram = encode('#','#');
  val beginningBigram = Bigram(encode('#','#'),encode('#','#'));
}
