package scalanlp.fst;

import scalanlp.math.Semiring.LogSpace._;

/**
* Levhenstein transducer over sum of alignments (not viterbi
* alignment) with the given parameters, which must be less than 0.
*
* A match is assumed to be 0.0, though these will be rescaled to ensure
* that the edit distance encodes a (log space) probability distribution.
* That is, the scores will be shifted by some constant
*/
class EditDistance( subRatio: Double, insRatio: Double, alphabet: Set[Char]) 
    extends Transducer[Double,Unit,Char,Char]()(doubleIsLogSpace,implicitly[Alphabet[Char]],implicitly[Alphabet[Char]]) {
  import Transducer._;
  require( subRatio < 0);
  require( insRatio < 0);

  val (insCost,subCost,matchCost) = {
    import Math.{exp,log};
    val n = alphabet.size
    // we have n^2-n subs, n matches, and 2n dels == insertions.
    // We want transitions to be markovian, so:
    //  c * ((n^2 - n ) exp(sub) + n * exp(0) + 2n exp(del)) = 1
    // log c + log ((n^2 - n ) exp(sub) + n * exp(0) + 2n exp(del)) = 0
    val logC = - log( (n*n - n) * exp(subRatio) + n + 2 * n * exp(insRatio))
    (insRatio + logC,subRatio + logC,logC)
  }

  val initialStateWeights = Map( () -> 0.0);

  def finalWeight(s: Unit) = 0.0;
  val epsilon = inAlpha.epsilon;
  val alphabetSeq = alphabet.toSeq;

  override val allEdges:Seq[Arc] = {
    val subs = for(a <- alphabetSeq;
        b <- alphabetSeq)
      yield Arc((),(),a,b,if(a != b) subCost else matchCost);
    val dels = for(a <- alphabetSeq iterator)
      yield Arc((),(),a,`epsilon`,insCost);
    val dels2 = for(a <- alphabetSeq iterator)
      yield Arc((),(),`epsilon`,a,insCost);

    subs ++ dels ++ dels2;
  }

  def edgesMatching(s: Unit, a: Char, b: Char) = {
    if(a == inAlpha.sigma && b == outAlpha.sigma) {
      allEdges
    } else if(a == inAlpha.sigma) {
      if(b == outAlpha.epsilon) {
        for(a <- alphabetSeq)
          yield Arc((),(),a,b,insCost);
      } else {
        (for(a <- alphabetSeq)
          yield Arc((),(),a,b,if(a != b) subCost else matchCost) ) ++
        Seq(Arc((),(),inAlpha.epsilon,b,insCost))
      }
    } else if(b == outAlpha.sigma) {
      if(a == inAlpha.epsilon) {
        for(b <- alphabetSeq)
          yield Arc((),(),a,b,insCost);
      } else {
        ((for(b <- alphabetSeq)
          yield Arc((),(),a,b,if(a != b) subCost else matchCost)) : Seq[Arc]) ++
          Seq(Arc((),(),a,outAlpha.epsilon,insCost))
      }
    } else if(a == b && b == inAlpha.epsilon) {
      Seq.empty;
    } else {
      val cost = {
        if(a == inAlpha.epsilon || b == inAlpha.epsilon)
          insCost
        else if (a != b)
          subCost
        else matchCost;
      }
      Seq(Arc((),(),a,b,cost));
    }
  }
}

