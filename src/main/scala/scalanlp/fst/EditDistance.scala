package scalanlp.fst;

import scalanlp.math.Semiring.LogSpace._;

/**
* Levhenstein transducer over sum of alignments (not viterbi
* alignment) with the given parameters, which must be less than 0.
*
* A match is assumed to be 0.0, though these will be rescaled to ensure
* that the edit distance encodes a (log space) probability distribution, or at
* least that conditioned on either the input or the output, there is a distribution. 
* logDecay (&lt;=0.0) adds a preference for the length of the alignments.
* That is, the scores will be shifted by some constant
*
* 
*
* @author dlwh
*/
class EditDistance( subRatio: Double, insRatio: Double, alphabet: Set[Char], logDecay: Double = 0.0) 
    extends Transducer[Double,Unit,Char,Char]()(doubleIsLogSpace,implicitly[Alphabet[Char]],implicitly[Alphabet[Char]]) {
  import Transducer._;
  require( subRatio < 0);
  require( insRatio < 0);
  require( logDecay <= 0);

  val (insCost,subCost,matchCost) = {
    import Math.{exp,log};
    val n = alphabet.size
    // we have n^2-n subs, n matches, and 2n dels == insertions.
    // We want transitions to be markovian, so:
    //  c * ((n^2 - n ) exp(sub) + n * exp(0) + 2n exp(del)) = decay
    // log c + log ((n^2 - n ) exp(sub) + n * exp(0) + 2n exp(del)) = log(decay)
    val logC = logDecay - log( (n*n - n) * exp(subRatio) + n + 2 * n * exp(insRatio))
    (insRatio + logC,subRatio + logC,logC)
  }

  val initialStateWeights = Map( () -> 0.0);

  def finalWeight(s: Unit) = 0.0;
  val epsilon = inAlpha.epsilon;
  val alphabetSeq = alphabet.toSeq;

  override def allEdges:Seq[Arc] = edgesMatching((),inAlpha.sigma,outAlpha.sigma).toSeq;

  def edgesMatching(s: Unit, a: Char, b: Char) = {
    if(a == inAlpha.sigma && b == outAlpha.sigma) {
      val subs = for(a <- alphabet.iterator;
          b <- alphabet.iterator)
        yield Arc((),(),a,b, if(a != b) subCost else matchCost);
      val dels = for(a <- alphabet.iterator)
                   yield Arc((),(),a,outAlpha.epsilon, insCost);
      val ins = for(a <- alphabet.iterator)
                   yield Arc((),(),inAlpha.epsilon,a, insCost);
      subs ++ dels ++ ins
    } else if(a == inAlpha.sigma) {
      if(b == outAlpha.epsilon) {
        for(a <- alphabet.iterator)
          yield Arc((),(),a,b,insCost);
      } else {
        (for(a <- alphabet.iterator)
          yield Arc((),(),a,b,if(a != b) subCost else matchCost) ) ++
        Iterator.single(Arc((),(),inAlpha.epsilon,b,insCost))
      }
    } else if(b == outAlpha.sigma) {
      if(a == inAlpha.epsilon) {
        for(b <- alphabet iterator)
          yield Arc((),(),a,b,insCost);
      } else {
        ((for(b <- alphabet iterator)
          yield Arc((),(),a,b,if(a != b) subCost else matchCost)))  ++
          Iterator.single(Arc((),(),a,outAlpha.epsilon,insCost))
      }
    } else if(a == b && b == inAlpha.epsilon) {
      Iterator.empty;
    } else {
      val cost = {
        if(a == inAlpha.epsilon || b == inAlpha.epsilon)
          insCost
        else if (a != b)
          subCost
        else matchCost;
      }
      Iterator.single(Arc((),(),a,b,cost));
    }
  }
}

