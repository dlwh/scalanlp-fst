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
class DecayAutomaton( val expectedLength:Double, chars: Set[Char], rhoSize: Int = 0) 
    extends Automaton[Double,Int,Char]()(doubleIsLogSpace,implicitly[Alphabet[Char]]) {
  import Transducer._;
  require( expectedLength > 0);
  // E[X] = p / (1-p)
  // p = E[X] / (1+E[X])
  val mass = Math.log(expectedLength / (1+expectedLength));

  override lazy val cost = doubleIsLogSpace.closure(mass);

  val arcCost = {
    val n = chars.size + rhoSize;
    // we have n emissions
    // We want transitions to be markovian, so:
    //  c * n = exp(mass)
    mass - Math.log(n)
  }

  val initialStateWeights = Map( 0 -> 0.0);

  def finalWeight(s: Int) = 0.0;

  override val allEdges:Seq[Arc] = {
    val subs = for(a <- chars.toSeq)
      yield Arc(0,0,a,a,arcCost)
    subs ++ Iterator.single(Arc(0,0,inAlpha.rho,inAlpha.rho,Math.log(rhoSize) + arcCost))
  }

  def edgesMatching(s: Int, a: Char) = {
    if(a == inAlpha.sigma)
      allEdges.iterator
    else if(a == inAlpha.rho)
      Iterator.single(Arc(0,0,a,a,Math.log(rhoSize) + arcCost));
    else Iterator.single(Arc(0,0,a,a,arcCost))
  }
}

