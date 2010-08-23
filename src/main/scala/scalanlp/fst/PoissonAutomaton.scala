package scalanlp.fst;

import scalanlp.math.Semiring.LogSpace._;
import scalanlp.stats.sampling.Poisson;

/**
* Generates a poisson distribution over string lengths, and a uniform
* distribution over individual characters, up to some maximum length.
* Afterwards, it is a geometric distrbution.
*
* Uses log probabilities.
*
* @author dlwh
*/
class PoissonAutomaton(val expectedLength: Double,
                       maxLength: Int, chars: Set[Char], geometricStoppingProb: Double = 0.9) extends Automaton[Double,Int,Char] {
  require( expectedLength > 0);

  private val poi = new Poisson(expectedLength);
  private val arcCost = -math.log(chars.size);
  private val leftOverProb = scalanlp.math.Numerics.logDiff(0.0,poi.logCdf(maxLength-1));

  val initialStateWeights = Map(0 -> 0.0);
  def finalWeight(s: Int) = if(s < maxLength) poi.logProbabilityOf(s) else leftOverProb + math.log(geometricStoppingProb);

  def edgesMatching(s: Int, a: Char) = {
    if(s < maxLength) {
      if(a == alphabet.sigma) {
        for(a <- chars.iterator) yield Arc(s,s+1,a,arcCost)
      } else if(a != alphabet.epsilon)
        Iterator.single(Arc(s,s+1,a,arcCost))
      else Iterator.empty;
    } else if(s == maxLength) {
      val arcCost = leftOverProb + math.log(1-geometricStoppingProb) - math.log(chars.size);
      if(a == alphabet.sigma) {
        for(a <- chars.iterator) yield Arc(s,s,a,arcCost)
      } else if(a != alphabet.epsilon)
        Iterator.single(Arc(s,s,a,arcCost))
      else Iterator.empty;

    } else Iterator.empty;
  }
}

