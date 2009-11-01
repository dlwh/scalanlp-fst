package scalanlp.fst;

import scalanlp.math.Semiring.LogSpace._;

/**
* Generates a geometric distribution over string lengths, and a uniform
* distribution over individual characters. Can simulate fake characters
* to remove chars.
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

  val arcCost = {
    val n = chars.size + rhoSize;
    // we have n emissions
    // We want transitions to be markovian, so:
    //  c * n = exp(mass)
    mass - Math.log(n)
  }

  val initialStateWeights = Map( 0 -> -doubleIsLogSpace.closure(mass));

  def finalWeight(s: Int) = 0.0;

  override val allEdges:Seq[Arc] = {
    val subs = for(a <- chars.toSeq)
      yield Arc(0,0,a,a,arcCost)
    if(rhoSize == 0) subs
    else subs ++ Iterator.single(Arc(0,0,inAlpha.rho,inAlpha.rho,Math.log(rhoSize) + arcCost))
  }

  def edgesMatching(s: Int, a: Char) = {
    if(a == inAlpha.sigma)
      allEdges.iterator
    else if(a == inAlpha.rho)
      Iterator.single(Arc(0,0,a,a,Math.log(rhoSize) + arcCost));
    else Iterator.single(Arc(0,0,a,a,arcCost))
  }
}

