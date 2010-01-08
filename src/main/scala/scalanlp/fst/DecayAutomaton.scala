package scalanlp.fst;

import scalanlp.math.Semiring.LogSpace._;

/**
* Generates a geometric distribution over string lengths, and a uniform
* distribution over individual characters. Can simulate fake characters
* to remove chars.
*
* @author dlwh
*/
class DecayAutomaton(val expectedLength:Double, chars: Set[Char], rhoSize: Int = 0) extends Automaton[Double,Int,Char] {
  import Transducer._;
  require( expectedLength > 0);
  // E[|X|] = p / (1-p)
  // p = E[X] / (1+E[X])
  val mass = Math.log(expectedLength / (1+expectedLength));
  val stopProb = Math.log( 1 / (1+expectedLength));

  val arcCost = {
    val n = chars.size + rhoSize;
    // we have n emissions
    // We want transitions to be markovian, so:
    //  c * n = exp(mass)
    mass - Math.log(n)
  }

  val initialStateWeights = Map( 0 -> 0.0);

  def finalWeight(s: Int) = stopProb;

  override lazy val allEdges:Seq[Arc] = edgesMatching(0,alphabet.sigma).toSeq;

  private val rhoEdge = Arc(0,0,alphabet.rho,Math.log(rhoSize) + arcCost);

  def edgesMatching(s: Int, a: Char) = {
    if(s == 0) {
      if(a == alphabet.sigma) {
        val subs = for(a <- chars.iterator)
          yield Arc(0,0,a,arcCost)
        if(rhoSize == 0) subs
        else subs ++ Iterator.single(rhoEdge)
      } else if(a == alphabet.rho)
        Iterator.single(rhoEdge);
      else if(a != alphabet.epsilon)
        Iterator.single(Arc(0,0,a,arcCost))
      else Iterator.empty;
    } else {
      Iterator.empty;
    }
  }
}

