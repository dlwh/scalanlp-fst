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

  def finalWeight(s: Int) = if(s == 1) 0.0 else Math.NEG_INF_DOUBLE;

  override lazy val allEdges:Seq[Arc] = edgesMatching(0,inAlpha.sigma).toSeq;

  private val endingEdge = Arc(0,1,inAlpha.epsilon,inAlpha.epsilon,stopProb);
  private val rhoEdge = Arc(0,0,inAlpha.rho,inAlpha.rho,Math.log(rhoSize) + arcCost);

  def edgesMatching(s: Int, a: Char) = {
    if(s == 0) {
      if(a == inAlpha.sigma) {
        val subs = for(a <- chars.iterator)
          yield Arc(0,0,a,a,arcCost)
        if(rhoSize == 0) subs ++ Iterator.single(endingEdge);
        else subs ++ Iterator(rhoEdge,endingEdge)
      } else if(a == inAlpha.rho)
        Iterator.single(rhoEdge);
      else if(a == inAlpha.epsilon)
        Iterator.single(endingEdge);
      else 
        Iterator.single(Arc(0,0,a,a,arcCost))
    } else {
      Iterator.empty;
    }
  }
}

