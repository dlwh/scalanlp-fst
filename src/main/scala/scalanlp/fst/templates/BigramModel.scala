package scalanlp.fst.templates

import scalanlp.math.Semiring
import scalanlp.fst.{Arc, Automaton, Alphabet}

class BigramModel[W:Semiring,T:Alphabet](init: T, chars: Set[T]) extends Automaton[W,T,T] {
  val initialStateWeights = Map( init -> ring.one);

  def finalWeight(s: T) = if (s == alphabet.epsilon) ring.one else ring.zero

  def edgesMatching(s: T, a: T) = {
    if(s == alphabet.epsilon) Iterator.empty
    else if(a == alphabet.sigma) {
      val realArcs = for(a <- chars.iterator)
      yield Arc(s,a,a,ring.one)
      val epsArc = Arc(s,alphabet.epsilon,alphabet.epsilon,ring.one);
      if(s != init)
        Iterator.single(epsArc) ++ realArcs;
      else
        realArcs
    } else {
      Iterator.single(Arc(s,a,a,ring.one))
    }

  }
}