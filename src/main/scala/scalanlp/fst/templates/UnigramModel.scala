package scalanlp.fst.templates

import scalanlp.math.Semiring
import scalanlp.fst.{Arc, Automaton, Alphabet}

class UnigramModel[W:Semiring:ClassManifest,T:Alphabet](init: T, chars: Set[T]) extends Automaton[W,Boolean,T] {
  val initialStateWeights = Map( true -> ring.one);

  def finalWeight(s: Boolean) = if(s) ring.zero else ring.one

  def edgesMatching(s: Boolean, a: T) = {
    if(!s) Iterator.empty
    else if(a == alphabet.sigma) {
      val nonEpsArcs = for(a <- chars.iterator)
        yield Arc(s,s,a,ring.one)
      val epsArc = Iterator.single(Arc(s,false,alphabet.epsilon,ring.one))
      nonEpsArcs ++ epsArc;
    }  else if(a != alphabet.epsilon)
      Iterator.single(Arc(s,s,a,ring.one))
    else Iterator.single(Arc(s,!s,a,ring.one))
  }
}
