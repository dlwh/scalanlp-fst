package scalanlp.fst.templates

import scalanlp.math.Semiring
import scalanlp.fst.{Arc, Automaton, Alphabet}

class UnigramModel[W:Semiring,T:Alphabet](init: T, chars: Set[T]) extends Automaton[W,T,T] {
  val initialStateWeights = Map( init -> ring.one);

  def finalWeight(s: T) = ring.one

  def edgesMatching(s: T, a: T) = {
    if(a == alphabet.sigma) {
      for(a <- chars.iterator)
      yield Arc(s,a,a,ring.one)
    }  else if(a != alphabet.epsilon)
      Iterator.single(Arc(s,a,a,ring.one))
    else Iterator.empty;
  }
}