package scalanlp.fst.templates

import scalanlp.math.Semiring
import scalanlp.fst.{Arc, Automaton, Alphabet}

class BigramModel[W:Semiring,T:Alphabet](init: T, chars: Set[T]) extends Automaton[W,(T,T),T] {
  val initialStateWeights = Map( (init,init) -> ring.one);

  def finalWeight(s: (T,T)) = if (s._2 == init) ring.one else ring.zero

  def edgesMatching(s: (T,T), a: T) = {
    if(s._2 == init && s._1 != init) {
      Iterator.empty
    } else if(a == alphabet.sigma) {
      val realArcs = for(a <- chars.iterator)
      yield Arc(s,(s._2,a),a,ring.one)
      val epsArc = Arc(s,(s._2,init),alphabet.epsilon,ring.one);
      if(s != (init,init))
        Iterator.single(epsArc) ++ realArcs;
      else realArcs
    }  else if(a != alphabet.epsilon) {
      Iterator.single(Arc(s,(s._2,a),a,ring.one))
    } else if(s._2 != init) Iterator.single(Arc(s,(s._2,init),a,ring.one));
    else Iterator.empty
  }
}