package scalanlp.fst.templates

import scalanlp.math.Semiring
import scalanlp.fst.{Arc, Automaton, Alphabet}

class PositionalUnigramModel[W:Semiring,T:Alphabet](init: T, chars: Set[T], maxLength: Int) extends Automaton[W,(T,Int),T] {
  val initialStateWeights = Map( (init,-1) -> ring.one);

  def finalWeight(s: (T,Int)) = if (s._1 == init) ring.one else ring.zero;

  private def next(s: Int) = (s+1) min (maxLength-1);

  def edgesMatching(s: (T,Int), a: T) = {
    val pos = next(s._2);
    if(s._1 == '#' && s._2 >= 0) Iterator.empty
    else if (a == alphabet.sigma) {
      val realArcs = for(a <- chars.iterator)
        yield Arc(s,(a,pos),a,ring.one)
      val epsArc = Arc(s,(init,pos),alphabet.epsilon,ring.one);
      if(s._1 != init)
        Iterator.single(epsArc) ++ realArcs;
      else realArcs
    }  else if(a != alphabet.epsilon) {
      Iterator.single(Arc(s,(a,pos),a,ring.one))
    } else if(s._1 != init) Iterator.single(Arc(s,(init,pos),a,ring.one));
    else Iterator.empty
  }
}