package scalanlp.newfst

/**
 * 
 * @author dlwh
 */

trait SelectingAutomaton[W,S,T] extends Automaton[W,S,T] with AutomatonLike[W,S,T,SelectingAutomaton[W,S,T]] {
  def selectEdges(from: S, label: T):Iterator[Arc[W,S,T]]
}

object SelectingAutomaton {
  implicit def tMatcher[W,S,T] = new ArcMatcher[SelectingAutomaton[W,S,T],W,S,T,T]{
    def arcsMatching(cc: SelectingAutomaton[W, S, T], s: S, mt: T) = cc.selectEdges(s, mt);
  }
}