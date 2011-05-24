package scalanlp.newfst

/**
 * @author dlwh
 */
trait ArcMatcher[-CC, W, S, T, -MatchType] {
  def arcsMatching(cc: CC, s: S, mt: MatchType):Iterator[Arc[W,S,T]];
}

object Sigma

object ArcMatcher {
  implicit def sigmaMatcher[W,S,T] = new ArcMatcher[Automaton[W,S,T],W,S,T,Sigma.type]{
    def arcsMatching(cc: Automaton[W, S, T], s: S, mt: Sigma.type) = cc.edgesFrom(s)
  }

  implicit def tMatcher[W,S,T] = new ArcMatcher[Automaton[W,S,T],W,S,T,T]{
    def arcsMatching(cc: Automaton[W, S, T], s: S, mt: T) = cc.edgesFrom(s).filter(_.label == mt);
  }

  implicit def tSigmaMatcher[W,S,T,U] = new ArcMatcher[Automaton[W,S,(T,U)],W,S,(T,U),(T,Sigma.type)] {
    def arcsMatching(cc: Automaton[W, S, (T, U)], s: S, mt: (T, Sigma.type)) = {
      cc.edgesFrom(s).filter(_.label._1 == mt._1)
    }
  }

  implicit def sigmaTMatcher[W,S,T,U] = new ArcMatcher[Automaton[W,S,(U,T)],W,S,(U,T),(Sigma.type, T)] {
    def arcsMatching(cc: Automaton[W, S, (U,T)], s: S, mt: (Sigma.type, T)) = {
      cc.edgesFrom(s).filter(_.label._2 == mt._2)
    }
  }
}