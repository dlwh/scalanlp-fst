package scalanlp.fst

import breeze.math.Semiring


/**
 * 
 * @author dlwh
 */

trait Reverser[-CC,+CC2] extends AutomatonTransformer[CC,CC2]

object Reverser {
  implicit def basicReverser[W:Semiring,State,T]:Reverser[Automaton[W,State,T],Automaton[W,State,T]] = {
    new Reverser[Automaton[W,State,T],Automaton[W,State,T]] {
      def apply(auto: Automaton[W, State, T]) = {
        import auto._
        val buf: IndexedSeq[Arc[W,State,T]] = edges.map({ case Arc(from,to,label,w) => Arc(to,from,label,w) }).toIndexedSeq

        val zero = implicitly[Semiring[W]].zero
        val myInit = Map.empty ++ (
          for(a <- buf if finalWeight(a.source) != zero) yield (a.source,finalWeight(a.source))
        ) withDefaultValue(zero)

        val finalWeights = initialStateWeights withDefaultValue zero

        Automaton.automaton(myInit,finalWeights)(buf:_*)
      }
    }
  }
}