package scalanlp.fst

import scalanlp.math._;

/**
 * @param F The input alphabet
 * @param T The output alphabet
 */
trait FiniteStateTransducer[F,T] {
  type State;
  /**
   * The weight attached to any arc.
   */
  type W;
  implicit val ring: Semiring[W];
  
  val states: Set[State];
  val initialState : State;
  
  case class Arc(from: State, to: State, weight: W, in: F, out: T)
                  
}
