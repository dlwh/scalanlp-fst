package scalanlp.fst

import scalanlp.math._;

trait FiniteStateTransducer {
  type State;
  /**
   * The type of the Alphabet characters
   * 
   */
  type T;
  type W;
  implicit val ring: Semiring[W];
  
  def admits
}
