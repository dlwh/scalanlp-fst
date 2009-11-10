package scalanlp.fst;

import scalanlp.math._;

object Pruning {
  def calculateStateFlow[W:Ordering,State,In,Out](auto: Transducer[W,State,In,Out]): Map[State,W] = {
    val forward = auto.allPathDistances;
    val backward = auto.reverse.allPathDistances;
    val combined = for {
      (s,f) <- forward;
      if f != auto.ring.zero
      b = backward.getOrElse(s,auto.ring.zero)
      if b != auto.ring.zero
    } yield {
      (s,auto.ring.times(f,b));
    }

    combined withDefaultValue auto.ring.zero;
  }
}
