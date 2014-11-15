package scalanlp.fst

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith



@RunWith(classOf[JUnitRunner])
class DecayAutomatonTest extends FunSuite {
  import Transducer._;
  test("da has cost 0") {
    import breeze.math.Semiring.LogSpace._;
    val da = new DecayAutomaton(4,Set('b','c','d'));
    val cost = da.cost;
    assert(cost.abs < 1E-10, cost + " to far from zero!");
  }

}
