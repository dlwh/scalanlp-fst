package scalanlp.fst.templates

import scalanlp.fst._;
import scalanlp.math._;


import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Before
import org.scalatest.prop._;

import org.scalacheck._;

/**
 * Tests the UnigramModel
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class UnigramCountsTest extends FunSuite with Checkers {

  test("constant automaton") {
    import Automaton._;
    import Semiring.LogSpace.doubleIsLogSpace;

    val auto = constant("Hello",0.0);
    val template = new UnigramModel[Double,Char]('#', "Hello".toSet);
    val counts = ExpectedCounts.counts(auto,template);
    def state(c: Char, isFinished: Boolean = false) = (true,!isFinished,c);

    assert( counts(state('H')) === 0.0);
    assert( counts(state('e')) === 0.0);
    assert( counts(state('l')) === doubleIsLogSpace.plus(0.0,0.0));
    assert( counts(state('o')) === 0.0);
    assert( counts(state('\0',true)) === 0.0);
  }

}