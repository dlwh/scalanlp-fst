package scalanlp.fst.templates

import scalanlp.fst._
import scalanlp.math.Semiring;

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.prop._;

/**
 * Tests the UnigramModel
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class BigramCountsTest extends FunSuite with Checkers {

  test("constant automaton") {
    import Automaton._;
    import Semiring.LogSpace.doubleIsLogSpace;

    val auto = constant("Hello",0.0);
    val template = new BigramModel('#', "Hello".toSet);
    val counts = ExpectedCounts.counts(auto,template);

    def state(ch1: Char, ch2: Char) = (ch1,ch2,ch2);

    assert( counts(state('#','H')) === 0.0);
    assert( counts(state('H','e')) === 0.0);
    assert( counts(state('e','l')) === 0.0);
    assert( counts(state('l','l')) === 0.0);
    assert( counts(state('l','o')) === 0.0);
    assert( counts(state('o','\0')) === 0.0);
  }

}