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

    assert( counts(('#','H')) === 0.0);
    assert( counts(('H','e')) === 0.0);
    assert( counts(('e','l')) === 0.0);
    assert( counts(('l','l')) === 0.0);
    assert( counts(('l','o')) === 0.0);
    assert( counts(('o','#')) === 0.0);
  }

}