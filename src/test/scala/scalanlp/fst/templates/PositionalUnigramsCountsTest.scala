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
class PositionalUnigramsCountsTest extends FunSuite with Checkers {

  test("constant automaton length 10") {
    import Automaton._;
    import Semiring.LogSpace.doubleIsLogSpace;

    val auto = constant("Hello",0.0);
    val template = new PositionalUnigramModel('#', "Hello".toSet, 10);
    val counts = ExpectedCounts.counts(auto,template);

    assert( counts(('H',0)) === 0.0);
    assert( counts(('e',1)) === 0.0);
    assert( counts(('l',2)) === 0.0);
    assert( counts(('l',3)) === 0.0);
    assert( counts(('o',4)) === 0.0);
    assert( counts(('#',5)) === 0.0);
  }

  test("constant automaton length 3") {
    import Automaton._;
    import Semiring.LogSpace.doubleIsLogSpace;

    val auto = constant("Hello",0.0);
    val template = new PositionalUnigramModel('#', "Hello".toSet, 3);
    val counts = ExpectedCounts.counts(auto,template);

    assert( counts(('H',0)) === 0.0);
    assert( counts(('e',1)) === 0.0);
    assert( counts(('l',2)) === doubleIsLogSpace.plus(0.0,0.0));
    assert( counts(('o',2)) === 0.0);
    assert( counts(('#',2)) === 0.0);
  }

}