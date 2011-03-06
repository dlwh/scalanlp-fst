package scalanlp.fst.fast.templates

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.prop._
import scalanlp.fst.fast.AutomatonFactory
import scalanlp.fst.Alphabet
import scalanlp.util.Index


/**
 * Tests the UnigramModel
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class PositionalUnigramsCountsTest extends FunSuite with Checkers {
  import scalanlp.math.Semiring.LogSpace.doubleIsLogSpace;
  val factory = new AutomatonFactory[Char](Index( Set() ++ ("#Hello") + implicitly[Alphabet[Char]].epsilon));
  import factory._;


  test("constant automaton length 10") {
    val auto = constant("Hello",0.0);
    val template = new PositionalUnigramModel('#', 10);
    val counts = expectedCounts(auto,template);

    assert( counts(0)(1)(index('H')) === 0.0);
    assert( counts(1)(2)(index('e')) === 0.0);
    assert( counts(2)(3)(index('l')) === 0.0);
    assert( counts(3)(4)(index('l')) === 0.0);
    assert( counts(4)(5)(index('o')) === 0.0);
    assert( counts(5)(10)(epsilonIndex) === 0.0);
  }

  test("constant automaton length 3") {
    val auto = constant("Hello",0.0);
    val template = new PositionalUnigramModel('#', 3);
    val counts = expectedCounts(auto,template);

    assert( counts(0)(1)(index('H'))=== 0.0);
    assert( counts(1)(2)(index('e')) === 0.0);
    assert( counts(2)(2)(index('l')) === doubleIsLogSpace.plus(0.0,0.0));
    assert( counts(2)(2)(index('o')) === 0.0);
    assert( counts(2)(3)(epsilonIndex) === 0.0);
  }

}
