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
    val (counts,totals,finalWeights) = expectedCounts(auto,template);

    assert( counts(0)(index('H'))(1) === 0.0,'H');
    assert( counts(1)(index('e'))(2) === 0.0,'e');
    assert( counts(2)(index('l'))(3) === 0.0,'l');
    assert( counts(3)(index('l'))(4) === 0.0,"l2");
    assert( counts(4)(index('o'))(5) === 0.0, "o");
    assert( finalWeights(5) === 0.0, "eps");
  }

  test("constant automaton length 3") {
    val auto = constant("Hello",0.0);
    val template = new PositionalUnigramModel('#', 3);
    val (counts,totals, finalWeights) = expectedCounts(auto,template);

    assert( counts(0)(index('H'))(1)=== 0.0);
    assert( counts(1)(index('e'))(2) === 0.0);
    assert( counts(2)(index('l'))(2) === doubleIsLogSpace.plus(0.0,0.0));
    assert( counts(2)(index('o'))(2) === 0.0);
    assert( finalWeights(2) === 0.0);
  }

}
