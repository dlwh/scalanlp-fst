package scalanlp.fst
package templates

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.prop._
import breeze.math.Semiring




/**
 * Tests the UnigramModel
 *
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class PositionalUnigramsCountsTest extends FunSuite with Checkers {

  test("constant automaton length 10") {
    import Automaton._
    import Semiring.LogSpace.doubleIsLogSpace

    val auto = constant("Hello",0.0)
    val template = new PositionalUnigramModel[Double,Char]('#', "Hello".toSet, 10)
    val counts = ExpectedCounts.counts(auto,template)

    assert( counts(0)(1)('H') === 0.0)
    assert( counts(1)(2)('e') === 0.0)
    assert( counts(2)(3)('l') === 0.0)
    assert( counts(3)(4)('l') === 0.0)
    assert( counts(4)(5)('o') === 0.0)
    assert( counts(5)(-1)('\0') === 0.0)
  }

  test("constant automaton length 3") {
    import Automaton._
    import Semiring.LogSpace.doubleIsLogSpace

    val auto = constant("Hello",0.0)
    val template = new PositionalUnigramModel[Double,Char]('#', "Hello".toSet, 3)
    val counts = ExpectedCounts.counts(auto,template)

    assert( counts(0)(1)('H') === 0.0)
    assert( counts(1)(2)('e') === 0.0)
    assert( counts(2)(2)('l') === doubleIsLogSpace.plus(0.0,0.0))
    assert( counts(2)(2)('o') === 0.0)
    assert( counts(2)(-1)('\0') === 0.0)
  }

}
