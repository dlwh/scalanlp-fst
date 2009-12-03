package scalanlp.fst;

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Before


import scalanlp.math._;
import scala.collection.Traversable;
import scala.collection.Seq;
import scala.collection.mutable.ArrayBuffer;
import scala.collection.mutable.PriorityQueue;


@RunWith(classOf[JUnitRunner])
class BigramSemiringTest extends FunSuite {
  import BigramSemiring._;
  import ring._;
  test("bg zero + zero == zero") {
    assert(plus(zero,zero) === zero)
  }
  test("bg one + zero == one") {
    assert(plus(one,zero) === one)
  }
  test("bg zero + one == one") {
    assert(plus(zero,one) === one)
  }

  test("constant automaton") {
    import Automaton._;
    import Semiring.LogSpace.doubleIsLogSpace;

    val auto = constant("Hello",0.0).reweight(promote[Int] _ , promoteOnlyWeight _);
    val counts = auto.cost.counts;

    assert( counts(ArrayBuffer('#', 'H')) === 0.0);
    assert( counts(ArrayBuffer('H', 'e')) === 0.0);
    assert( counts(ArrayBuffer('l', 'l')) === 0.0);
    assert( counts(ArrayBuffer('l', 'o')) === 0.0);
    assert( counts(ArrayBuffer('o', '#')) === 0.0);
    assert( counts(ArrayBuffer('o')) === 0.0);
    assert((counts(ArrayBuffer('l')) - 0.6931471805599453).abs < 1E-6, counts(ArrayBuffer('l')));
    assert( counts(ArrayBuffer('e')) === 0.0);
    assert( counts(ArrayBuffer('H')) === 0.0);
  }

  test("split automaton") {
    import Automaton._;
    import Semiring.LogSpace.doubleIsLogSpace;
    import IdentityPartitioner._;
    import Numerics._;

    val auto1 = constant("Hello",0.0);
    val auto2 = constant("Hemmo",0.0)
    val auto = Minimizer.minimize(auto1|auto2).reweight(promote[Int] _ , promoteOnlyWeight _);
    val counts = auto.cost.counts;

    assert( counts(ArrayBuffer('#', 'H')) === logSum(0.0,0.0));
    assert( counts(ArrayBuffer('#', '#')) === Double.NegativeInfinity);
    assert( counts(ArrayBuffer('H', 'e')) === logSum(0.0,0.0));
    assert( counts(ArrayBuffer('l', 'l')) === 0.0);
    assert( counts(ArrayBuffer('m', 'o')) === 0.0);
    assert( counts(ArrayBuffer('o')) === logSum(0.0,0.0));
    assert((counts(ArrayBuffer('l')) - 0.6931471805599453).abs < 1E-6, counts(ArrayBuffer('l')));
    assert( counts(ArrayBuffer('H')) === logSum(0.0,0.0));
  }

  
}
