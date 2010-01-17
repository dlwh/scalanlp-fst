package scalanlp.fst;

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Before


import scalanlp.math._;
import Numerics._;
import scala.collection.Traversable;
import scala.collection.Seq;
import scala.collection.mutable.ArrayBuffer;
import scala.collection.mutable.PriorityQueue;
import org.scalacheck._;

object OneBestSetup {
  import scalanlp.math.Semiring.LogSpace._;
  val tgring = new OneBestSemiring[Char,Double];
}
import OneBestSetup._;
import tgring._;


@RunWith(classOf[JUnitRunner])
class OneBestSemiringTest extends FunSuite with SemiringAxioms[Elem] {

  import ring._;
  import Arbitrary.arbitrary;

  def makeRing = tgring.ring;

  def simpleWeight = for {
    ch <- Gen.alphaChar;
    w <- arbitrary[Double];
    if !w.isNaN
  } yield promote(Arc(0,0,ch,w));

  def compositeWeight = for {
    w1 <- simpleWeight;
    w2 <- simpleWeight;
    bool <- arbitrary[Boolean]
  } yield if(bool) times(w1,w2) else plus(w1,w2);

  def arb: Arbitrary[Elem] = Arbitrary(Gen.oneOf(compositeWeight,simpleWeight));

  import Math.log;

  test("constant automaton") {
    import Automaton._;
    import Semiring.LogSpace.doubleIsLogSpace;

    val auto = constant("Hello",0.0).reweight(promote[Int] _ , promoteOnlyWeight _);
    val cost = auto.cost;
    assert(cost.str.mkString === "Hello");

  }

  test("split automaton") {
    import Automaton._;
    import Semiring.LogSpace.doubleIsLogSpace;
    import IdentityPartitioner._;
    import Numerics._;

    val auto1 = constant("Hello",0.0);
    val auto2 = constant("Hemmo",-0.1)
    val auto = Minimizer.minimize(auto1|auto2).reweight(promote[Int] _ , promoteOnlyWeight _);
    val cost = auto.cost;

    assert(cost.str.mkString === "Hello");
  }

  
}
