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
import org.scalacheck._;

object PositionalUnigramSetup {
  val validChars = Set.empty ++ "Helloabcdem";
  val tgring = new PositionalUnigramSemiring(10,validChars,'#');
}
import PositionalUnigramSetup._;
import tgring._;


@RunWith(classOf[JUnitRunner])
class PositionalUnigramSemiringTest extends FunSuite with SemiringAxioms[Elem] {

  import TrigramSemiring._;
  import ring._;
  import Arbitrary.arbitrary;

  def makeRing = tgring.ring;

  def simpleWeight = for {
    ch <- Gen.elements(validChars.toSeq:_*);
    w <- arbitrary[Double];
    if !w.isNaN
  } yield promote(Arc(0,0,ch,w));

  def compositeWeight = for {
    w1 <- simpleWeight;
    w2 <- simpleWeight;
    bool <- arbitrary[Boolean]
  } yield if(bool) times(w1,w2) else plus(w1,w2);

  def arb: Arbitrary[Elem] = Arbitrary(Gen.oneOf(compositeWeight,simpleWeight));

  import math.log;

  test("simple* works") {
    val enc = 'a';
    val w = promote(Arc(0,0,'a',log(0.5)));
    val cl = closure(w);
    val counts = cl.decode;
    assert(cl.totalProb === log(2.0));
    for(i <- 0 until cl.counts.length) {
      assert(counts(i)('a') === log(0.5) * (i+1));
    }
  }

  test("composite* works") {
    val enc = 'a';
    val enc2 = 'b';
    val w1 = promote(Arc(0,0,'a',log(0.25)));
    val w2 = promote(Arc(0,0,'b',log(0.25)));
    val w = plus(w1,w2);
    val cl = closure(w);
    val counts = cl.decode;
    assert(cl.totalProb === log(2.0));
    for(i <- 0 until cl.counts.length) {
      assert(counts(i)('a') === log(0.25) + log(0.5) * (i));
    }
  }

  test("constant automaton") {
    import Automaton._;
    import Semiring.LogSpace.doubleIsLogSpace;

    val auto = constant("Hello",0.0).reweight(promote[Int] _ , promoteOnlyWeight _);
    val cost = auto.cost;
    val counts = cost.decode;

    assert( counts(0)('#') === 0.0);
    assert( counts(0).logTotal === 0.0);
    assert( counts(1)('H') === 0.0);
    assert( counts(1).logTotal === 0.0);
    assert( counts(2)('e') === 0.0);
    assert( counts(2).logTotal === 0.0);
    assert( counts(3)('l') === 0.0);
    assert( counts(3).logTotal === 0.0);
    assert( counts(4)('l') === 0.0);
    assert( counts(4).logTotal === 0.0);
    assert( counts(5)('o') === 0.0);
    assert( counts(5).logTotal === 0.0);
    assert( counts(6)('#') === 0.0);
    assert( counts(6).logTotal === 0.0);
    assert( counts(7).logTotal ===Double.NegativeInfinity);
  }

  test("split automaton") {
    import Automaton._;
    import Semiring.LogSpace.doubleIsLogSpace;
    import IdentityPartitioner._;
    import Numerics._;

    val auto1 = constant("Hello",0.0);
    val auto2 = constant("Hemmo",0.0)
    val auto = Minimizer.minimize(auto1|auto2).reweight(promote[Int] _ , promoteOnlyWeight _);
    val counts = auto.cost.decode;

    assert( counts(0)('#') === logSum(0.0,0.0));
    assert( counts(1)('H') === logSum(0.0,0.0));
    assert( counts(1).logTotal === logSum(0.0,0.0));
    assert( counts(2)('e') === logSum(0.0,0.0));
    assert( counts(2).logTotal === logSum(0.0,0.0));
    assert( counts(3)('l') === 0.0);
    assert( counts(3)('m') === 0.0);
    assert( counts(3).logTotal === logSum(0.0,0.0));
    assert( counts(5)('o') === logSum(0.0,0.0));
    assert( counts(5).logTotal === logSum(0.0,0.0));
    assert( counts(6)('#') === logSum(0.0,0.0));
    assert( counts(6).logTotal === logSum(0.0,0.0));
    assert( counts(7).logTotal === Double.NegativeInfinity);
  }

  
}
