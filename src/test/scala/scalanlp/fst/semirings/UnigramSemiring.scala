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

object UnigramSetup {
  val validChars = Set.empty ++ "Helloabcdem";
  val tgring = new UnigramSemiring[Char](validChars,'#');
}
import UnigramSetup._;
import tgring._;


@RunWith(classOf[JUnitRunner])
class UnigramSemiringTest extends FunSuite with SemiringAxioms[Elem] {

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

  import Math.log;

  test("simple* works") {
    val enc = 'a';
    val w = promote(Arc(0,0,'a',log(0.5)));
    val cl = closure(w);
    val counts = cl.decode;
    assert(cl.totalProb === log(2.0));
    assert(counts('a') === log(0.5) + 2 * log(2.0));
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
    assert(counts('a') === log(0.25) + 2 * log(2.0));
  }

  test("constant automaton") {
    import Automaton._;
    import Semiring.LogSpace.doubleIsLogSpace;

    val auto = constant("Hello",0.0).reweight(promote[Int] _ , promoteOnlyWeight _);
    val cost = auto.cost;
    val counts = cost.decode;

    assert( counts('H') === 0.0);
    assert( counts('e') === 0.0);
    assert( counts('l') === logSum(0.0,0.0));
    assert( counts('o') === 0.0);
    assert( counts('#') === 0.0);
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

    assert( counts('#') === logSum(0.0,0.0));
    assert( counts('H') === logSum(0.0,0.0));
    assert( counts('e') === logSum(0.0,0.0));
    assert( counts('l') === logSum(0.0,0.0));
    assert( counts('m') === logSum(0.0,0.0));
    assert( counts('o') === logSum(0.0,0.0));
  }

  
}
