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

object TrigramSetup {
  val validChars = Seq('a','b','c','d','H','e','l','o');
  val validPairs = validChars zip validChars;
  val tgring = new TrigramSemiring(Set.empty ++ validPairs);
}
import TrigramSetup._;
import tgring._;


@RunWith(classOf[JUnitRunner])
class TrigramSemiringTest extends FunSuite with SemiringAxioms[Elem] {

  import TrigramSemiring._;
  import ring._;
  import Arbitrary.arbitrary;

  def makeRing = tgring.ring;

  def simpleWeight = for {
    ch <- Gen.elements(validPairs:_*);
    w <- arbitrary[Double];
    if !w.isNaN
  } yield promote(Arc(0,0,ch._1,ch._2,w));

  def compositeWeight = for {
    w1 <- simpleWeight;
    w2 <- simpleWeight;
    bool <- arbitrary[Boolean]
  } yield if(bool) times(w1,w2) else plus(w1,w2);

  def arb: Arbitrary[Elem] = Arbitrary(Gen.oneOf(compositeWeight,simpleWeight));

  import Math.log;

  test("simple* works") {
    val enc = encode('a','a');
    val w = promote(Arc(0,0,'a','a',log(0.5)));
    val cl = closure(w);
    assert(cl.totalProb === log(2.0));
    val decoded = cl.decode;
    val bigram = Bigram(enc,enc);
    assert(cl.leftUnigrams === cl.rightUnigrams);
    assert(cl.leftBigrams === cl.rightBigrams);
    assert( (decoded(bigram)(enc) - log(1.0/2.0)).abs < 1E-9,decoded(bigram)(enc) + " vs " + log(1.0/2.0) );
  }

  test("composite* works") {
    val enc = encode('a','a');
    val enc2 = encode('b','b');
    val w1 = promote(Arc(0,0,'a','a',log(0.25)));
    val w2 = promote(Arc(0,0,'b','b',log(0.25)));
    val w = plus(w1,w2);
    val cl = closure(w);
    assert(cl.totalProb === log(2.0));
    val decoded = cl.decode;
    val bigram = Bigram(enc,enc);
    assert( (decoded(bigram)(enc) - log(0.0625)).abs < 1E-9);
    assert(cl.leftUnigrams === cl.rightUnigrams);
    assert(cl.leftBigrams === cl.rightBigrams);
  }

  test("constant automaton") {
    import Automaton._;
    import Semiring.LogSpace.doubleIsLogSpace;

    val auto = constant("Hello",0.0).reweight(promote[Int] _ , promoteOnlyWeight _);
    val cost = auto.cost;
    val counts = cost.decode;

    assert( counts(Bigram('H','e'), encodeOne('l')) === 0.0);
    assert( counts(Bigram('#','H'), encodeOne('e')) === 0.0);
    assert( counts(Bigram('l','o'), encodeOne('#')) === 0.0);
    assert( counts(Bigram('l','l'), encodeOne('o')) === 0.0);
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

    assert( counts(Bigram('#','H'), encodeOne('e')) === logSum(0.0,0.0));
    assert( counts(Bigram('H','e'), encodeOne('l')) === 0.0);
    assert( counts(Bigram('l','l'), encodeOne('o')) === 0.0);
    // m isn't an acceptible char
    assert( counts(Bigram('m','m'), encodeOne('o')) === Double.NegativeInfinity);
    assert( counts(Bigram('l','o'), encodeOne('#')) === 0.0);
  }

  
}
