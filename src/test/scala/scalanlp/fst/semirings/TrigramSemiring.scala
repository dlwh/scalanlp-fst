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
  val validBasicBigrams = (validChars.drop(1) zip validChars.take(validChars.length -1)) ++ validChars.zip(validChars);
  val moreBigrams = "##Hello".zip("#Hello#");
  val tgring = new TrigramSemiring(Set.empty ++ validChars, Set.empty ++ validBasicBigrams ++ moreBigrams, '#');
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
    ch <- Gen.elements(validChars:_*);
    w <- arbitrary[Double];
    if !w.isNaN
  } yield promote(Arc(0,0,ch,w));

  def compositeWeight = for {
    ws <- Gen.listOf(simpleWeight);
    if ws.length > 1
    combine <- Gen.listOfN(ws.size-1,arbitrary[Boolean])
  } yield ws.drop(1).zip(combine).foldLeft(ws(0)) { (acc,w2com) => if(w2com._2) times(acc,w2com._1) else plus(acc,w2com._1) }

  def arbGen: Gen[Elem] = Gen.oneOf(simpleWeight, compositeWeight);

  def arb: Arbitrary[Elem] = Arbitrary(arbGen);

  import math.log;

  test("simple* works") {
    val enc = 'a';
    val w = promote(Arc(0,0,'a',log(0.5)));
    val cl = closure(w);
    assert(cl.totalProb === log(2.0));
    val decoded = cl.decode;
    val bigram = Bigram(enc,enc);
    assert(cl.leftUnigrams === cl.rightUnigrams);
    assert( (decoded(bigram)(enc) - log(1.0/2.0)).abs < 1E-9,decoded(bigram)(enc) + " vs " + log(1.0/2.0) );
  }

  test("composite* works") {
    val enc = 'a';
    val enc2 = 'b';
    val w1 = promote(Arc(0,0,'a',log(0.25)));
    val w2 = promote(Arc(0,0,'b',log(0.25)));
    val w = plus(w1,w2);
    val cl = closure(w);
    assert(cl.totalProb === log(2.0));
    val decoded = cl.decode;
    val bigram = Bigram(enc,enc);
    assert( (decoded(bigram)(enc) - log(0.0625)).abs < 1E-9);
    assert(cl.leftUnigrams === cl.rightUnigrams);
  }

  test("constant automaton") {
    import Automaton._;
    import Semiring.LogSpace.doubleIsLogSpace;

    val auto = constant("Hello",0.0).reweight(promote[Int] _ , promoteOnlyWeight _);
    val cost = auto.cost;
    val counts = cost.decode;

    assert( counts(Bigram('H','e'), ('l')) === 0.0);
    assert( counts(Bigram('#','H'), ('e')) === 0.0);
    assert( counts(Bigram('l','o'), ('#')) === 0.0);
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

    assert( counts(Bigram('#','H'), ('e')) === logSum(0.0,0.0));
    assert( counts(Bigram('H','e'), ('l')) === 0.0);
    assert( counts(Bigram('l','l'), ('o')) === 0.0);
    // m isn't an acceptible char
    assert( counts(Bigram('m','m'), ('o')) === Double.NegativeInfinity);
    assert( counts(Bigram('l','o'), ('#')) === 0.0);
  }

  
}
