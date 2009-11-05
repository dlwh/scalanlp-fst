package scalanlp.fst

;

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.prop._;
import org.scalacheck.Prop._;
import org.scalacheck.Arbitrary._;
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Before


import scalanlp.math._;
import scala.collection.Traversable;
import scala.collection.Seq;
import scala.collection.mutable.PriorityQueue;


@RunWith(classOf[JUnitRunner])
class EditDistanceTest extends FunSuite with Checkers {
  import Transducer._;
  test("ed is markovian") {
    import scalanlp.math.Semiring.LogSpace._;
    val ed = new EditDistance(-3, -4, Set('a', 'b', 'c'));
    val allWeights = ed.edgesFrom(0).map(_.weight);
    val totalMass = allWeights.reduceLeft(logSum _);
    assert( (totalMass).abs < 1E-10, totalMass + " to far from zero!");
  }

  test("ed is markovian even in the presence of some rhos") {
    import scalanlp.math.Semiring.LogSpace._;
    val ed = new EditDistance(-3, -4, Set('a'),2);

    val allWeights = ed.edgesFrom(0).map(_.weight);
    val totalMass = allWeights.reduceLeft(logSum _);
    assert(totalMass.abs < 1E-10, totalMass + " to far from zero!");
  }

  test("ed is markovian even in the presence of only rhos") {
    import scalanlp.math.Semiring.LogSpace._;
    val ed = new EditDistance(-3, -4, Set(),3);
    val allWeights = ed.edgesFrom(0).map(_.weight);
    val totalMass = allWeights.reduceLeft(logSum _);

    assert(totalMass.abs < 1E-10, totalMass + " to far from zero!");
  }


  test("ed is symmetric") {
    import scalanlp.math.Semiring.LogSpace._;
    val ed = new EditDistance(-3, -4, Set('a','b','c'));
    val abc = Automaton.constant("abc",0.0);
    assert( ((ed >> abc).cost - (abc >> ed).cost).abs < 1E-6);
  }


  test("matches only produces markovian transducer") {
    import scalanlp.math.Semiring.LogSpace._;
    val ed = new EditDistance(doubleIsLogSpace.zero, doubleIsLogSpace.zero, Set('a','b','c'));
    val abc = Automaton.constant("abc",0.0);
    println(ed);
    println(abc);
    println(ed >> abc);
    assert( (ed >> abc).cost === 0.0);
  }

  test("matches and subs only produces markovian transducer") {
    import scalanlp.math.Semiring.LogSpace._;
    val ed = new EditDistance(-2.0, doubleIsLogSpace.zero, Set('a','b','c'));
    val abc = Automaton.constant("abc",0.0);
    assert( (ed >> abc).cost < 1E-6);
  }

  test("ed with a 0 cost automaton gives a distribution") {
    import scalanlp.math.Semiring.LogSpace._;
    val ed = new EditDistance(-3, -4, Set('a','b','c'));
    val abc = Automaton.constant("abc",0.0);
    assert( (ed >> abc).cost === 0.0);
    assert( (abc >> ed).cost === 0.0);
  }

  test("ed with a short 0 cost automaton gives a distribution") {
    import scalanlp.math.Semiring.LogSpace._;
    val ed = new EditDistance(-3, -4, Set('a','b'));
    val abc = Automaton.constant("a",0.0);
    println(abc)
    println(ed);
    println(abc >> ed);
    assert( (ed >> abc).cost === 0.0);
    assert( (abc >> ed).cost === 0.0);
  }

  test("ed with a short 0 cost automaton gives a distribution (acyclicCost)") {
    import scalanlp.math.Semiring.LogSpace._;
    val ed = new EditDistance(-3, -4, Set('a','b'));
    val abc = Automaton.constant("a",0.0);
    println(abc)
    println(ed);
    println(abc >> ed);
    assert( (ed >> abc).acyclicCost === 0.0);
    assert( (abc >> ed).acyclicCost === 0.0);
  }



}
