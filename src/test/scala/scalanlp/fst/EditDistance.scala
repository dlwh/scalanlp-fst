package scalanlp.fst;

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Before


import scalanlp.math._;
import scala.collection.Traversable;
import scala.collection.Seq;
import scala.collection.mutable.PriorityQueue;


@RunWith(classOf[JUnitRunner])
class EditDistanceTest extends FunSuite {
  import Transducer._;

  test("ed is symmetric") {
    import scalanlp.math.Semiring.LogSpace._;
    val ed = new EditDistance(-3, -4, Set('a','b','c'));
    val abc = Automaton.constant("abc",0.0).asTransducer;
    assert( ((ed >> abc).cost - (abc >> ed).cost).abs < 1E-6, (ed >> abc).cost + " " + (abc >> ed).cost);
  }


  test("matches only produces markovian transducer") {
    import scalanlp.math.Semiring.LogSpace._;
    val ed = new EditDistance(doubleIsLogSpace.zero, doubleIsLogSpace.zero, Set('a','b','c'));
    val abc = Automaton.constant("abc",0.0).asTransducer;
    assert( (ed >> abc).cost.abs < 1E-6,ed >> abc cost);
  }

  test("matches and subs only produces markovian transducer") {
    import scalanlp.math.Semiring.LogSpace._;
    val ed = new EditDistance(-2.0, doubleIsLogSpace.zero, Set('a','b','c'));
    val abc = Automaton.constant("abc",0.0).asTransducer;
    assert( (ed >> abc).cost.abs < 1E-6, (ed >> abc) cost);
  }

  test("matches,subs,dels produces markovian transducer") {
    import scalanlp.math.Semiring.LogSpace._;
    val ed = new EditDistance(-2.0, -3.0, Set('a','b','c'));
    val abc = Automaton.constant("abc",0.0).asTransducer;
    assert( (ed >> abc).cost.abs < 1E-6, (ed >> abc) cost);
  }

  test("matches,subs with rhos produces markovian transducer") {
    import scalanlp.math.Semiring.LogSpace._;
    val ed = new EditDistance(-2.0, Double.NegativeInfinity, Set('a','b','c'),3);
    val abc = Automaton.constant("abc",0.0).asTransducer;
    assert( (ed >> abc).cost.abs < 1E-6, (ed >> abc) cost);
  }

  test("matches,subs,dels with rhos produces markovian transducer") {
    import scalanlp.math.Semiring.LogSpace._;
    val ed = new EditDistance(-2.0, -3.0, Set('a','b','c'),3);
    val abc = Automaton.constant("abc",0.0).asTransducer;
    assert( (ed >> abc).cost.abs < 1E-6, (ed >> abc) cost);
  }

  test("matches,subs,dels with rhos ^2 produces markovian transducer") {
    import scalanlp.math.Semiring.LogSpace._;
    val ed = new EditDistance(-2.0, -3.0, Set('a','b','c'),3);
    val abc = Automaton.constant("abc",0.0).asTransducer;
    val ed2 = Composition.compose(ed,ed,EditDistance.rhoSquaredCost(ed,ed));
    assert( (ed2 >> abc).cost.abs < 1E-6, (ed >> abc) cost);
  }

  test("ed with a 0 cost automaton gives a distribution") {
    import scalanlp.math.Semiring.LogSpace._;
    val ed = new EditDistance(-3, -4, Set('a','b','c'));
    val abc = Automaton.constant("abc",0.0).asTransducer;
    assert( (ed >> abc).cost.abs < 1E-6, ed >> abc cost);
    assert( (abc >> ed).cost.abs < 1E-6);
  }

  test("ed with a short 0 cost automaton gives a distribution") {
    import scalanlp.math.Semiring.LogSpace._;
    val ed = new EditDistance(-3, -4, Set('a','b'));
    val abc = Automaton.constant("a",0.0).asTransducer;
    assert( (ed >> abc).cost.abs < 1E-6);
    assert( (abc >> ed).cost.abs < 1E-6);
  }

}
