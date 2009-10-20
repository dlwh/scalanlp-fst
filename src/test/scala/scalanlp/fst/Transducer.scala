package scalanlp.fst;

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
class TransducerTest extends FunSuite with Checkers {
  import Transducer._;
  import Semiring.LogSpace._;

  test("cost of a no-arc system is correct") {
    val fst = Transducer.transducer(Map(0->0.0),Map(0->0.0))();
    assert(fst.cost === 0.0);
  }

  test("cost of a single-self loop system is its closure") {
    val selfLoopScore = -1.0;
    val trueCost = doubleIsLogSpace.closure(selfLoopScore)
    val fst = Transducer.transducer(Map(0->0.0),Map(0->0.0))(Arc(0,0,None,None,selfLoopScore));
    assert(fst.cost === trueCost);
  }

  test("reverse works nicely") {
    val fst = transducer(Map(0->0.0),Map(2->0.0))( Arc(0,1,None,None,-1.0), 
        Arc(1,0,None,None,-2.0), Arc(1,2,None,Some('3'),-3.0), Arc(2,0,None,Some('4'),-5.0));
    val myRevd = transducer(Map(2->0.0),Map(0->0.0))( Arc(1,0,None,None,-1.0), 
        Arc(0,1,None,None,-2.0), Arc(2,1,None,Some('3'),-3.0), Arc(0,2,None,Some('4'),-5.0));
    assert(myRevd === fst.reverse);
  }

  test("Mohri hwa fig12a weight pushing") {
    import Semiring.Tropical._;
    val dsl = new DSL[Int,Double]()(doubleIsTropical);
    import dsl._;
    val fst = transducer(Map(0->0.0),Map(3->0.0))(
      0 -> 1 ('a',None,0),
      0 -> 1 ('b',None,1),
      0 -> 1 ('c',None,5),
      0 -> 2 ('d',None,0),
      0 -> 2 ('e',None,1),
      1 -> 3 ('e',None,0),
      1 -> 3 ('f',None,1),
      2 -> 3 ('e',None,4),
      2 -> 3 ('f',None,5)
    );
    val pushed = transducer(Map(0->0.0),Map(3->0.0))(
      0 -> 1 ('a',None,0),
      0 -> 1 ('b',None,1),
      0 -> 1 ('c',None,5),
      0 -> 2 ('d',None,4),
      0 -> 2 ('e',None,5),
      1 -> 3 ('e',None,0),
      1 -> 3 ('f',None,1),
      2 -> 3 ('e',None,0),
      2 -> 3 ('f',None,1)
    );
    assert(pushed === fst.pushWeights(doubleIsTropical));
  }

  test("Mohri hwa fig12c weight pushing") {
    import Semiring.Probability.semiring;
    val dsl = new DSL[Int,Double]()(semiring);
    import dsl._;
    val fst = transducer(Map(0->1.0),Map(3->1.0))(
      0 -> 1 ('a',None,0),
      0 -> 1 ('b',None,1),
      0 -> 1 ('c',None,5),
      0 -> 2 ('d',None,0),
      0 -> 2 ('e',None,1),
      1 -> 3 ('e',None,0),
      1 -> 3 ('f',None,1),
      2 -> 3 ('e',None,4),
      2 -> 3 ('f',None,5)
    );
    val pushed = transducer(Map(0->15.0),Map(3->1.0))(
      0 -> 1 ('a',None,0),
      0 -> 1 ('b',None,1./15),
      0 -> 1 ('c',None,5./15),
      0 -> 2 ('d',None,0),
      0 -> 2 ('e',None,9./15),
      1 -> 3 ('e',None,0),
      1 -> 3 ('f',None,1),
      2 -> 3 ('e',None,4./9),
      2 -> 3 ('f',None,5./9)
    );
    assert(pushed === fst.pushWeights(semiring));
  }

  test("Mohri hwa fig12d weight pushing") {
    import Semiring.Tropical._;
    val dsl = new DSL[Int,Double]()(doubleIsTropical);
    import dsl._;
    val fst = transducer(Map(0->0.0),Map(3->0.0))(
      0 -> 1 ('a',None,0),
      0 -> 1 ('b',None,1),
      0 -> 1 ('c',None,5),
      0 -> 2 ('d',None,0),
      0 -> 2 ('e',None,1),
      1 -> 3 ('e',None,0),
      1 -> 3 ('f',None,1),
      2 -> 3 ('e',None,4),
      2 -> 3 ('f',None,5)
    );
    val minimized = transducer(Map(0->0.0),Map(3->0.0))(
      0 -> 1 ('a',None,0),
      0 -> 1 ('b',None,1),
      0 -> 1 ('c',None,5),
      0 -> 1 ('d',None,4),
      0 -> 1 ('e',None,5),
      1 -> 3 ('e',None,0),
      1 -> 3 ('f',None,1)
    );
    assert(minimized === fst.minimize(doubleIsTropical));
  }

}
