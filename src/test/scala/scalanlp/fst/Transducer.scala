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

  test("cost of a no-arc system is correct") {
    import Semiring.LogSpace._;
    val fst = Transducer.transducer[Double,Int,Char,Char](Map(0->0.0),Map(0->0.0))();
    assert(fst.cost === 0.0);
  }

  test("cost of a single-self loop system is its closure") {
    import Semiring.LogSpace._;
    val selfLoopScore = -1.0;
    val trueCost = doubleIsLogSpace.closure(selfLoopScore)
    val fst = Transducer.transducer[Double,Int,Char,Char](Map(0->0.0),Map(0->0.0))(Arc(0,0,'\0','\0',selfLoopScore));
    assert(fst.cost === trueCost);
  }

  test("reverse works nicely") {
    import Semiring.LogSpace._;
    val fst = transducer[Double,Int,Char,Char](Map(0->0.0),Map(2->0.0))( Arc(0,1,'\0','\0',-1.0), 
        Arc(1,0,'\0','\0',-2.0), Arc(1,2,'\0',('3'),-3.0), Arc(2,0,'\0',('4'),-5.0));
    val myRevd = transducer[Double,Int,Char,Char](Map(2->0.0),Map(0->0.0))( Arc(1,0,'\0','\0',-1.0), 
        Arc(0,1,'\0','\0',-2.0), Arc(2,1,'\0',('3'),-3.0), Arc(0,2,'\0',('4'),-5.0));
    assert(myRevd.initialStateWeights === fst.reverse.initialStateWeights);
    assert(myRevd === fst.reverse);
  }

  test("Mohri hwa fig12a weight pushing") {
    import Semiring.Tropical._;
    val dsl = new DSL[Int,Double,Char,Char]();
    import dsl._;
    val fst = transducer(Map(0->0.0),Map(3->0.0))(
      0 -> 1 ('a',eps,0),
      0 -> 1 ('b',eps,1),
      0 -> 1 ('c',eps,5),
      0 -> 2 ('d',eps,0),
      0 -> 2 ('e',eps,1),
      1 -> 3 ('e',eps,0),
      1 -> 3 ('f',eps,1),
      2 -> 3 ('e',eps,4),
      2 -> 3 ('f',eps,5)
    );
    val pushed = transducer(Map(0->0.0),Map(3->0.0))(
      0 -> 1 ('a',eps,0),
      0 -> 1 ('b',eps,1),
      0 -> 1 ('c',eps,5),
      0 -> 2 ('d',eps,4),
      0 -> 2 ('e',eps,5),
      1 -> 3 ('e',eps,0),
      1 -> 3 ('f',eps,1),
      2 -> 3 ('e',eps,0),
      2 -> 3 ('f',eps,1)
    );
    assert(pushed === fst.pushWeights(doubleIsTropical));
  }

  test("Mohri hwa fig12c weight pushing") {
    import Semiring.Probability.semiring;
    val dsl = new DSL[Int,Double,Char,Char]();
    import dsl._;
    val fst = transducer(Map(0->1.0),Map(3->1.0))(
      0 -> 1 ('a',eps,0),
      0 -> 1 ('b',eps,1),
      0 -> 1 ('c',eps,5),
      0 -> 2 ('d',eps,0),
      0 -> 2 ('e',eps,1),
      1 -> 3 ('e',eps,0),
      1 -> 3 ('f',eps,1),
      2 -> 3 ('e',eps,4),
      2 -> 3 ('f',eps,5)
    );
    val pushed = transducer(Map(0->15.0),Map(3->1.0))(
      0 -> 1 ('a',eps,0),
      0 -> 1 ('b',eps,1./15),
      0 -> 1 ('c',eps,5./15),
      0 -> 2 ('d',eps,0),
      0 -> 2 ('e',eps,9./15),
      1 -> 3 ('e',eps,0),
      1 -> 3 ('f',eps,1),
      2 -> 3 ('e',eps,4./9),
      2 -> 3 ('f',eps,5./9)
    );
    assert(pushed === fst.pushWeights(semiring));
  }

  test("Mohri hwa fig12d weight pushing") {
    import Semiring.Tropical._;
    val dsl = new DSL[Int,Double,Char,Char]();
    import dsl._;
    val fst = transducer(Map(0->0.0),Map(3->0.0))(
      0 -> 1 ('a',eps,0),
      0 -> 1 ('b',eps,1),
      0 -> 1 ('c',eps,5),
      0 -> 2 ('d',eps,0),
      0 -> 2 ('e',eps,1),
      1 -> 3 ('e',eps,0),
      1 -> 3 ('f',eps,1),
      2 -> 3 ('e',eps,4),
      2 -> 3 ('f',eps,5)
    );
    val minimized = transducer(Map(0->0.0),Map(3->0.0))(
      0 -> 1 ('a',eps,0),
      0 -> 1 ('b',eps,1),
      0 -> 1 ('c',eps,5),
      0 -> 1 ('d',eps,4),
      0 -> 1 ('e',eps,5),
      1 -> 3 ('e',eps,0),
      1 -> 3 ('f',eps,1)
    );
    assert(minimized === fst.minimize(doubleIsTropical));
  }

}
