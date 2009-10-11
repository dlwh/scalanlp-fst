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
class TransducerTest extends FunSuite {
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
}
