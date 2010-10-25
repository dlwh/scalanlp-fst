package scalanlp.fst;

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.scalatest.prop._;
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Before


import scalanlp.math._;
import scala.collection.Traversable;
import scala.collection.Seq;
import scala.collection.mutable.PriorityQueue;



@RunWith(classOf[JUnitRunner])
class DecayAutomatonTest extends FunSuite {
  import Transducer._;
  test("da has cost 0") {
    import scalanlp.math.Semiring.LogSpace._;
    val da = new DecayAutomaton(4,Set('b','c','d'));
    val cost = da.cost;
    assert(cost.abs < 1E-10, cost + " to far from zero!");
  }

}
