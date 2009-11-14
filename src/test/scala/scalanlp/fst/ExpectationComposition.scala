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
class ExpCompTest extends FunSuite {
  import Transducer._;
  import ExpectationComposition.logExpectedWeightIsSemiring._;
  test("logWeight zero + zero == zero") {
    assert(plus(zero,zero) === zero)
  }
}
