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
class EditDistanceTest extends FunSuite with Checkers {
  import Transducer._;
  test("ed is markovian") {
    import scalanlp.math.Semiring.LogSpace._;
    val ed = new EditDistance(-3,-4,Set('a','b','c'));
    val allWeights = ed.edgesFrom(()).map(_.weight);
    val totalMass = allWeights.reduceLeft(logSum _);
    assert(totalMass.abs < 1E-10,totalMass + " to far from zero!");
  }
}
