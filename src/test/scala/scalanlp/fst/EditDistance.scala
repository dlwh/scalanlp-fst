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
    assert(totalMass.abs < 1E-10, totalMass + " to far from zero!");
  }

  test("rho and nonRho are similar") {
    val ed = new EditDistance(-3,-4,Set('a','b'),0,Math.log(.9));
    val ed2 = new EditDistance(-3,-4,Set.empty,2,Math.log(.9));
    assert( (ed.cost - ed2.cost) < 1E-6);
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

}
