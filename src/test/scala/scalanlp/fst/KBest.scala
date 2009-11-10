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
class KBestTest extends FunSuite with Checkers {
  import Automaton._;
  test("Simple KBest") {
    val a = constant("Hello",1.0);
    val b = constant("HellX",0.2);
    val c = constant("GoodBye",0.1);
    val kbest = KBest.extract(a | b | c)
    val list = kbest.map{ case (deriv,w) => (deriv.mkString,w) }.toList;
    assert(list === List(("Hello",1.0),("HellX",0.2),("GoodBye",0.1)));
  }
}
