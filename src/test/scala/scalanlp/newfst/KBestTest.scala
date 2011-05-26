package scalanlp.newfst

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class KBestTest extends FunSuite {
  import Automaton._;
  test("Simple KBest") {
//    implicit val h = KBest.aStarHeuristic[scalanlp.newfst.Automaton[Double,Either[Either[Int,Int],Int],Char],scalanlp.newfst.Automaton[Double,Either[Either[Int,Int],Int],Char],Double,Either[Either[Int,Int],Int],Char]
    val a = constant("Hello",1.0);
    val b = constant("HellX",0.2);
    val c = constant("GoodBye",0.1);
    val kbest = KBest.extract(a | b | c)
    val list = kbest.map{ case (deriv,w) => (deriv.mkString,w) }.toList;
    assert(list === List(("Hello",1.0),("HellX",0.2),("GoodBye",0.1)));
  }
}
