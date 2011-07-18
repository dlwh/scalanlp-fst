package scalanlp.fst

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Before


import scala.collection.Traversable;
import scala.collection.Seq;
import scala.collection.mutable.PriorityQueue
import collection.immutable.BitSet


/**
 * 
 * @author dlwh
 */
@RunWith(classOf[JUnitRunner])
class DeterminizeTest extends FunSuite {
  import Automaton._;
  test("basic test") {
    val dsl = new DSL[Boolean,Int]();
    import dsl._;
    val auto = automaton(Map(1->true),Map(3->true,4->true) withDefaultValue (false)) (
      1 -> 2 (0,true),
      1 -> 2 (1,true),
      1 -> 4 (1,true),
      1 -> 4 (0,true),
      2 -> 2 (1,true),
      2 -> 4 (1,true),
      3 -> 2 (1,true),
      3 -> 4 (1,true),
      3 -> 4 (0,true),
      4 -> 2 (0,true)
    )

    val result = {
      val dsl2 = new DSL[Boolean,Int]();
      import dsl2._;
      val auto = automaton(Map(BitSet(1)->true), Map(BitSet(2,4)->true)) (
        BitSet(1) -> BitSet(2,4) (1,true),
        BitSet(1) -> BitSet(2,4) (0,true),
        BitSet(2,4) -> BitSet(2,4) (1,true),
        BitSet(2,4) -> BitSet(2) (0,true),
        BitSet(2) -> BitSet(2,4) (1,true)
      )
      auto
    }

    assert(auto.determinize.edges.toSet === result.edges.toSet);


  }
}