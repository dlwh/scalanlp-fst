package scalanlp.fst

import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Before


import breeze.math._;
import scala.collection.immutable.IntMap


@RunWith(classOf[JUnitRunner])
class MinimizeTest extends FunSuite {
  import Transducer._;

  test("Mohri hwa fig12d minimization") {
    import breeze.math._;
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

    val minimized = transducer(Map(2->0.0,0->Double.PositiveInfinity,1->Double.PositiveInfinity),Map(1->0.0))(
      0->1('f',eps,1.0),
      0->1('e',eps,0.0),
      2->0('b',eps,1.0),
      2->0('a',eps,0.0),
      2->0('c',eps,5.0),
      2->0('d',eps,4.0),
      2->0('e',eps,5.0)
    );
    import IdentityPartitioner._;
    assert(minimized.initialStateWeights === Minimizer.minimize(fst.pushWeights).relabel.initialStateWeights );
    assert(minimized === Minimizer.minimize(fst.pushWeights).relabel);
  }

  test("Mohri hwa fig12d minimization with ApproxPartitioner") {
    import breeze.math._;
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

     val minimized = transducer(Map(2->0.0,0->Double.PositiveInfinity,1->Double.PositiveInfinity),Map(1->0.0))(
      0->1('f',eps,1.0),
      0->1('e',eps,0.0),
      2->0('b',eps,1.0),
      2->0('a',eps,0.0),
      2->0('c',eps,5.0),
      2->0('d',eps,4.0),
      2->0('e',eps,5.0)
    );
    import ApproximatePartitioner._;
    import IdentityPartitioner._;
    assert(minimized.initialStateWeights === Minimizer.minimize(fst.pushWeights).relabel.initialStateWeights );
    assert(minimized === Minimizer.minimize(fst.pushWeights).relabel);
  }


  test("minimize works on a simple string") {
    import IdentityPartitioner._;
    val a = Automaton.constant("hello",1.0);
    val minned = Minimizer.minimize(a | a);
    assert(a.states.size === 6);
  }

  test("minimize is reasonably consistent") {
    import Semiring.LogSpace._;
    import ApproximatePartitioner._;
    val words = List("cu","ko");
    val auto = words.map(w => Automaton.constant(w,0.0).asTransducer );
    val ed = new EditDistance(-3,-4,Set.empty ++ words.iterator.flatMap(_.iterator));

    val eds = auto.map(a => (ed >> a).inputProjection);
    val joined = (eds(0) & eds(1)).relabel
    val shrunk = Minimizer.minimize(joined);
    val shrinkCost = shrunk.cost
    val cost = joined.cost
    assert( (shrinkCost - cost).abs < 1E-2, shrinkCost +" vs " + cost);
  }

}
