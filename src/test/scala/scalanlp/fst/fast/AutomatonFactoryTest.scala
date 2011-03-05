package scalanlp.fst.fast;

import scalanlp.fst.Alphabet;
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.Before


import scalanlp.math._;
import scala.collection.Traversable;
import scala.collection.Seq;
import scala.collection.mutable.PriorityQueue
import scalanlp.util.Index;


@RunWith(classOf[JUnitRunner])
class AutomatonFactoryTest extends FunSuite {
  val factory = new AutomatonFactory[Char](Index( Set() ++ ('a' to 'g') + implicitly[Alphabet[Char]].epsilon));
  import factory._;

  test("basics") {
    val a = automaton("abc",1.0);
    val b = automaton("bcde",1.0);
    assert(a.cost === 1.0);
    assert(intersect(a,b).cost === 0.0);
    assert(intersect(a,a).cost === 1.0);
  }


  test("Mohri hwa fig 7 epsilon-full composition") {
    import dsl._;
    val t1 = transducer( 0->1.0, Map(4->1.0))(
      0 -> (1)('a','a',1.0),
      1 -> 2 ('b',eps,1.0),
      2 -> 3 ('c',eps,1.0),
      3 -> (4)('d','d',1.0)
    );
    val t2 = transducer( 0->1.0, Map(3->1.0))(
      0 -> (1)  ('a','d',1.0),
      1 -> 2 (eps,'e',1.0),
      2 -> (3)  ('d','a',1.0)
    );
    val comps = compose(t1,t2);
    assert(comps.arcsFrom(destination(t1.numStates,0,0,NoEps))(index('a'))(index('d'))(destination(t1.numStates,1,1,NoEps)) === 1.0);
    assert(comps.arcsFrom(destination(t1.numStates,0,0,NoEps),index('a'),index('d'))(destination(t1.numStates,1,1,NoEps)) === 1.0);
    assert(comps.arcsFrom(destination(t1.numStates,1,1,NoEps))(index('b'))(epsilonIndex)(destination(t1.numStates,2,1,LeftEps)) === 1.0);
    assert(comps.arcsFrom(destination(t1.numStates,1,1,NoEps),index('b'),epsilonIndex)(destination(t1.numStates,2,1,LeftEps)) === 1.0);
    assert(comps.arcsFrom(destination(t1.numStates,1,1,NoEps))(index('b'))(index('e'))(destination(t1.numStates,2,2,NoEps)) === 1.0);
    assert(comps.arcsFrom(destination(t1.numStates,1,1,NoEps))(epsilonIndex)(index('e'))(destination(t1.numStates,1,2,RightEps)) === 1.0);
    assert(comps.arcsFrom(destination(t1.numStates,2,1,LeftEps))(index('c'))(epsilonIndex)(destination(t1.numStates,3,1,LeftEps)) === 1.0);
    assert(comps.arcsFrom(destination(t1.numStates,2,2,NoEps))(index('c'))(epsilonIndex)(destination(t1.numStates,3,2,LeftEps)) === 1.0);
    assert(comps.arcsFrom(destination(t1.numStates,3,2,LeftEps))(index('d'))(index('a'))(destination(t1.numStates,4,3,NoEps)) === 1.0);

    /*
    val result = {
      val dsl = new DSL[(Int,Int,InboundEpsilon),Boolean,Char,Char]();
      import dsl._;
      transducer(Map((0,0,NoEps:InboundEpsilon)->true),Map( (4,3,NoEps:InboundEpsilon)-> true))( 
        (0,0,NoEps)   -> (1,1,NoEps)    ('a','d',true),
        (1,1,NoEps)   -> (2,1,LeftEps)  ('b',eps,true),
        (1,1,NoEps)   -> (2,2,NoEps)    ('b','e',true),
        (1,1,NoEps)   -> (1,2,RightEps) (eps,'e',true),
        (2,1,LeftEps) -> (3,1,LeftEps)  ('c',eps,true),
        (2,2,NoEps)   -> (3,2,LeftEps)  ('c',eps,true),
        (3,2,LeftEps) -> (4,3,NoEps)    ('d','a',true)
      )
    };
    assert(result === (t1 >> t2))
    */
  }

  /*
  test("cost of a one-arc system is correct") {
    import Semiring.LogSpace._;
    val fst = Transducer.transducer[Double,Int,Char,Char](Map(0->0.0),Map(1->0.0))(Arc(0,1,('a','b'),0.0));
    assert(fst.cost === 0.0);
  }

  test("cost of a single-self loop system is its closure") {
    import Semiring.LogSpace._;
    val selfLoopScore = -1.0;
    val trueCost = doubleIsLogSpace.closure(selfLoopScore)
    val fst = Transducer.transducer[Double,Int,Char,Char](Map(0->0.0),Map(0->0.0))(Arc(0,0,('\0','\0'),selfLoopScore));
    assert(fst.cost === trueCost);
  }

  test("reverse works nicely") {
    import Semiring.LogSpace._;
    import Automaton._;
    val fst = automaton[Double,Int,Char](Map(0->0.0),Map(2->0.0))( Arc(0,1,'\0',-1.0),
        Arc(1,0,'\0',-2.0), Arc(1,2,('3'),-3.0), Arc(2,0,'4',-5.0));
    val myRevd = automaton[Double,Int,Char](Map(2->0.0),Map(0->0.0))( Arc(1,0,'\0',-1.0),
        Arc(0,1,'\0',-2.0), Arc(2,1,('3'),-3.0), Arc(0,2,('4'),-5.0));
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

  test("relabel doesn't screw up cost") {
    import Semiring.LogSpace._;
    val a = Automaton.constant("aa",0.0).asTransducer;
    val ed = new EditDistance(-3,-3,Set('a','b'));
    val pushed = (a >> ed).pushWeights(doubleIsLogSpace);
    val pC = pushed.cost;
    val prC = pushed.relabel.cost;
    assert( (pC - prC).abs < 1E-6, pC + " " + prC);
  }
  */

}
