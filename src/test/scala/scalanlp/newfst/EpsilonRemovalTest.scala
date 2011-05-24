package scalanlp.newfst

/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */



import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.FunSuite

@RunWith(classOf[JUnitRunner])
class EpsilonRemovalTest extends FunSuite {
  test("Mohri hwa fig 10 epsilon removal") {
    val dsl = new Transducer.DSL[Int,Double,Char,Char]();
    import dsl._;
    val t1 = transducer( Map(0->1.), Map(4->.2))(
      0 -> (1)('a','b',.1),
      1 -> 2 (eps,eps,.2),
      1 -> 4('b','a',.2),
      2 -> 0 ('b','a',.1),
      2 -> 1 ('a','b',.4),
      2 -> 4 ('a','b',.4),
      2 -> 4 (eps,eps,.25),
      2 -> 3 (eps,eps,.5),
      3 -> 0 (eps,eps,.4),
      3 -> 4 (eps,eps,1.0)
    );

    // damn floating point
    val t2 = transducer( Map(0->1.), Map(1-> 0.030000000000000006,4->.2))(
      0 -> 1  ('a','b',.1),
      1 -> 0 ('b','a',0.020000000000000004),
      1 -> 1 ('a','b',0.08400000000000002),
      1 -> 4 ('a','b',0.08000000000000002),
      1 -> 4 ('b','a',0.20)
    );

    val result = EpsilonRemoval.removeEpsilons(t1).collapseEdges;
    assert(result === t2);
 }


}
