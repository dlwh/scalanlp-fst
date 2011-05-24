package scalanlp.newfst

import util.MapMaker
import scalanlp.collection.mutable.AutoUpdater

/**
 * 
 * @author dlwh
 */

object Topology {

  def isCyclic[Auto,W,State,T](auto: Auto)(implicit mapMaker: MapMaker[Auto,State,Int], ev: Auto<:<Automaton[W,State,T]) = {
    val aa = ev(auto);
    import aa._;
    val WHITE = 0
    val GREY = 1
    val BLACK = 2
    val visited = AutoUpdater(mapMaker.mkMap(auto),WHITE);
    var cyclic = false;
    import scala.util.control.Breaks._;

    def visit(p: State) {
      assert(visited(p) != GREY);
      visited(p) = GREY;
      for ( Arc(_,q,_,_) <- edgesFrom(p) if q != p ) {
        if(visited(q) == GREY) {
          cyclic = true;
          break;
        } else if(visited(q) == WHITE) {
          visit(q);
        }
      }
      visited(p) = BLACK;
    }


    breakable {
      for {
        p <- initialStateWeights.keysIterator;
        if visited(p) == WHITE
      } {
        visit(p);
      }
    }

    cyclic;


  }

}
