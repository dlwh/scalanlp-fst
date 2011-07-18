package scalanlp.fst

/*
 Copyright 2010 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/


import scalanlp.math._
import scalala.collection.sparse.DefaultArrayValue
import util.MapMaker
import scalanlp.collection.mutable.AutoUpdater

trait Distance[-CC,W,State] {
  /**
   * return the sum of all paths from the start states
   * to each state. Dispatches based on automata cyclicity
   */
  def allPathDistances(fst: CC): Map[State,W]

  /**
   * For suitable graphs, return the sum of all paths from the start states
   * to each state. May not work on cyclic automata
   */
  def singleSourceShortestDistances(fst: CC):Map[State,W]

  /**
   * Finds all pair-wise distances between all points. Works for any complete semiring.
   */
  def allPairDistances(fst: CC):Map[State,Map[State,W]]
}

/**
 * Provides routines for computing the distance/score/costs of various
 * paths through the automaton.
 */
object Distance {
  /**
   * return the sum of all paths from the start states
   * to each state. Dispatches based on automata cyclicity
   */
  def allPathDistances[CC,W,State](fst: CC)(implicit distance: Distance[CC,W,State]): Map[State,W] = {
    distance.allPathDistances(fst)
  }

  /**
   * For suitable graphs, return the sum of all paths from the start states
   * to each state. May not work on cyclic automata
   */
  def singleSourceShortestDistances[CC,W,State](fst: CC)(implicit distance: Distance[CC,W,State]):Map[State,W] = {
    distance.singleSourceShortestDistances(fst)
  }

  /**
   * Finds all pair-wise distances between all points. Works for any complete semiring.
   */
  def allPairDistances[CC,W,State](fst: CC)(implicit distance: Distance[CC,W,State]):Map[State,Map[State,W]] = {
    distance.allPairDistances(fst)
  }

  implicit def defaultDistancer[CC,W,State,T](implicit ev: CC<:<Automaton[W,State,T],
                                              mm2: MapMaker[CC,State,collection.mutable.Map[State,W]],
                                              mm3: MapMaker[CC,State,Int],
                                              mm: MapMaker[CC,State,W],
                                              ring: Semiring[W]) = {
    new DefaultDistancer[CC,W,State,T];
  }

  class DefaultDistancer[CC,W,State,T](implicit  mm2: MapMaker[CC,State,collection.mutable.Map[State,W]],
                                       mm3: MapMaker[CC,State,Int],
                                       mm: MapMaker[CC,State,W],
                                       ring: Semiring[W],
                                       ev: CC<:<Automaton[W,State,T]) extends Distance[CC,W,State] {

    /**
     * Returns the sum of paths from the start states to each state.
     * Selects between singleSourceShortestDistance and allPairDistances
     * based on cyclicity
     */
    def allPathDistances(fst: CC) = if(fst.isCyclic) {
      val allPairs = allPairDistances(fst);

      val paths = mm.mkMap(fst).withDefaultValue(ring.zero);
      for( (from,initWeight) <- fst.initialStateWeights;
           (to,pathWeight) <- allPairs(from)) {
        paths(to) = ring.maybe_+=(paths(to),ring.times(initWeight,pathWeight))._1;
      }
      Map.empty ++ paths withDefaultValue(ring.zero);
    } else {
      singleSourceShortestDistances(fst);
    }

    /**
     * For suitable graphs, return the sum of all paths from the start states
     * to each state.
     *

     * Implements Generic-Single-Source-Shortest-Distance described in Mohri(2002)
     * with extra support for doing closure operations on selfloops. Only works
     * for acyclic graphs, k-closed semirings, or grahs that are acyclic except
     * for self-loops
     */
    def singleSourceShortestDistances(fst: CC) :Map[State,W] = {
      import ring._;
      val (distances,allStates) = neighborDistances(fst);

      val auto = ev(fst);
      import auto._;


      val d = AutoUpdater(mm.mkMap(fst), zero);
      val r = AutoUpdater(mm.mkMap(fst), zero);
      val selfLoops = AutoUpdater(mm.mkMap(fst), zero);

      val S = new collection.mutable.Queue[State]();
      val visited = AutoUpdater(mm3.mkMap(fst),0)
      val enqueued = AutoUpdater(mm3.mkMap(fst),0);
      for( (s,w) <- initialStateWeights if !closeTo(w,zero)) {
        d(s) = plus(w,zero);
        r(s) = plus(w,zero);
        S += s;
        enqueued(s) = 1;
      }

      while(!S.isEmpty) {
        val from = S.head;
        S.dequeue();
        enqueued(from) = 0;

        visited(from) += 1;

        if(visited(from) == 1) {
          selfLoops(from) = closure(distances(from)(from));
        } else if(visited(from) % 20 == 0) {
          println("Visited " + from + " " + visited(from) + " times!");
        }
        val dkk_star = selfLoops(from);

        r(from) = times(r(from),dkk_star);

        val rFrom = r(from);
        r -= from;

        for( (to,w) <- distances(from) if !closeTo(w,zero) && from != to) {
          val dt = d(to);
          val wRFrom = times(rFrom,w);
          val (dt_p_wRFrom,tooCloseToMatter) = maybe_+=(dt,wRFrom);
          if( !tooCloseToMatter ) {
            r(to) = maybe_+=(r(to),wRFrom)._1;
            d(to) = dt_p_wRFrom;
            if(enqueued(to) == 0) {
              S += to;
              enqueued(to) = 1
            }
          }
        }
      }

      for(  (s,mass) <- selfLoops if !closeTo(mass,zero)) {
        d(s) = times(d(s),mass);
      }

      Map.empty ++ d withDefaultValue(zero);
    }

    /*
    * Returns the distances between individual pairs of states using
    * only one hop
    */
    private def neighborDistances(fst: CC) = {
      import ring._;
      val distances = AutoUpdater(mm2.mkMap(fst), mm.mkMap(fst).withDefaultValue(ring.zero));
      val allStates = new collection.mutable.HashSet[State];
      fst.edges.foreach { case a@Arc(from,to,_,w) =>
        val current = distances(from)(to)
        distances(from)(to) = maybe_+=(current,w)._1;
        allStates += from;
        allStates += to;
      }
      (distances,allStates)
    }

    /**
     * Implements Gen-All-Pairs described in Mohri (2002).
     * Finds all pair-wise distances between all points in O(n^3),
     * where n is the number of states. Works for any complete semiring.
     */
    def allPairDistances(fst: CC) = {
      import ring._;
      val (distances,allStates) = neighborDistances(fst);

      for {
        k <- allStates
      } {
        // cache some commonly used values
        val dkk = distances(k)(k);
        val dkkStar = closure(dkk);

        for {
          (j,dkj) <- distances(k).iterator
          if j != k && !closeTo(dkj,zero)
          i <- allStates if i != k
          dik = distances(i)(k)
          if !closeTo(dik,zero)
        } {
          val current = distances(i)(j);
          val pathsThroughK = times(dik,times(dkkStar,dkj));
          distances(i)(j) = maybe_+=(current,pathsThroughK)._1;
        }

        for (i <- allStates if i != k) {
          distances(k)(i) = times(dkkStar,distances(k)(i));
          distances(i)(k) = times(distances(i)(k),dkkStar);
        }
        distances(k)(k) = dkkStar;
      }

      Map.empty ++ distances.map { case (from,map) =>
        (from,Map.empty ++ map  withDefaultValue zero)
      } withDefaultValue (Map.empty.withDefaultValue(zero))

    }
  }


}
