package scalanlp.fst.fast

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
import scalala.tensor.sparse.{SparseHashVector, SparseVector}
import scalala.tensor.Vector

/**
 * Provides routines for computing the distance/score/costs of various
 * paths through the automaton.
 */
trait Distance[T] { this: AutomatonFactory[T] =>

  /**
  * Returns the sum of paths from the start state to each state.
  * Selects between singleSourceShortestDistance and allPairDistances
  * based on cyclicity
  */
  def allPathDistances(fst: Automaton) = if(fst.isCyclic) {
    val allPairs = allPairDistances(fst);

    val paths = Array.fill(fst.numStates)(ring.zero);
    for( (to,pathWeight) <- allPairs(fst.initialState).activeElements) {
      paths(to) = ring.times(fst.initialWeight,pathWeight);
    }
    paths;
  } else {
    singleSourceShortestDistances(fst);
  }

  /** 
  * For suitable graphs, return the sum of all paths from the start states 
  * to each state.
  *

  * Implements Generic-Single-Source-Shortest-Distance described in Mohri(2002)
  * with extra support for doing closure operations on selfloops. Only works
  * for acyclic graphs, k-closed semirings, or graphs that are acyclic except
  * for self-loops.
  */
  def singleSourceShortestDistances(fst: Automaton):Array[Double] = {
    import ring._;
    import fst._;
    val distances = neighborDistances(fst);

    val d = Array.fill(numStates)(zero);
    val r = Array.fill(numStates)(zero);
    val selfLoops = Array.fill(numStates)(zero);

    val S = new collection.mutable.Queue[Int]();
    val visited = Array.fill(numStates)(0);
    val enqueued = collection.mutable.BitSet();
    d(fst.initialState) = initialWeight;
    r(fst.initialState) = initialWeight;
    S += initialState;
    enqueued += initialState;

    while(!S.isEmpty) {
      val from = S.dequeue();
      enqueued -= from
      visited(from) += 1;

      if(visited(from) == 1) {
        selfLoops(from) = closure(distances(from)(from));
      } else if(visited(from) % 20 == 0) {
        println("Visited " + from + " " + visited(from) + " times!");
      }
      val dkk_star = selfLoops(from);

      r(from) = times(r(from),dkk_star);

      val rFrom = r(from);
      r(from) = zero;

      for( (to,w) <- distances(from).activeElements if !closeTo(w,zero) && from != to) {
        val dt = d(to);
        val wRFrom = times(rFrom,w);
        val dt_p_wRFrom = plus(dt,wRFrom);
        if( !closeTo(dt,dt_p_wRFrom) ) {
          r(to) = plus(r(to),wRFrom);
          d(to) = dt_p_wRFrom;
          if(!enqueued(to)) {
            S += to;
            enqueued += to;
          }
        }
      }
    }

    for(  (mass,s) <- selfLoops.zipWithIndex if !closeTo(mass,zero)) {
      d(s) = times(d(s),mass);
    }

    d
  }

  /*
  * Returns the distances between individual pairs of states using
  * only one hop
  */
  private def neighborDistances(fst: Automaton): Array[_ <: Vector] = {
    import ring._;

    val distances = Array.fill(fst.numStates){
      val r = new SparseHashVector(fst.numStates);
      r.default = ring.zero;
      r;
    }
    for {
      from <- 0 until fst.numStates;
      (_,targets) <- fst.arcsFrom(from);
      (to,weight) <- targets.activeElements
    } {
      val current = distances(from)(to);
      distances(from)(to) = plus(current,weight)
    }
    distances
  }

  /**
  * Implements Gen-All-Pairs described in Mohri (2002).
  * Finds all pair-wise distances between all points in O(n^3),
  * where n is the number of states. Works for any complete semiring.
  */
  def allPairDistances(fst: Automaton) = {
    import ring._;
    import fst._;
    val distances = neighborDistances(fst);

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
        distances(i)(j) = plus(current,pathsThroughK)
      }

      for (i <- allStates if i != k) {
        distances(k)(i) = times(dkkStar,distances(k)(i));
        distances(i)(k) = times(distances(i)(k),dkkStar);
      }
      distances(k)(k) = dkkStar;
    }

    distances

  }


}
