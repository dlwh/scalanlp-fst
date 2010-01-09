package scalanlp.fst

import scalanlp.math._;

object Distance {

  /**
  * Returns the sum of paths from the start states to each state.
  * Selects between singleSourceShortestDistance and allPairDistances
  * based on cyclicity
  */
  def allPathDistances[W:Semiring,State,T](fst: Automaton[W,State,T]) = if(fst.isCyclic) {
    val ring = implicitly[Semiring[W]];

    val allPairs = allPairDistances(fst);

    val paths = fst.makeMap(ring.zero);
    for( (from,initWeight) <- fst.initialStateWeights;
         (to,pathWeight) <- allPairs(from)) {
      paths(to) = ring.plus(paths(to),ring.times(initWeight,pathWeight));
    }
    Map.empty ++ paths;
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
  def singleSourceShortestDistances[W:Semiring,State,T](fst: Automaton[W,State,T]):Map[State,W] = {
    val ring = implicitly[Semiring[W]];
    import ring._;

    val (distances,allStates) = neighborDistances(fst);

    import fst._;

    val d = makeMap[W](zero);
    val r = makeMap[W](zero);
    val selfLoops = makeMap[W](zero);

    val S = new collection.mutable.Queue[State]();
    val visited = makeMap(0);
    val enqueued = makeMap(false);
    for( (s,w) <- initialStateWeights if w != zero) {
      d(s) = w;
      r(s) = w;
      S += s;
      enqueued(s) = true;
    }

    while(!S.isEmpty) {
      val from = S.head;
      S.dequeue();
      enqueued(from) = false;

      visited(from) += 1;

      if(visited(from) == 1) {
        selfLoops(from) = closure(distances(from)(from));
      }
      val dkk_star = selfLoops(from);

      r(from) = times(r(from),dkk_star);

      val rFrom = r(from);
      r -= from;
      
      for( (to,w) <- distances(from) if w != zero && from != to) {
        val dt = d(to);
        val wRFrom = times(rFrom,w);
        val dt_p_wRFrom = plus(dt,wRFrom);
        if( !closeTo(dt,dt_p_wRFrom) ) {
          r(to) = plus(r(to),wRFrom);
          d(to) = dt_p_wRFrom;
          if(!enqueued(to)) {
            S += to;
            enqueued(to) = true
          }
        }
      }
    }

    for(  (s,mass) <- selfLoops if mass != zero) {
      d(s) = times(d(s),mass);
    }


    Map.empty ++ d;
  }

  /*
  * Returns the distances between individual pairs of states using
  * only one hop
  */
  private def neighborDistances[W:Semiring,State,T](fst: Automaton[W,State,T]) = {
    val ring = implicitly[Semiring[W]];
    import ring._;
    import fst._;

    val distances = makeMap(makeMap(zero));
    val allStates = makeMap[State](null.asInstanceOf[State]); // XXX
    allEdges.foreach { case Arc(from,to,_,w) =>
      val current = distances(from)(to);
      distances(from)(to) = plus(current,w);
      allStates(from) = from;
      allStates(to) = to;
    }
    (distances,allStates)
  }

  /**
  * Implements Gen-All-Pairs described in Mohri (2002).
  * Finds all pair-wise distances between all points in O(n^3),
  * where n is the number of states. Works for any complete semiring.
  */
  def allPairDistances[W:Semiring,State,T](fst: Automaton[W,State,T]) = {
    val ring = implicitly[Semiring[W]];
    import ring._;
    val (distances,allStates) = neighborDistances(fst);

    import fst._;
   

    for {
      k <- allStates.keysIterator
    } {
      // cache some commonly used values
      val dkk = distances(k)(k);
      val dkkStar = closure(dkk);

      for {
        (j,dkj) <- distances(k).iterator
        if j != k && dkj != zero
        i <- allStates.keysIterator
        if i != k
        dik = distances(i)(k)
        if dik != zero
      } {
        val current = distances(i)(j);
        val pathsThroughK = times(dik,times(dkkStar,dkj));
        distances(i)(j) = plus(current,pathsThroughK);
      }

      for (i <- allStates.keysIterator if i != k) {
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
