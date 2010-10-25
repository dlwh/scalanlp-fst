package scalanlp.fst

import scalanlp.math.Semiring

/**
* Epsilon-removal removes all epsilons from an automaton by essentially collapsing
* epsilon paths onth the state. That is, if you could go from state 1 to state 2 via epsilon,
* you just copy all the arcs from state 2 to state 1 (weighted by the weight of the epsilon arc).
*
* @author dlwh
*/
object EpsilonRemoval {
  def removeEpsilons[W:Semiring:ClassManifest,S,T:Alphabet](a: Automaton[W,S,T]):Automaton[W,S,T] = {
    val epsilon = implicitly[Alphabet[T]].epsilon;
    val ring = implicitly[Semiring[W]];
    import ring._;

    val epsilonsOnly = a.filterArcs(_.label == epsilon);
    val pairDistances = Distance.allPairDistances(epsilonsOnly);

    val newArcs = for {
      (p,distances) <- pairDistances.iterator
      (q,w) <- distances.iterator
      if w != zero
      Arc(_,r,label,arcWeight) <- a.edgesFrom(q)
      if label != epsilon
    } yield Arc(p,r,label,times(w,arcWeight));


    val newInitialWeights = a.makeMap[W](zero);
    for {
      (p,startW) <- a.initialStateWeights;
      (q,w) <- pairDistances(p);
      if w != zero
    } {
      newInitialWeights(p) = plus(newInitialWeights(p),times(startW,w));
    }

    val newFinalWeights = a.makeMap[W](zero);

    for {
      (p,distances) <- pairDistances;
      (q,w) <- distances;
      if w != zero
      endWeight = a.finalWeight(q)
    } {
      newFinalWeights(p) = plus(newFinalWeights(p),times(w,endWeight));
    }

    Automaton.automaton(Map.empty ++ newInitialWeights,Map.empty ++ newFinalWeights)( (newArcs.toSeq):_*);
  }
}
