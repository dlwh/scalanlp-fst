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
