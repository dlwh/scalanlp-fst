package scalanlp.fst;
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



import scala.collection.mutable.PriorityQueue;
import scala.collection.mutable.ArrayBuffer;

import scalanlp.math._
import scalala.collection.sparse.DefaultArrayValue
;

/**
* Returns kbest derivations from an automaton. Naturally, the semiring involved needs to have an ordering.
*
*
* @author dlwh
*/
trait KBest {
  case class Derivation[W,State,T](str: ArrayBuffer[T], state: State, weight: W,  heuristic: W, atFinal: Boolean);
  implicit def orderDeriv[W:Ordering,State,T] = Ordering[W].on[Derivation[W,State,T]](_.heuristic);

  protected def computeHeuristics[W:Semiring:ClassManifest:DefaultArrayValue,State,T](auto: Automaton[W,State,T]): (State=>W);

  def extract[W:Semiring:Ordering:ClassManifest:DefaultArrayValue,State,T:Alphabet](auto: Automaton[W,State,T]) :Iterator[(Seq[T],W)] = {
    val heuristics = computeHeuristics(auto);

    val pq = initialPQ(auto,heuristics);

    val derivations = new Iterator[Derivation[W,State,T]] {

      def hasNext:Boolean = {
        !pq.isEmpty
      }

      def next = {
        relax(auto,pq,heuristics);
      }
    }

    val kbest = for( deriv <- derivations if deriv.atFinal)
      yield (deriv.str,deriv.weight);

    kbest
  }

  def extractList[W:ClassManifest:Semiring:Ordering:DefaultArrayValue,State,T:Alphabet](auto: Automaton[W,State,T], num: Int)
                            :Seq[(Seq[T],W)] = {
    val heuristics = computeHeuristics(auto);

    val pq = initialPQ(auto,heuristics);
    val kbest = new ArrayBuffer[(Seq[T],W)];

    while(!pq.isEmpty && kbest.length < num) {
      //println(pq.size);
      val deriv = relax(auto,pq,heuristics);
      if(deriv.atFinal) {
        kbest += ((deriv.str,deriv.weight));
      }
    }

    kbest
  }

  private def initialPQ[W,State,T](auto: Automaton[W,State,T],heuristics: State=>W)(implicit ring: Semiring[W], ord: Ordering[W]) = {
    val pq = new PriorityQueue[Derivation[W,State,T]];
    for( (state,w) <- auto.initialStateWeights) {
      pq += Derivation(ArrayBuffer.empty,state,w,ring.times(w,heuristics(state)),false);
    }

    pq

  }

  // Pops the top derivation off and adds any descendents to the queue, returns that derivation
  private def relax[W,S,T](auto: Automaton[W,S,T], pq: PriorityQueue[Derivation[W,S,T]], heuristics: S=>W)
                          (implicit ring: Semiring[W], ord: Ordering[W], alphabet: Alphabet[T]) = {
    import ring._;
    val deriv@Derivation(str,state,weight,_,atFinal) = pq.dequeue;
    if(!atFinal) {
      val finalWeight = auto.finalWeight(state);
      if(finalWeight != zero) {
        val finalScore = times(weight,finalWeight);
        pq += Derivation(str,state,finalScore,finalScore,true);
      }

      for( Arc(_,to,ch,w) <- auto.edgesFrom(state)) {
        val newDeriv = if(ch == alphabet.epsilon) str else (str.clone() += ch)
          val newWeight = times(weight,w);
        val newHeuristic = times(newWeight,heuristics(to));
        pq += Derivation(newDeriv,to,newWeight,newHeuristic,false);
      }
    } 

    deriv;
  }
}

/**
* Uses an A* heuristic to do the kbest list.
*/
object KBest extends KBest {

  override protected def computeHeuristics[W:Semiring:ClassManifest:DefaultArrayValue,State,T](auto: Automaton[W,State,T]) = {
    Distance.allPathDistances(auto.reverse) withDefaultValue implicitly[Semiring[W]].zero;
  }
}

/**
* Uses no heuristic to do the kbest list.
*/
object UniformCostKBest extends KBest {
  override protected def computeHeuristics[W:Semiring:ClassManifest:DefaultArrayValue,State,T](auto: Automaton[W,State,T]) = {
    { (s: State) => implicitly[Semiring[W]].zero }
  }
}
