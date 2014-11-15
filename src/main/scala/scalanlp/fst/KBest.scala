package scalanlp.fst

/*
 Copyright 2010 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/


import breeze.math.Semiring

import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.ArrayBuffer



/**
* Returns kbest derivations from an automaton. Naturally, the semiring involved needs to have an ordering.
*
*
* @author dlwh
*/
trait KBest {
  case class Derivation[W,State,T](str: ArrayBuffer[T], state: State, weight: W,  heuristic: W, atFinal: Boolean)
  implicit def orderDeriv[W:Ordering,State,T]: Ordering[Derivation[W, State, T]] = Ordering[W].on[Derivation[W,State,T]](_.heuristic)
  trait Heuristic[-CC,State,W] {
    def computeHeuristics(auto: CC): (State=>W)
  }

  def extract[CC,W,State,T](auto: CC)
                           (implicit ev: CC<:<Automaton[W,State,T],
                            ring: Semiring[W],
                            order: Ordering[W],
                           alphabet: Alphabet[T],
                            h: Heuristic[CC,State,W]
                            ) :Iterator[(Seq[T],W)] = {
    val heuristics = h.computeHeuristics(auto)

    val pq = initialPQ(auto,heuristics)

    val derivations = new Iterator[Derivation[W,State,T]] {

      def hasNext:Boolean = {
        !pq.isEmpty
      }

      def next = {
        relax(auto,pq,heuristics)
      }
    }

    val kbest = for( deriv <- derivations if deriv.atFinal)
      yield (deriv.str,deriv.weight)

    kbest
  }

  private def initialPQ[W,State,T](auto: Automaton[W,State,T],heuristics: State=>W)(implicit ring: Semiring[W], ord: Ordering[W]) = {
    val pq = new PriorityQueue[Derivation[W,State,T]]
    for( (state,w) <- auto.initialStateWeights) {
      pq += Derivation(ArrayBuffer.empty,state,w,ring.*(w,heuristics(state)),false)
    }

    pq

  }

  // Pops the top derivation off and adds any descendents to the queue, returns that derivation
  private def relax[W,S,T](auto: Automaton[W,S,T], pq: PriorityQueue[Derivation[W,S,T]], heuristics: S=>W)
                          (implicit ring: Semiring[W], ord: Ordering[W], alphabet: Alphabet[T]) = {
    import ring._
    val deriv@Derivation(str,state,weight,_,atFinal) = pq.dequeue
    if(!atFinal) {
      val finalWeight = auto.finalWeight(state)
      if(finalWeight != zero) {
        val finalScore = *(weight,finalWeight)
        pq += Derivation(str,state,finalScore,finalScore,true)
      }

      for( Arc(_,to,ch,w) <- auto.edgesFrom(state)) {
        val newDeriv = if(ch == alphabet.epsilon) str else (str.clone() += ch)
          val newWeight = *(weight,w)
        val newHeuristic = *(newWeight,heuristics(to))
        pq += Derivation(newDeriv,to,newWeight,newHeuristic,false)
      }
    } 

    deriv
  }
}

/**
* Uses an A* heuristic to do the kbest list.
*/
object KBest extends KBest {

  implicit def aStarHeuristic[CC,CCR,W,State,T](implicit reverser: Reverser[CC,CCR],
                                                dist: Distance[CCR,W,State],
                                                ring: Semiring[W]):Heuristic[CC,State,W]  = new Heuristic[CC,State,W] {
    def computeHeuristics(auto: CC) = {
      Distance.allPathDistances(reverser(auto)) withDefaultValue ring.zero
    }
  }
}

/**
* Uses no heuristic to do the kbest list.
*/
object UniformCostKBest extends KBest {
  implicit def blahHeuristic[CC,State,W](implicit r: Semiring[W]): UniformCostKBest.Heuristic[CC, State, W] = new Heuristic[CC,State,W] {
    def computeHeuristics(auto: CC) = {(s:State)=> r.zero}
  }
}

