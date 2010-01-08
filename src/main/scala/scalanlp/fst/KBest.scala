package scalanlp.fst;

import scala.collection.mutable.PriorityQueue;
import scala.collection.mutable.ArrayBuffer;

import scalanlp.math._;

trait KBest {
  case class Derivation[W,State,T](str: ArrayBuffer[T], state: State, weight: W,  heuristic: W, atFinal: Boolean);
  implicit def orderDeriv[W:Ordering,State,T] = Ordering[W].on[Derivation[W,State,T]](_.heuristic);

  protected def computeHeuristics[W:Semiring,State,T](auto: Automaton[W,State,T]): (State=>W);

  def extract[W,State,T](auto: Automaton[W,State,T])
                        (implicit ring: Semiring[W], ord: Ordering[W], alphabet : Alphabet[T]):Iterator[(Seq[T],W)] = {
    val heuristics = computeHeuristics(auto)(ring);

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

  def extractList[W,State,T](auto: Automaton[W,State,T], num: Int)
                            (implicit ring: Semiring[W], ord: Ordering[W], alphabet: Alphabet[T]):Seq[(Seq[T],W)] = {
    val heuristics = computeHeuristics(auto)(ring);

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

object KBest extends KBest {

  override protected def computeHeuristics[W:Semiring,State,T](auto: Automaton[W,State,T]) = {
    Distance.allPathDistances(auto.reverse);
  }
}

object UniformCostKBest extends KBest {
  override protected def computeHeuristics[W:Semiring,State,T](auto: Automaton[W,State,T]) = {
    { (s: State) => implicitly[Semiring[W]].zero }
  }
}
