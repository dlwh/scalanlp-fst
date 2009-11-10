package scalanlp.fst;

import scala.collection.mutable.PriorityQueue;

import scalanlp.math._;

object KBest {
  def extract[W:Ordering,State,T](auto: Automaton[W,State,T]):Iterator[(Seq[T],W)] = {
    case class Derivation(reversedDeriv: List[T], state: State, weight: W, atFinal: Boolean);
    implicit val orderDeriv = Ordering[W].on[Derivation](_.weight);

    val derivations = new Iterator[Derivation] {
      import auto.ring._;
      val pq = new PriorityQueue[Derivation];
      for( (state,w) <- auto.initialStateWeights) {
        pq += Derivation(Nil,state,w,false);
      }

      def hasNext:Boolean = {
        !pq.isEmpty
      }

      def next = {
        val deriv@Derivation(rev,state,weight,atFinal) = pq.dequeue;
        if(!atFinal) {
          val finalWeight = auto.finalWeight(state);
          if(finalWeight != zero) {
            pq += Derivation(rev,state,times(weight,finalWeight),true);
          }

          for( Arc(_,to,ch,_,w) <- auto.edgesFrom(state)) {
            val newDeriv = if(ch == auto.inAlpha.epsilon) rev else ch :: rev;
            pq += Derivation(newDeriv,to,times(weight,w),false);
          }
        }
        deriv;
      }
    }

    val kbest = for( deriv <- derivations if deriv.atFinal)
      yield (deriv.reversedDeriv.reverse,deriv.weight);

    kbest
  }
}

