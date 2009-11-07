package scalanlp.fst;

import scala.collection.mutable.ArrayBuffer;
import scala.collection.mutable.PriorityQueue;
import scala.util.control.Breaks._;

import scalanlp.math._;

object KBest {


  def extract[W:Ordering,State,T](auto: Automaton[W,State,T], numToExtract: Int) = {
    case class Derivation(reversedDeriv: List[T], state: State, weight: W, atFinal: Boolean);

    implicit val orderDeriv = Ordering[W].on[Derivation](_.weight);

    var kbest = new ArrayBuffer[(Seq[T],W)](numToExtract);
    import auto.ring._;
    val pq = new PriorityQueue[Derivation];
    for( (state,w) <- auto.initialStateWeights) {
      pq += Derivation(Nil,state,w,false);
    }

    while(!pq.isEmpty && kbest.size < numToExtract) {
      val deriv@Derivation(rev,state,weight,atFinal) = pq.dequeue();

      if(atFinal) {
        kbest += ( (rev.reverse,weight)); 
      } else {
        val finalWeight = auto.finalWeight(state);
        if(finalWeight != zero) {
          pq += Derivation(rev,state,times(weight,finalWeight),true);
        }

        for( Arc(_,to,ch,_,w) <- auto.edgesFrom(state)) {
          val newDeriv = if(ch == auto.inAlpha.epsilon) rev else ch :: rev;
          pq += Derivation(newDeriv,to,times(weight,w),false);
        }
      }
    }

    kbest.toSeq;
  }
}

