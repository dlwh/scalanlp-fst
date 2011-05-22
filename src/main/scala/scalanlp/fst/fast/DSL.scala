package scalanlp.fst.fast

import scalanlp.fst.Alphabet
import scalanlp.tensor.sparse.OldSparseVector

/**
 * 
 * @author dlwh
 */
trait DSL[T] { outer:AutomatonFactory[T] =>
  object dsl {
    private val epsilon = implicitly[Alphabet[T]].epsilon
    class Extras(to: Int) {
      def apply(in: T, out: T, weight: Double) = (to,in,out,weight);
      def apply(in: T, out: eps.type, weight: Double) = (to,in,epsilon,weight);
      def apply(in: eps.type, out: T, weight: Double) = (to,epsilon,out,weight);
      def apply(in: eps.type, out: eps.type, weight: Double) = (to,epsilon,epsilon,weight);
      def apply(label: T, weight: Double) = (to,(label),weight);
      def apply(label: eps.type, weight: Double) = (to,epsilon,weight);
    }
    object eps

    implicit def extras(t: Int) = new Extras(t);

    def transducer(initialState: (Int,Double), finalWeights: Map[Int,Double])(arcs: (Int,(Int,T,T,Double))*): Transducer = {
      val numStates = (arcs.map(_._1).max max arcs.map(_._2._1).max max finalWeights.keys.max max initialState._1)+1
      val allArcs = Array.fill(numStates){
        encoder.fillSparseArrayMap(encoder.fillSparseArrayMap{
          val vec = new OldSparseVector(numStates);
          vec.default = ring.zero
          vec
        });
      }

      for( (from,(to,in,out,w)) <- arcs) {
        val vec = allArcs(from).getOrElseUpdate(index(in)).getOrElseUpdate(index(out))
        vec(to) = ring.plus(vec(to),w);
      }
      val fWeights = Array.tabulate(numStates) { i => finalWeights.getOrElse(i,ring.zero)}
      outer.transducer(allArcs,fWeights,initialState._1,initialState._2);
    }

    def automaton(initialState: (Int,Double), finalWeights: Map[Int,Double])(arcs: (Int,(Int,T,Double))*): Automaton = {
      val numStates = arcs.map(_._1).max max arcs.map(_._2._1).max max finalWeights.keys.max max initialState._1
      val allArcs = Array.fill(numStates){
        encoder.fillSparseArrayMap{
          val vec = new OldSparseVector(numStates);
          vec.default = ring.zero
          vec
        };
      }

      for( (from,(to,in,w)) <- arcs) {
        val vec = allArcs(from).getOrElseUpdate(index(in))
        vec(to) = ring.plus(vec(to),w);
      }
      val fWeights = Array.tabulate(numStates) { i => finalWeights.getOrElse(i,ring.zero)}
      outer.automaton(allArcs,fWeights,initialState._1,initialState._2);
    }
  }
}