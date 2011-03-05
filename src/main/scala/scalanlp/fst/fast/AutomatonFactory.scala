package scalanlp.fst.fast

import scalanlp.fst;

import fst.Alphabet
import scalanlp.math.Semiring
import scalanlp.util.{Encoder, Index, DenseIntIndex}
import scalala.tensor.sparse.SparseVector
import scalanlp.collection.mutable.SparseArray
import collection.immutable.BitSet


/**
 * 
 * @author dlwh
 */

class AutomatonFactory[ T](index: Index[T])
                          (implicit protected val ring: Semiring[Double],
                           protected val alphabet: Alphabet[T]) extends Distance[T]  with Composition[T] {
  protected val encoder = Encoder.fromIndex(index);
  protected val epsilonIndex = index(alphabet.epsilon);

  trait Automaton {
    // source -> character -> target -> weight
    def arcsFrom(s: Int):SparseArray[SparseVector]
    def arcsFrom(s:Int, ch: Int):SparseVector
    def numStates:Int
    def initialState: Int;
    def initialWeight: Double;
    def finalWeights: Array[Double];
    def finalWeight(s: Int) = finalWeights(s);
    def allStates = 0 until numStates;

     override def toString = {

       val Eps = alphabet.epsilon;
       val Sig = alphabet.sigma;
       def transform(c: T) = c match {
         case Eps => "&epsilon;"
         case Sig => "&sigma;"
         case x => x;
       }

       val sb = new StringBuilder;
       sb ++= "digraph A {\n";

       for {
         s <- 0 until numStates iterator;
         (idx,targets) <- arcsFrom(s) iterator;
         (sink,weight) <- targets.activeElements
       } {
         sb ++= "    \"" + s + "\"->\"" + sink +"\"";
         sb ++= "[ label=\""+transform(index.get(idx))+"/" + weight +"\"]\n";
       }

       for(s <- allStates) {
         sb ++= "    \"" + s + "\"";
         sb ++= "[ label=\""+ s + " " + finalWeight(s) + "\"]\n";
       }
       sb ++= "}";
       sb.toString;
     }

    lazy val cost = if(false) { //if(isCyclic) {
      val costs = allPairDistances(this);
      var cost = ring.zero;
      for( (to,pathWeight) <- costs(initialState)) {
        cost = ring.plus(cost,ring.times(initialWeight,ring.times(pathWeight,finalWeight(to))));
      }
      cost;
    } else {
      val costs = singleSourceShortestDistances(this);
      var cost = ring.zero;
      for( (w,s) <- costs zipWithIndex) {
        cost = ring.plus(cost,ring.times(w,finalWeight(s)));
      }
      cost;
    }
  }

  def automaton(seq: Seq[T], weight: Double):Automaton = {
    val numStates = seq.length + 1;
    val arcs = Array.fill(numStates)(encoder.fillSparseArray {
      val sp = new SparseVector(numStates);
      sp.default = ring.zero;
      sp
    });
    for((ch,i) <- seq.zipWithIndex) {
      arcs(i).getOrElseUpdate(index(ch))(i+1) = ring.one;
    }
    val finalWeights = Array.fill(numStates)(ring.zero);
    finalWeights(numStates-1) = ring.one;
    automaton(arcs, finalWeights, startWeight = weight);
  }

  def automaton(arcs: Array[SparseArray[SparseVector]], endWeights: Array[Double],
               initialState: Int = 0, startWeight: Double = ring.zero):Automaton = new Automaton {
    def allArcs = arcs;
    def numStates = arcs.length;
    def arcsFrom(s:Int) = allArcs(s);
    def arcsFrom(s:Int, ch: Int) = allArcs(s)(ch);
    def initialState = 0;
    def initialWeight: Double = startWeight;
    def finalWeights = endWeights;
  }


  implicit def asNormalAutomaton(a: Automaton): fst.Automaton[Double,Int,T] = new fst.Automaton[Double,Int,T] {
    import a._;
    val initialStateWeights = Map(a.initialState -> a.initialWeight);
    def finalWeight(s: Int) = a.finalWeight(s);
    def edgesMatching(s: Int, l: T) = {
      if(l == alphabet.sigma) {
        for {
          (idx,targets) <- a.arcsFrom(s)
          (sink,weight) <- targets.activeElements
        } yield new Arc(s,sink,index.get(idx),weight)
      } else {
        for {
          (sink,weight) <- a.arcsFrom(s)(index(l)).activeElements
        } yield new Arc(s,sink,l,weight)
      }
    };

    override lazy val allEdges = {
      for {
        s <- 0 until numStates iterator;
        (idx,targets) <- a.arcsFrom(s) iterator;
        (sink,weight) <- targets.activeElements
      } yield new Arc(s,sink,index.get(idx),weight)
    } toIndexedSeq

    override def allStates:Set[Int] = new Set[Int] {
      def iterator = (0 until numStates).iterator

      def -(elem: Int) = if(!contains(elem)) this else {
        BitSet.empty ++ (0 until numStates) - elem;
      }

      def +(elem: Int) = if(contains(elem)) this else {
        BitSet.empty ++ (0 until numStates) + elem;
      }

      def contains(elem: Int) = {
        0 <= elem && elem < numStates;
      }

      override def size = numStates;

      override def toIndexedSeq[B >: Int] = 0 until numStates;
    }

    override def collapseEdges = (a:fst.Automaton[Double,Int,T]);
    override def relabel = (a:fst.Automaton[Double,Int,T]);
    override def relabelWithIndex = (a:fst.Automaton[Double,Int,T],new DenseIntIndex(numStates));


  }


}