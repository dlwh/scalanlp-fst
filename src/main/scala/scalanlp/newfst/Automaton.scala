package scalanlp.newfst

import scalanlp.math.Semiring
import util.MapMaker
import scalanlp.util.Index
import scalanlp.util.{seqExtras}
import scalanlp.collection.mutable.ArrayMap
import collection.mutable.{ArrayBuffer, HashSet}
import scalala.collection.sparse.DefaultArrayValue

/**
 * 
 * @author dlwh
 */
trait Automaton[W,State,T] extends AutomatonLike[W,State,T,Automaton[W,State,T]] {
  def initialStateWeights: Map[State,W];
  def finalWeight(s: State):W

  def edgesFrom(s: State):Iterator[Arc[W,State,T]]

  def edges:Iterable[Arc[W,State,T]] = new Iterable[Arc[W,State,T]] {
    def iterator = new Iterator[Arc[W,State,T]] {
      val visited = new HashSet[State];
      val queue = new collection.mutable.Queue[Arc[W,State,T]]();
      for(s <- initialStateWeights.keysIterator) {
        visited += s
        queue ++= edgesFrom(s);
      }

      def hasNext = !queue.isEmpty;

      def next() = {
        val arc@Arc(_,s,_,_) = queue.dequeue();
        if(!visited(s)) {
          visited += s
          queue ++= edgesFrom(s);
        }
        arc
      }
    }
  }

  def states:Iterable[State] = new Iterable[State] {
    def iterator = new Iterator[State] {
      val visited = new HashSet[State];
      val queue = new collection.mutable.Queue[State]();
      queue ++= initialStateWeights.keys;

      def hasNext = !queue.isEmpty;

      def next() = {
        val s = queue.dequeue();
        for(Arc(_,to,_,_) <- edgesFrom(s))
          if(!visited(to)) {
            visited += to;
            queue += to;
          }
        s
      }
    }
  }

    /**
   * Outputs the automaton's graph in DOT format for easy visualization.
   * It shouldn't be too hard to read this input either...
   */
  override def toString = {
    def escape2(s: String) = s.replaceAll("\"","\\\"");

    val sb = new StringBuilder;
    sb ++= "digraph A {\n";

    val states = collection.mutable.Set[State]();
    edges.foreach { case Arc(s,to,label,weight) =>
        sb ++= "    \"" + escape2(s.toString) + "\"->\"" + escape2(to.toString) +"\"";
        sb ++= "[ label=\""+(label)+"/" + weight +"\"]\n";
        states += s;
        states += to;
	  }

    for(s <- states) {
	    sb ++= "    \"" + escape2(s.toString) + "\"";
		  sb ++= "[ label=\""+ escape2(s.toString) + " " + finalWeight(s) + "\"]\n";
    }
    sb ++= "}";
    sb.toString;
  }

  override def hashCode = {
    initialStateWeights.hashCode;
  }

  override def equals(that: Any) =  that match {
    case that: Automaton[_,_,_] => (this eq that) || (
        initialStateWeights == that.initialStateWeights
        && {
        val theseEdges = edges.toSet;
        val thoseEdges = that.edges.toSet;
        (
          theseEdges == thoseEdges &&
          {for(Arc(from,to,_,_) <- thoseEdges) yield (
            finalWeight(from.asInstanceOf[State]) == that.finalWeight(from)
              && finalWeight(to.asInstanceOf[State]) == that.finalWeight(to)
            ) }.forall ( x => x )
          )
      }
      )
    case _ => false;
  }
}

trait AutomatonLike[W,State,T,+CC<:AutomatonLike[W,State,T,CC] with Automaton[W,State,T]] { outer : CC =>

  def edgesMatching[MatchType](s: State, m: MatchType)(implicit arcMatcher: ArcMatcher[CC,W,State,T,MatchType]) = {
    arcMatcher.arcsMatching(this,s,m);
  }

  def isCyclic(implicit mapMaker: MapMaker[CC,State,Int]) = Topology.isCyclic(this:CC)

  def determinize[CC2](implicit det: Determinizer[CC,CC2]) = det(this)

  def reverse[CC2](implicit rev: Reverser[CC,CC2]):CC2 = rev(this)

  def pushWeights[CC2](implicit rev: WeightPusher[CC,CC2]):CC2 = rev(this)

  def cost(implicit distance: Distance[CC,W,State], ring: Semiring[W]):W = if(false) { //if(isCyclic) {
    val costs = distance.allPairDistances(this);
    var cost = ring.zero;
    for( (from,initWeight) <- initialStateWeights;
        (to,pathWeight) <- costs(from)) {
      cost = ring.plus(cost,ring.times(initWeight,ring.times(pathWeight,finalWeight(to))));

    }
    cost;
  } else {
    val costs = distance.singleSourceShortestDistances(this);
    var cost = ring.zero;
    for( (s,w) <- costs) {
      cost = ring.plus(cost,ring.times(w,finalWeight(s)));
    }
    cost;
  }

  // TODO: generify this somehow.
  def relabel = {
    val index = Index[State]();

    val arcs = {
      val seqArcs = new ArrayMap(new ArrayBuffer[Arc[W,Int,T]]);
      edges.foreach { case Arc(from,to,label,score) =>
        val newFrom = index.index(from);
        val newTo = index.index(to);
        seqArcs(newFrom) += Arc(newFrom,newTo,label,score);
      }
      seqArcs.innerArray
    }


    val myFinalWeights = index.map(finalWeight _).toIndexedSeq;
    val initialStateWeights = index.map(this.initialStateWeights).toIndexedSeq;

    val auto = new IntAutomaton(arcs, initialStateWeights,myFinalWeights);
    auto
  }

  /**
  * Projects this automaton to make it look like a transducer that maps a string to the
  * same string with the same weight as the acceptance weight of this automaton.
  */
  // TODO: extract this behaior as separate class and add ArcMatchers
  def asTransducer: Transducer[W,State,T,T] = new Transducer[W,State,T,T] {
    def edgesFrom(s: State) = outer.edgesFrom(s).map {
      arc=> arc.copy(label = arc.label -> arc.label)
    }
    val initialStateWeights = outer.initialStateWeights
    def finalWeight(x: State) = outer.finalWeight(x);
  }

  /**
   * Keeps only arcs that match the filter f. Lazy.
   */
  // TODO: decide how to keep density by doing this
  // FilteredAutomaton?
  def filterArcs(f: Arc[W,State,T]=>Boolean): Automaton[W,State,T] = new Automaton[W,State,T] {
    val initialStateWeights = outer.initialStateWeights;
    def finalWeight(s: State) = outer.finalWeight(s);
    def edgesFrom(s: State) = outer.edgesFrom(s) filter f;
    override def edges = outer.edges filter f;
    // Need this because we might lose all states otherwise.
    override def states = outer.states;
  }

    /**
   * Creates a new automaton that collapses edges with the same label ane destination
   */
  def collapseEdges(implicit ring: Semiring[W]):Automaton[W,State,T] = new Automaton[W,State,T] {
    val initialStateWeights = outer.initialStateWeights;
    def finalWeight(s: State) = outer.finalWeight(s);

    def edgesFrom(s: State) = {
      // group edges by their follow, input and output arcs
      // i.e. everything except their weight.
      val matchingEdges = outer.edgesFrom(s).toSeq.groupBy { case Arc(from,to,t,w) =>
          (to,t);
      }

      // collapse all the weights
      val collapsedArcs = for {
        ( (to,t), arcs ) <- matchingEdges.iterator
      } yield {
        val totalWeight = arcs.iterator.map(_.weight).reduceLeft(ring.plus(_,_));
        Arc(s,to,t,totalWeight);
      }

      collapsedArcs;
    }
  }

  /**
   * Computes the weighted intersection of two automata.
   */
  def &[S](that: Automaton[W,S,T])
          (implicit alpha: Alphabet[T],
           ring: Semiring[W]):Automaton[W,(State,S,Composition.InboundEpsilon),T] = {
    import scalanlp.newfst.transducerExtras;
    (transducerExtras(asTransducer) >> that.asTransducer).outputProjection;
  }


  /**
   * Computes the weighted union of two automata. Left(S) is this's state, Right(S) is that's state.
   */
  def |[S](that: Automaton[W,S,T]): Automaton[W,Either[State,S],T] = new Automaton[W,Either[State,S],T] {
    val initialStateWeights = (Map[Either[State,S],W]() ++ outer.initialStateWeights.map { case(k,v) =>
      (Left(k),v);
    } ++ that.initialStateWeights.map { case (k,v) =>
      (Right(k),v)
    }) withDefault {
      case Left(x) => outer.initialStateWeights(x);
      case Right(x) => that.initialStateWeights(x);
    }

    def edgesFrom(s: Either[State,S]) = s match {
      case l@Left(os) =>
        for(Arc(_,to,label,weight) <- outer.edgesFrom(os))
        yield Arc(l,Left(to),label,weight);
      case l@Right(os) =>
        for(Arc(_,to,label,weight) <- that.edgesFrom(os))
        yield Arc(l,Right(to),label,weight);
    }

    def finalWeight(s: Either[State,S]) = s match {
      case Left(s) => outer.finalWeight(s);
      case Right(s) => that.finalWeight(s);
    }
  }

}

object Automaton {

  def constant[W:Semiring,T](seq: Seq[T], weight: W) = new ConstantAutomaton(seq,weight);

  def automaton[W:Semiring,S,T](initialWeights: Map[S,W], finalWeights: Map[S,W])(arcs: Arc[W,S,T]*):Automaton[W,S,T] = {
    val groupedArcs = arcs.groupBy(_.source);
    val zero = implicitly[Semiring[W]].zero;
    new Automaton[W,S,T] {
      def edgesFrom(s: S) = groupedArcs.getOrElse(s,Seq.empty).iterator;

      def finalWeight(s: S) = finalWeights.getOrElse(s,zero);

      val initialStateWeights = initialWeights.withDefaultValue(zero);
    }
  }

  class DSL[W:Semiring,T:Alphabet] {
    private val epsilonT = implicitly[Alphabet[T]].epsilon;
    private val zero = implicitly[Semiring[W]].zero;
    class Extras[S](to: S) {
      def apply(label: T, weight: W) = (to,(label),weight);
      def apply(label: eps.type, weight: W) = (to,epsilonT,weight);
    }
    object eps;

    implicit def extras[S](t: S) = new Extras(t);

    def automaton[S](initialStates: Map[S,W], finalWeights: Map[S,W])(arcs: (S,(S,T,W))*): Automaton[W,S,T] = {
      val realArcs = for( (from,(to,label,w)) <- arcs) yield Arc(from,to,label,w);
      Automaton.automaton(initialStates withDefaultValue zero,finalWeights withDefaultValue zero)(realArcs:_*);
    }
  }

}

/**
 * A DenseAutomaton has densely packed integer states from 0 until states.size
 *
 */
trait DenseAutomaton[W,T] extends Automaton[W,Int,T] with AutomatonLike[W,Int,T,DenseAutomaton[W,T]];

/**
 * An IntAutomaton has densely packed integer states from 0 until states.size
 *
 * @arg seq the arcs from each state. there must be an entry for each state
 * @arg initialWeights initial weights at each state. there must be an entry for each state
 * @arg finalWeights final weights at each state. there must be an entry for each state
 */
class IntAutomaton[W,T](seq: IndexedSeq[Iterable[Arc[W,Int,T]]],
                        initialWeights: IndexedSeq[W],
                        finalWeights: IndexedSeq[W]) extends DenseAutomaton[W,T] with Automaton[W,Int,T] {
  def initialStateWeights = initialWeights.asMap;

  def finalWeight(s: Int):W = finalWeights(s);

  def edgesFrom(s: Int) = seq(s).iterator;

  override def states = 0 until seq.length

}



class ConstantAutomaton[W:Semiring,T](seq: Seq[T], weight: W) extends DenseAutomaton[W,T] with Automaton[W,Int,T] {
  def initialStateWeights = Map(0 -> weight);
  def finalWeight(s: Int) = if (s == seq.length) implicitly[Semiring[W]].one else implicitly[Semiring[W]].zero
  def edgesFrom(s: Int) = {
    if(s >= 0 && s < seq.length) Iterator.single(Arc(s,s+1,seq(s),implicitly[Semiring[W]].one))
    else Iterator.empty
  };

  override def states = 0 to seq.length;
}