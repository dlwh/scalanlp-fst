package scalanlp.fst

import scalanlp.math._;
import scala.collection.Traversable;
import scala.collection.mutable.PriorityQueue;

import Transducer._;

/**
* A weighted automaton is just a transducer where the input label is the same as the output label. 
*/
abstract class Automaton[W,State,T](implicit ring: Semiring[W]) extends Transducer[W,State,T,T] { outer =>
  import Automaton._;

  override def edgesWithOutput(s: State, l: Option[T]) = edgesWithInput(s,l);
  
  /**
  * Computes the weighted intersection of two automata.
  */
  def &[S](that: Automaton[W,S,T]):Automaton[W,(State,S,InboundEpsilon),T] = (this >> that).inputProjection;
  
  /**
  * Computes the weighted union of two automata. Left(S) is this's state, Right(S) is that's state.
  */
  def |[S](that: Automaton[W,S,T]): Automaton[W,Either[State,S],T] = new Automaton[W,Either[State,S],T] {
    val initialStateWeights = Map[Either[State,S],W]() ++ outer.initialStateWeights.map { case(k,v) =>
      (Left(k),v);
    } ++ that.initialStateWeights.map { case (k,v) =>
	    (Right(k),v)
    };
    
    def edgesFrom(s: Either[State,S]) = s match {
      case l@Left(os) => 
        for(Arc(_,to,label,_,weight) <- outer.edgesFrom(os))
          yield Arc(l,Left(to),label,label,weight);
      case l@Right(os) => 
        for(Arc(_,to,label,_,weight) <- that.edgesFrom(os))
          yield Arc(l,Right(to),label,label,weight);
    }
    
    def finalWeight(s: Either[State,S]) = s match {
      case Left(s) => outer.finalWeight(s);
      case Right(s) => that.finalWeight(s);
    }
  }


  override def relabel = super.relabel(Stream.from(0)).inputProjection;
  override def relabel[U](newStates: Iterable[U]) = super.relabel(newStates).inputProjection;
  override def determinize(implicit wld: WLDSemiring[W]) = super.determinize.inputProjection;
  
  /**
  * Outputs the automaton's graph in DOT format for easy visualization. 
  * It shouldn't be too hard to read this input either...
  */
  override def toString = {
    def escape2(s: String) = s.replaceAll("\"","\\\"");
    val sb = new StringBuilder;
    sb ++= "digraph A {\n";
    
    val states = collection.mutable.Set[State]();
    breadthFirstSearch{ case Arc(s,to,label,_,weight) =>
	    sb ++= "    \"" + escape2(s.toString) + "\"->\"" + escape2(to.toString) +"\"";
		  sb ++= "[ label=\""+label.getOrElse("&epsilon;")+"/" + weight +"\"]\n";
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
  
}

object Automaton {
  /**
  * Create an automaton that accepts this word and only this word with the given weight.
  */
  def constant[T,W](x: Seq[T], w: W)(implicit sring: Semiring[W]): Automaton[W,Int,T] = new Automaton[W,Int,T]()(sring) {
    val initialStateWeights = Map(0 -> sring.one);
    def finalWeight(s: Int) = if(s == x.length) w else sring.zero;
    
    def edgesFrom(s: Int) = {
      if(s == x.length) Seq[Arc[W,Int,T,T]]();
      else Seq(Arc(s,s+1,Some(x(s)),Some(x(s)),sring.one));
    }

    override def edgesWithInput(s: Int, l: Option[T]) = {
      if (l == x(s)) 
        Seq(Arc(s,s+1,Some(x(s)),Some(x(s)),sring.one));
      else Seq.empty
    }
  }

  /**
  * Factory method for automaton. Creates an automaton with the
  * given initial states, final weights, and arcs.
  */
  def automaton[W:Semiring,S,T](initialStates: Map[S,W], finalWeights: Map[S,W])(arcs: Arc[W,S,T,T]*): Automaton[W,S,T] = {
    val arcMap = arcs.groupBy(_.from);
    new Automaton[W,S,T]()(implicitly[Semiring[W]]) {
      val initialStateWeights = initialStates;
      def finalWeight(s: S) = finalWeights.getOrElse(s,ring.zero);
      def edgesFrom(s: S) = arcMap.getOrElse(s,Seq());
    }
  }

  /**
  * This can be used to make automata in a pleasing-to-the-eye
  * kind of way. Example:
  *
  * <code>
  {
    val dsl = new DSL[Double]; // Double is the weight type we chose.
    import dsl._;
    dsl.automaton(initialStates=Map(1-&gt;1.0),finalWeights=Map(3-&gt;1.0))(
      1 -&gt; 2 (label='3',weight=10.),
      1 -&gt; 3 (label='4',weight=11.),
      2 -&gt; 3 (label='5',weight=1.0),
      1 -&gt; 2 (label=eps,weight=1.0),
      2 -&gt; 2 (label='3',weight= -1.0)
    );
  }
  * </code>
  */
  class DSL[W:Semiring] {
    class Extras[S](to: S) {
      def apply[T](label: T, weight: W) = (to,Some(label),weight);
      def apply(label: eps.type, weight: W) = (to,None,weight);
    }
    object eps;

    implicit def extras[S](t: S) = new Extras(t);

    def automaton[S,T](initialStates: Map[S,W], finalWeights: Map[S,W])(arcs: (S,(S,Option[T],W))*): Automaton[W,S,T] = {
      val realArcs = for( (from,(to,label,w)) <- arcs) yield Arc(from,to,label,label,w);
      Automaton.automaton(initialStates,finalWeights)(realArcs:_*);
    }
  }
  
}
