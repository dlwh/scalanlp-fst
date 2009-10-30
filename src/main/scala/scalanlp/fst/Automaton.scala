package scalanlp.fst

import scalanlp.math._;
import scala.collection.Traversable;
import scala.collection.mutable.PriorityQueue;

import Transducer._;

/**
* A weighted automaton is just a transducer where the input label is the same as the output label. 
*/
abstract class Automaton[W,State,T](implicit ring: Semiring[W], alpha: Alphabet[T]) extends Transducer[W,State,T,T] { outer =>
  import Automaton._;

  /**
  * Forwards calls to edgesMatching w/ one arg
  */
  override def edgesMatching(s: State, in: T, out: T) = { 
    if(in == out) edgesMatching(s,in) 
    else if(in == alpha.sigma) edgesMatching(s,out)
    else if(out == alpha.sigma) edgesMatching(s,in)
    else Iterator.empty;
  }

  def edgesMatching(s: State, l: T):Iterator[Arc] 
  
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
    
    def edgesMatching(s: Either[State,S], label: T):Iterator[Arc] = s match {
      case l@Left(os) => 
        for(Arc(_,to,label,_,weight) <- outer.edgesMatching(os,label))
          yield Arc(l,Left(to),label,label,weight);
      case l@Right(os) => 
        for(Arc(_,to,label,_,weight) <- that.edgesMatching(os,label))
          yield Arc(l,Right(to),label,label,weight);
    }
    
    def finalWeight(s: Either[State,S]) = s match {
      case Left(s) => outer.finalWeight(s);
      case Right(s) => that.finalWeight(s);
    }
  }


  override def relabel = super.relabel.inputProjection;
  
  /**
  * Outputs the automaton's graph in DOT format for easy visualization. 
  * It shouldn't be too hard to read this input either...
  */
  override def toString = {
    def escape2(s: String) = s.replaceAll("\"","\\\"");

    val Eps = alpha.epsilon;
    val Sig = alpha.sigma;
    def transform(c: T) = c match {
      case Eps => "&epsilon;"
      case Sig => "&sigma;"
      case x => x;
    }

    val sb = new StringBuilder;
    sb ++= "digraph A {\n";
    
    val states = collection.mutable.Set[State]();
    breadthFirstSearch{ case Arc(s,to,label,_,weight) =>
	    sb ++= "    \"" + escape2(s.toString) + "\"->\"" + escape2(to.toString) +"\"";
		  sb ++= "[ label=\""+transform(label)+"/" + weight +"\"]\n";
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
  def constant[@specialized("Char") T,W](x: Seq[T], w: W)(implicit sring: Semiring[W], alpha: Alphabet[T]): Automaton[W,Int,T] = new Automaton[W,Int,T]()(sring,alpha) {
    val initialStateWeights = Map(0 -> sring.one);
    def finalWeight(s: Int) = if(s == x.length) w else sring.zero;

    final val myEdges:Seq[Arc] = for(s <- Array.range(0,x.length)) yield {
      Arc(s,s+1,(x(s)),(x(s)),sring.one);
    };

    def edgesMatching(s: Int, l: T) = {
      if(s == x.length || (myEdges(s).in != l && l != alpha.sigma)) Iterator.empty else Iterator(myEdges(s));
    }
  }

  /**
  * Factory method for automaton. Creates an automaton with the
  * given initial states, final weights, and arcs.
  */
  def automaton[W:Semiring,S,T:Alphabet](initialStates: Map[S,W], finalWeights: Map[S,W])(arcs: Arc[W,S,T,T]*): Automaton[W,S,T] = {
    val arcMap = arcs.groupBy(_.from);
    new Automaton[W,S,T]()(implicitly[Semiring[W]], implicitly[Alphabet[T]]) {
      val initialStateWeights = initialStates;
      def finalWeight(s: S) = finalWeights.getOrElse(s,ring.zero);
      def edgesMatching(s: S, l: T) = {
        if(l == inAlpha.sigma) {
          arcMap.getOrElse(s,Seq.empty).iterator
        } else {
          arcMap.getOrElse(s,Seq.empty) filter { arc =>
            (l == inAlpha.sigma || l == arc.in)
          } iterator
        }
      };
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
  class DSL[W:Semiring,T:Alphabet] {
    private val epsilonT = implicitly[Alphabet[T]].epsilon;
    class Extras[S](to: S) {
      def apply(label: T, weight: W) = (to,(label),weight);
      def apply(label: eps.type, weight: W) = (to,epsilonT,weight);
    }
    object eps;

    implicit def extras[S](t: S) = new Extras(t);

    def automaton[S](initialStates: Map[S,W], finalWeights: Map[S,W])(arcs: (S,(S,T,W))*): Automaton[W,S,T] = {
      val realArcs = for( (from,(to,label,w)) <- arcs) yield Arc(from,to,label,label,w);
      Automaton.automaton(initialStates,finalWeights)(realArcs:_*);
    }
  }
  
}
