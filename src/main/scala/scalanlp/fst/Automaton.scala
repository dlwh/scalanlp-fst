package scalanlp.fst

import scalanlp.math._;
import scala.collection.Traversable;
import scala.collection.mutable.PriorityQueue;

import Transducer._;
trait Automaton[W,State,T] extends Transducer[W,State,T,T] { outer =>
  import Automaton._;

  override def edgesWithOutput(s: State, l: Option[T]) = edgesWithInput(s,l);
  
  def &[S](that: Automaton[W,S,T]):Automaton[W,(State,S,InboundEpsilon),T] = (this >> that).inputProjection;
  
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
    
    protected override val ring = outer.ring;
  }


  override def relabel = super.relabel(Stream.from(0)).inputProjection;
  override def relabel[U](newStates: Iterable[U]) = super.relabel(newStates).inputProjection;
  override def determinize(implicit wld: WLDSemiring[W]) = super.determinize.inputProjection;
  
  
  override def toString = {
    def escape(s: String) = s.replaceAll("\"","\\\"");
    val sb = new StringBuilder;
    sb ++= "digraph A {\n";
    
    val states = collection.mutable.Set[State]();
    breadthFirstSearch{ case Arc(s,to,label,_,weight) =>
	    sb ++= "    \"" + escape(s.toString) + "\"->\"" + escape(to.toString) +"\"";
		  sb ++= "[ label=\""+label.getOrElse("&epsilon;")+"/" + weight +"\"]\n";
      states += s;
      states += to;
	  }
    
    for(s <- states) {
	    sb ++= "    \"" + escape(s.toString) + "\"";
		  sb ++= "[ label=\""+ escape(s.toString) + " " + finalWeight(s) + "\"]\n";
    }
    sb ++= "}";
    sb.toString;
  }
  
}

object Automaton {
  def constant[T,W](x: Seq[T], w: W)(implicit sring: Semiring[W]) = new Automaton[W,Int,T] {
    val initialStateWeights = Map(0 -> sring.one);
    def finalWeight(s: Int) = if(s == x.length) w else sring.zero;
    protected implicit val ring = sring;
    
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

  def automaton[W:Semiring,S,T](initialStates: Map[S,W], finalWeights: Map[S,W])(arcs: Arc[W,S,T,T]*): Automaton[W,S,T] = {
    val arcMap = arcs.groupBy(_.from);
    new Automaton[W,S,T] {
      val ring = implicitly[Semiring[W]];
      val initialStateWeights = initialStates;
      def finalWeight(s: S) = finalWeights.getOrElse(s,ring.zero);
      def edgesFrom(s: S) = arcMap.getOrElse(s,Seq());
    }
  }

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
  
  {
    val dsl = new DSL[Double];
    import dsl._;
    dsl.automaton(initialStates=Map(1->1.0),finalWeights=Map(3->1.0))(
      1 -> 2 (label='3',weight=10.),
      1 -> 3 (label='4',weight=11.),
      2 -> 3 (label='5',weight=1.0),
      1 -> 2 (label=eps,weight=1.0),
      2 -> 2 (label='3',weight= -1.0)
    );
  }
  
}
