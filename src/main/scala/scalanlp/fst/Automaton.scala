package scalanlp.fst

import scalanlp.math._;
import scala.collection.Traversable;
import scala.collection.mutable.PriorityQueue;

trait Automaton[W,State,T] { outer =>
  import Automaton._;
  
  protected implicit val ring: Semiring[W];
  
  val initialStateWeights: Map[State,W];
  def finalWeight(s: State): W;
  
  def edgesFrom(a: State):Seq[Arc[W,State,T]];
  
  def &[S](that: Automaton[W,S,T]):Automaton[W,(State,S),T] = new Automaton[W,(State,S),T] {
    protected implicit val ring = outer.ring;

    val initialStateWeights = for {
      (k1,w1) <- outer.initialStateWeights;
      (k2,w2) <-  that.initialStateWeights
    } yield ((k1,k2),ring.times(w1,w1))
    
    
    def finalWeight(s: (State,S)) = ring.times(outer.finalWeight(s._1),that.finalWeight(s._2));
    def edgesFrom(s: (State,S)) = {
      for(Arc(_,to1,l1,w1) <- outer.edgesFrom(s._1);
          Arc(_,to2,`l1`,w2) <- that.edgesFrom(s._2)) yield {
        Arc(s,(to1,to2),l1,ring.times(w1,w2));
      }
    }
  }
  
  def |[S](that: Automaton[W,S,T]): Automaton[W,Either[State,S],T] = new Automaton[W,Either[State,S],T] {
    val initialStateWeights = Map[Either[State,S],W]() ++ outer.initialStateWeights.map { case(k,v) =>
      (Left(k),v);
    } ++ that.initialStateWeights.map { case (k,v) =>
	    (Right(k),v)
    };
    
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
    
    protected override val ring = outer.ring;
  }
  
  override def toString = {
    def escape(s: String) = s.replaceAll("\"","\\\"");
    val sb = new StringBuilder;
    sb ++= "digraph A {\n";
    
    val states = collection.mutable.Set[State]();
    breadthFirstSearch{ case Arc(s,to,label,weight) =>
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
  
  protected final def breadthFirstSearch(func: Arc[W,State,T]=>Unit) = {
    val visited = collection.mutable.Set[State]();
    val queue = new collection.mutable.Queue[State]();
    for(s <- initialStateWeights.keysIterator) {
      queue += s;
    }
    
    while(!queue.isEmpty) {
      val s = queue.dequeue(); 
      for(arc@Arc(_,to,_,_) <- edgesFrom(s)) {
        func(arc);
	      if(!visited(to)){
	       queue += to; 
	      }
      }
      visited += s;
    }
  }


  /**
   *  Relabels this automaton with integers. This is a strict algorithm.
   */
  def relabel:Automaton[W,Int,T] = relabel(Stream.from(0));
  
  /**
   *  Relabels this automaton according to a new scheme. This is a strict algorithm.
   */
  def relabel[U](newStates: Iterable[U]): Automaton[W,U,T] = {
    val (arcs,stateToU) = {
      val newLabels = collection.mutable.Map[State,U]();
      val seqArcs = new collection.mutable.ArrayBuffer[Arc[W,U,T]];
      val uIter = newStates.iterator;
        outer.breadthFirstSearch { case Arc(from,to,label,score) =>
        val newFrom = newLabels.getOrElseUpdate(from,uIter.next);
        seqArcs += Arc(newFrom,newLabels.getOrElseUpdate(to,uIter.next),label,score);
      }
      (seqArcs,newLabels)
    }

    val myFinalWeights = Map() ++ stateToU.iterator.map { case (s,u) =>
      (u, outer.finalWeight(s));
    }

    val initialStateWeights = outer.initialStateWeights map { case (k,v) =>
      (stateToU(k),v) 
    };

    automaton[W,U,T](initialStateWeights,myFinalWeights)(arcs:_*);
  }
      

  /**
  * Determinizes the automaton lazily.
  */
  def determinize(implicit wld: WLDSemiring[W]): Automaton[W,Map[State,W],T] = new Automaton[W,Map[State,W],T] {
    val ring = wld;
    val initialStateWeights = {
      Map( outer.initialStateWeights -> ring.one);
    }

    def edgesFrom(map: Map[State,W]) = {
      import collection.mutable._; 
      val labeledWeights = Map[Option[T],W]();
      val labeledStates = Map[Option[T],Map[State,W]]();
      import ring._;

      for((s,v) <- map;
          Arc(_,to,label,w) <- outer.edgesFrom(s)) {
        // sum over all the different ways to follow arc with label label to 
        // state t
        val newTo = labeledStates.getOrElseUpdate(label,Map[State,W]())
        val cur = newTo.getOrElse(to,zero);
        newTo(to) = plus(cur,times(w,v));

        val wcur = labeledWeights.getOrElse(label,zero);
        labeledWeights(label) = plus(wcur,times(w,v));
      }
      
      // normalize by w
      val arcs = for((label,newState) <- labeledStates.elements;
          w = labeledWeights(label)) yield {
        newState.transform { (innerState,v) =>
          leftDivide(w,v);
        }
        Arc(map, collection.immutable.Map() ++ newState,label,w);
      } 

      arcs.toSequence;
    }

    def finalWeight(map: Map[State,W]) = {
      val weights = for( (state,v) <- map.elements;
          fW = outer.finalWeight(state))
        yield ring.times(v,fW);
      weights.foldLeft(ring.zero)(ring.plus _);
    }
  }

}

object Automaton {
  final case class Arc[+W,+State,+T](from: State, to: State, label: Option[T], weight: W);
  
  def constant[T,W](x: Seq[T], w: W)(implicit sring: Semiring[W]) = new Automaton[W,Int,T] {
    val initialStateWeights = Map(0 -> sring.one);
    def finalWeight(s: Int) = if(s == x.length) w else sring.zero;
    protected implicit val ring = sring;
    
    def edgesFrom(s: Int) = {
      if(s == x.length) Array[Arc[W,Int,T]]();
      else Array(Arc(s,s+1,Some(x(s)),sring.one));
    }
  }

  def automaton[W:Semiring,S,T](initialStates: Map[S,W], finalWeights: Map[S,W])(arcs: Arc[W,S,T]*): Automaton[W,S,T] = {
    val arcMap = arcs.groupBy(_.from);
    new Automaton[W,S,T] {
      val ring = evidence[Semiring[W]];
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
      val realArcs = for( (from,(to,label,w)) <- arcs) yield Arc(from,to,label,w);
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
