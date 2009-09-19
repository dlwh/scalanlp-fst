package scalanlp.fst

import scalanlp.math._;
import scala.collection.Traversable;
import scala.collection.mutable.PriorityQueue;

trait Automaton[W,State,T] { outer =>
  import Automaton._;
  
  protected implicit val ring: Semiring[W];
  
  val initialStateWeights: Map[State,W];
  def finalWeight(s: State): W;
  
  case class Arc(from: State, to: State, label: Option[T], weight: W);
  def edgesFrom(a: State):Seq[Arc];
  
  def &[S](that: Automaton[W,S,T]):Automaton[W,(State,S),T] = new Automaton[W,(State,S),T] {
    protected implicit val ring = outer.ring;

    val initialStateWeights = for {
      (k1,w1) <- outer.initialStateWeights;
      (k2,w2) <-  that.initialStateWeights
    } yield ((k1,k2),ring.times(w1,w1))
    
    
    def finalWeight(s: (State,S)) = ring.times(outer.finalWeight(s._1),that.finalWeight(s._2));
    def edgesFrom(s: (State,S)) = {
      for(outer.Arc(_,to1,l1,w1) <- outer.edgesFrom(s._1);
          that.Arc(_,to2,`l1`,w2) <- that.edgesFrom(s._2)) yield {
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
        for(outer.Arc(_,to,label,weight) <- outer.edgesFrom(os))
          yield Arc(l,Left(to),label,weight);
      case l@Right(os) => 
        for(that.Arc(_,to,label,weight) <- that.edgesFrom(os))
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
  
  protected final def breadthFirstSearch(func: Arc=>Unit) = {
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
   *  Relabels this autamaton according to this new scheme. This is a strict algorithm.
   */
  def relabel[U](newStates: Iterable[U]): Automaton[W,U,T] = {
    new Automaton[W,U,T] {
      val (arcs,stateToU) =  {
        val newLabels = collection.mutable.Map[State,U]();
        val seqArcs = new collection.mutable.ArrayBuffer[Arc];
        val uIter = newStates.iterator;
        outer.breadthFirstSearch { case outer.Arc(from,to,label,score) =>
          val newFrom = newLabels.getOrElseUpdate(from,uIter.next);
          seqArcs += Arc(newFrom,newLabels.getOrElseUpdate(to,uIter.next),label,score);
        }
        (seqArcs.groupBy(_.from),newLabels)
      }

      val myFinalWeights = stateToU map { case (s,u) =>
        (u, outer.finalWeight(s));
      }
      val initialStateWeights = outer.initialStateWeights map { case (k,v) =>
        (stateToU(k),v) 
      };
      
      def edgesFrom(s: U) = arcs.getOrElse(s,Seq[this.Arc]());
      def finalWeight(s: U) = myFinalWeights.getOrElse(s,ring.zero);
      implicit val ring = outer.ring
    }
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
          outer.Arc(_,to,label,w) <- outer.edgesFrom(s)) {
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
        new Arc(map, collection.immutable.Map() ++ newState,label,w);
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
  
  def constant[T,W](x: Seq[T], w: W)(implicit sring: Semiring[W]) = new Automaton[W,Int,T] {
    val initialStateWeights = Map(0 -> sring.one);
    def finalWeight(s: Int) = if(s == x.length) w else sring.zero;
    protected implicit val ring = sring;
    
    def edgesFrom(s: Int) = {
      if(s == x.length) Array[Arc]();
      else Array(Arc(s,s+1,Some(x(s)),sring.one));
    }
  }
  
}
