package scalanlp.fst

import scalanlp.math._;
import scala.collection.Traversable;
import scala.collection.mutable.PriorityQueue;

trait Transducer[W,State,In,Out] { outer =>
  import Transducer._;
  
  protected implicit val ring: Semiring[W];
  
  val initialStateWeights: Map[State,W];
  def finalWeight(s: State): W;
  
  def edgesFrom(a: State):Seq[Arc[W,State,In,Out]];
  
  /*
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
  
  */
  override def toString = {
    def escape(s: String) = s.replaceAll("\"","\\\"");
    val sb = new StringBuilder;
    sb ++= "digraph A {\n";
    
    val states = collection.mutable.Set[State]();
    breadthFirstSearch{ case Arc(s,to,in,out,weight) =>
	    sb ++= "    \"" + escape(s.toString) + "\"->\"" + escape(to.toString) +"\"";
		  sb ++= "[ label=\""+in.getOrElse("&epsilon;")
		  sb ++= ":" + out.getOrElse("&epsilon;");
      sb ++= "/" + weight +"\"]\n";
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
  
  protected final def breadthFirstSearch(func: Arc[W,State,In,Out]=>Unit) = {
    val visited = collection.mutable.Set[State]();
    val queue = new collection.mutable.Queue[State]();
    for(s <- initialStateWeights.keysIterator) {
      queue += s;
    }
    
    while(!queue.isEmpty) {
      val s = queue.dequeue(); 
      for(arc@Arc(_,to,_,_,_) <- edgesFrom(s)) {
        func(arc);
	      if(!visited(to)){
	       queue += to; 
	      }
      }
      visited += s;
    }
  }


  /**
   *  Relabels this transducer's states with integers. This is a strict algorithm.
   */
  def relabel:Transducer[W,Int,In,Out] = relabel(Stream.from(0));
  
  /**
   *  Relabels this transducer's states according to a new scheme. This is a strict algorithm.
   */
  def relabel[U](newStates: Iterable[U]): Transducer[W,U,In,Out] = {
    val (arcs,stateToU) = {
      val newStateMap = collection.mutable.Map[State,U]();
      val seqArcs = new collection.mutable.ArrayBuffer[Arc[W,U,In,Out]];
      val uIter = newStates.iterator;
        outer.breadthFirstSearch { case Arc(from,to,in,out,score) =>
        val newFrom = newStateMap.getOrElseUpdate(from,uIter.next);
        seqArcs += Arc(newFrom,newStateMap.getOrElseUpdate(to,uIter.next),in,out,score);
      }
      (seqArcs,newStateMap)
    }

    val myFinalWeights = Map() ++ stateToU.iterator.map { case (s,u) =>
      (u, outer.finalWeight(s));
    }

    val initialStateWeights = outer.initialStateWeights map { case (k,v) =>
      (stateToU(k),v) 
    };

    transducer[W,U,In,Out](initialStateWeights,myFinalWeights)(arcs:_*);
  }
      

  /**
  * Determinizes the transducer lazily.
  */
  def determinize(implicit wld: WLDSemiring[W]): Transducer[W,Map[State,W],In,Out] = new Transducer[W,Map[State,W],In,Out] {
    val ring = wld;
    val initialStateWeights = {
      Map(outer.initialStateWeights -> ring.one);
    }

    def edgesFrom(map: Map[State,W]) = {
      import collection.mutable._; 
      val labeledWeights = Map[(Option[In],Option[Out]),W]();
      val labeledStates = Map[(Option[In],Option[Out]), Map[State,W]]();
      import ring._;

      for((s,v) <- map;
          Arc(_,to,in,out,w) <- outer.edgesFrom(s)) {
          val label = (in,out);
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
        Arc(map, collection.immutable.Map() ++ newState,label._1,label._2,w);
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

object Transducer {
  final case class Arc[+W,+State,+In, +Out](from: State, to: State, in: Option[In], out: Option[Out], weight: W);

  def transducer[W:Semiring,S,In,Out](initialStates: Map[S,W], finalWeights: Map[S,W])(arcs: Arc[W,S,In,Out]*): Transducer[W,S,In,Out] = {
    val arcMap = arcs.groupBy(_.from);
    new Transducer[W,S,In,Out] {
      val ring = evidence[Semiring[W]];
      val initialStateWeights = initialStates;
      def finalWeight(s: S) = finalWeights.getOrElse(s,ring.zero);
      def edgesFrom(s: S) = arcMap.getOrElse(s,Seq());
    }
  }

  class DSL[W:Semiring] {
    class Extras[S](to: S) {
      def apply[T,U](in: T, out: U, weight: W) = (to,Some(in),Some(out),weight);
      def apply[T](in: T, out: eps.type, weight: W) = (to,Some(in),None,weight);
      def apply[U](in: eps.type, out: U, weight: W) = (to,None,Some(out),weight);
      def apply(in: eps.type, out: eps.type, weight: W) = (to,None,None,weight);
    }
    object eps;

    implicit def extras[S](t: S) = new Extras(t);

    def transducer[S,In,Out](initialStates: Map[S,W], finalWeights: Map[S,W])(arcs: (S,(S,Option[In],Option[Out],W))*): Transducer[W,S,In,Out] = {
      val realArcs = for( (from,(to,in,out,w)) <- arcs) yield Arc(from,to,in,out,w);
      Transducer.transducer(initialStates,finalWeights)(realArcs:_*);
    }
  }
  
  {
    val dsl = new DSL[Double];
    import dsl._;
    dsl.transducer(initialStates=Map(1->1.0),finalWeights=Map(3->1.0))(
      1 -> 2 (in='3',out=eps,weight=10.),
      1 -> 3 (in='4',out=eps,weight=11.),
      2 -> 3 (in='5',out=eps,weight=1.0),
      1 -> 2 (in=eps,out='3',weight=1.0),
      2 -> 2 (in='3',out=eps,weight= -1.0)
    );
  }
  
}
