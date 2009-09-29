package scalanlp.fst

import scalanlp.math._;
import scala.collection.Traversable;
import scala.collection.Sequence;
import scala.collection.mutable.PriorityQueue;

trait Transducer[W,State,In,Out] { outer =>
  import Transducer._;

  protected implicit val ring: Semiring[W];
  
  val initialStateWeights: Map[State,W];
  def finalWeight(s: State): W;
  
  def edgesFrom(a: State):Seq[Arc[W,State,In,Out]];
  def edgesWithInput(a: State, trans: Option[In]): Seq[Arc[W,State,In,Out]] =   edgesFrom(a).filter(_.in == trans);
  def edgesWithOutput(a: State, trans: Option[Out]): Seq[Arc[W,State,In,Out]] = edgesFrom(a).filter(_.out == trans);

  def swapLabels: Transducer[W,State,Out,In] = new Transducer[W,State,Out,In] {
    val ring = outer.ring;
    val initialStateWeights = outer.initialStateWeights;
    def finalWeight(s: State) = outer.finalWeight(s);
    def edgesFrom(s: State) = outer.edgesFrom(s).map {
      case Arc(from,to,in,out,w) => Arc(from,to,out,in,w);
    }
    override def edgesWithInput(s: State, trans: Option[Out]) = outer.edgesWithOutput(s,trans).map {
      case Arc(from,to,in,out,w) => Arc(from,to,out,in,w);
    }
    override def edgesWithOutput(s: State, trans: Option[In]) = outer.edgesWithInput(s,trans).map {
      case Arc(from,to,in,out,w) => Arc(from,to,out,in,w);
    }
  }

  def inputProjection:Automaton[W,State,In] = new Automaton[W,State,In] {
    val ring = outer.ring;
    val initialStateWeights = outer.initialStateWeights;
    def finalWeight(s: State) = outer.finalWeight(s);
    def edgesFrom(a: State) = outer.edgesFrom(a).map { 
      case Arc(from,to,in,out,w) => Arc(from,to,in,in,w)
    }
    override def edgesWithInput(a: State, trans: Option[In]) = outer.edgesWithInput(a,trans).map { 
      case Arc(from,to,in,_,w) => Arc(from,to,in,in,w);
    }
  }

  def outputProjection: Automaton[W,State,Out] = new Automaton[W,State,Out] {
    val ring = outer.ring;
    val initialStateWeights = outer.initialStateWeights;
    def finalWeight(s: State) = outer.finalWeight(s);
    def edgesFrom(a: State) = outer.edgesFrom(a).map { 
      case Arc(from,to,in,out,w) => Arc(from,to,out,out,w)
    }
    override def edgesWithInput(a: State, trans: Option[Out]) = outer.edgesWithOutput(a,trans).map { 
      case Arc(from,to,_,out,w) => Arc(from,to,out,out,w);
    }
  }

  /**
  * Epsilon-free composition
  */ 
  def >!>[S,Out2](that: Transducer[W,S,Out,Out2]):Transducer[W,(State,S),In,Out2] = new Transducer[W,(State,S),In,Out2] {
    protected implicit val ring = outer.ring;

    val initialStateWeights = for {
      (k1,w1) <- outer.initialStateWeights;
      (k2,w2) <-  that.initialStateWeights
    } yield ((k1,k2),ring.times(w1,w1))
    
    
    def finalWeight(s: (State,S)) = ring.times(outer.finalWeight(s._1),that.finalWeight(s._2));

    override def edgesWithInput(s: (State,S), trans: Option[In]): Seq[Arc[W,(State,S),In,Out2]] = {
      for(Arc(_,to1,in1,out1,w1) <- outer.edgesWithInput(s._1,trans);
          Arc(_,to2,_,out2,w2) <- that.edgesWithInput(s._2,out1)) yield {
        Arc(s,(to1,to2),in1,out2,ring.times(w1,w2));
      }
    }
    override def edgesWithOutput(s: (State,S), trans: Option[Out2]): Seq[Arc[W,(State,S),In,Out2]] = {
      for(Arc(_,to2,out1,out2,w2) <- that.edgesWithOutput(s._2,trans);
          Arc(_,to1,in1,_,w1) <- outer.edgesWithOutput(s._1,out1)
          ) yield {
        Arc(s,(to1,to2),in1,out2,ring.times(w1,w2));
      }
    }

    def edgesFrom(s: (State,S)) = {
      (for(Arc(_,to1,in1,out1,w1) <- outer.edgesFrom(s._1);
          Arc(_,to2,_,out2,w2) <- that.edgesWithInput(s._2,out1)) yield {
        Arc(s,(to1,to2),in1,out2,ring.times(w1,w2));
      })
    }
  }

  def compose[S,Out2](that: Transducer[W,S,Out,Out2]) = this >> that;


  /**
  * Composition of two transducers in the general case
  */
  def >>[S,Out2](that: Transducer[W,S,Out,Out2]):Transducer[W,(State,S,InboundEpsilon),In,Out2] = new Transducer[W,(State,S,InboundEpsilon),In,Out2] {
    protected implicit val ring = outer.ring;

    val initialStateWeights: Map[(State,S,InboundEpsilon),W] = for {
      (k1,w1) <- outer.initialStateWeights;
      (k2,w2) <-  that.initialStateWeights
    } yield ((k1,k2,NoEps:InboundEpsilon),ring.times(w1,w2))
    
    
    def finalWeight(s: (State,S,InboundEpsilon)) = ring.times(outer.finalWeight(s._1),that.finalWeight(s._2));

    // ugh a lot of code duplication. What to do? XXX
    override def edgesWithInput(s: (State,S,InboundEpsilon), trans: Option[In]) = {
      val arcs = collection.mutable.ArrayBuffer[Arc[W,(State,S,InboundEpsilon),In,Out2]]()
      for(a1 @ Arc(from1,to1,in1,out1,w1) <- outer.edgesWithInput(s._1,trans);
          if out1 != None;
          a2 @ Arc(from2,to2,in2,out2,w2) <- that.edgesWithInput(s._2,out1);
          w = ring.times(w1,w2)) {
        arcs += Arc(s,(to1,to2,NoEps),in1,out2,w);
      }
      
      // Handle epsilon case
      s._3 match {
        case NoEps =>
          for( Arc(_,to1,`trans`,None,w)  <- outer.edgesWithOutput(s._1,None) ) {
            arcs += Arc(s,(to1,s._2,LeftEps),trans,None,w);
            for( Arc(_,to2,None,out2,w)  <- that.edgesWithInput(s._2,None) ) {
              arcs += Arc(s,(to1,to2,NoEps),trans,out2,w);
            }
          }
          if(trans == None)
            for( Arc(_,to,None,out2,w)  <- that.edgesWithInput(s._2,None) ) {
              arcs += Arc(s,(s._1,to,RightEps),None,out2,w);
            }
        case LeftEps =>
          for( Arc(_,to1,`trans`,None,w)  <- outer.edgesWithOutput(s._1,None) ) {
            arcs += Arc(s,(to1,s._2,LeftEps),trans,None,w);
          }
        case RightEps if trans == None =>
          for( Arc(_,to,None,out2,w)  <- that.edgesWithInput(s._2,None) ) {
            arcs += Arc(s,(s._1,to,RightEps),None,out2,w);
          }
        case RightEps =>
      }
      arcs;
    }

    override def edgesWithOutput(s: (State,S,InboundEpsilon), trans: Option[Out2]) = {
      val arcs = collection.mutable.ArrayBuffer[Arc[W,(State,S,InboundEpsilon),In,Out2]]()
      for(a2 @ Arc(from2,to2,out1,out2,w2) <- that.edgesWithOutput(s._2,trans);
          if out1 != None;
          a1 @ Arc(from1,to1,in1,_,w1) <- outer.edgesWithOutput(s._1,out1);
          w = ring.times(w1,w2)) {
        arcs += Arc(s,(to1,to2,NoEps),in1,out2,w);
      }
      
      // Handle epsilon case
      s._3 match {
        case NoEps =>
          for( Arc(_,to1,in1,None,w)  <- outer.edgesWithOutput(s._1,None) ) {
            if(trans == None)
              arcs += Arc(s,(to1,s._2,LeftEps),in1,None,w);
            for( Arc(_,to2,None,_,w)  <- that.edgesWithOutput(s._2,trans) ) {
              arcs += Arc(s,(to1,to2,NoEps),in1,trans,w);
            }
          }
          for( Arc(_,to,None,`trans`,w)  <- that.edgesWithInput(s._2,None) ) {
            arcs += Arc(s,(s._1,to,RightEps),None,`trans`,w);
          }
        case LeftEps if trans == None  =>
          for( Arc(_,to1,in1,None,w)  <- outer.edgesWithOutput(s._1,None) ) {
            arcs += Arc(s,(to1,s._2,LeftEps),in1,None,w);
          }
        case LeftEps =>
        case RightEps =>
          for( Arc(_,to,None,out2,w)  <- that.edgesWithInput(s._2,None) ) {
            arcs += Arc(s,(s._1,to,RightEps),None,out2,w);
          }
      }
      arcs;
    }

    def edgesFrom(s: (State,S,InboundEpsilon)) = {
      val arcs = collection.mutable.ArrayBuffer[Arc[W,(State,S,InboundEpsilon),In,Out2]]()
      for(a1 @ Arc(from1,to1,in1,out1,w1) <- outer.edgesFrom(s._1);
          if out1 != None;
          a2 @ Arc(from2,to2,in2,out2,w2) <- that.edgesWithInput(s._2,out1);
          w = ring.times(w1,w2)) {
        arcs += Arc(s,(to1,to2,NoEps),in1,out2,w);
      }
      
      // Handle epsilon case
      s._3 match {
        case NoEps =>
          for( Arc(_,to1,in1,None,w)  <- outer.edgesWithOutput(s._1,None) ) {
            arcs += Arc(s,(to1,s._2,LeftEps),in1,None,w);
            for( Arc(_,to2,None,out2,w)  <- that.edgesWithInput(s._2,None) ) {
              arcs += Arc(s,(to1,to2,NoEps),in1,out2,w);
            }
          }
          for( Arc(_,to,None,out2,w)  <- that.edgesWithInput(s._2,None) ) {
            arcs += Arc(s,(s._1,to,RightEps),None,out2,w);
          }
        case LeftEps =>
          for( Arc(_,to1,in1,None,w)  <- outer.edgesWithOutput(s._1,None) ) {
            arcs += Arc(s,(to1,s._2,LeftEps),in1,None,w);
          }
        case RightEps =>
          for( Arc(_,to,None,out2,w)  <- that.edgesWithInput(s._2,None) ) {
            arcs += Arc(s,(s._1,to,RightEps),None,out2,w);
          }
      }
      arcs;
    }
  }

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

  lazy val cost = {
    val costs = allPathDistances;
    var cost = ring.zero;
    for( (s,w) <- costs) {
      cost = ring.plus(cost,ring.times(w,finalWeight(s)));
    }
    cost;
  }

  /**
  * Implements Generic-Single-Source-Shortest-Distance described in Mohri (2002)
  */
  def allPathDistances:Map[State,W] = {
    val d = new collection.mutable.HashMap[State,W] { 
      override def default(k:State) = ring.zero;
    };
    val r = new collection.mutable.HashMap[State,W] { 
      override def default(k:State) = ring.zero;
    };
    val S = new collection.mutable.Queue[State]();
    for( (s,w) <- initialStateWeights) {
      d(s) = w;
      r(s) = w;
      S += s;
    }

    while(!S.isEmpty) {
      val  q = S.head;
      S.dequeue();
      val rq = r(q);
      r -= q;
      for( Arc(_,to,_,_,w) <- edgesFrom(q)) {
        val dt = d(to);
        val rqw = ring.times(rq,w);
        val dt_p_rqw = ring.plus(dt,rqw);
        if(!ring.closeTo(dt,dt_p_rqw)) {
          r(to) = ring.plus(r(to),rqw);
          d(to) = dt_p_rqw;
          if(!S.contains(to)) {
            S += to;
          }
        }
      }
    }

    for( (s,w) <- initialStateWeights) {
      d(s) = w;
    }
    Map.empty ++ d;

  }

}

object Transducer {
  final case class Arc[+W,+State,+In, +Out](from: State, to: State, in: Option[In], out: Option[Out], weight: W);

  def transducer[W:Semiring,S,In,Out](initialStates: Map[S,W], finalWeights: Map[S,W])(arcs: Arc[W,S,In,Out]*): Transducer[W,S,In,Out] = {
    val arcMap = arcs.groupBy(_.from);
    new Transducer[W,S,In,Out] {
      val ring = implicitly[Semiring[W]];
      val initialStateWeights = initialStates;
      def finalWeight(s: S) = finalWeights.getOrElse(s,ring.zero);
      def edgesFrom(s: S) = arcMap.getOrElse(s,Seq.empty);
    }
  }

  class DSL[S,W:Semiring] {
    class Extras(to: S) {
      def apply[T,U](in: T, out: U, weight: W) = (to,Some(in),Some(out),weight);
      def apply[T](in: T, out: eps.type, weight: W) = (to,Some(in),None,weight);
      def apply[U](in: eps.type, out: U, weight: W) = (to,None,Some(out),weight);
      def apply(in: eps.type, out: eps.type, weight: W) = (to,None,None,weight);
    }
    object eps;

    implicit def extras(t: S) = new Extras(t);

    def transducer[In,Out](initialStates: Map[S,W], finalWeights: Map[S,W])(arcs: (S,(S,Option[In],Option[Out],W))*): Transducer[W,S,In,Out] = {
      val realArcs = for( (from,(to,in,out,w)) <- arcs) yield Arc(from,to,in,out,w);
      Transducer.transducer(initialStates,finalWeights)(realArcs:_*);
    }
  }
  
  {
    val dsl = new DSL[Int,Double];
    import dsl._;
    dsl.transducer(initialStates=Map(1->1.0),finalWeights=Map(3->1.0))(
      1 -> 2 (in='3',out=eps,weight=10.),
      1 -> 3 (in='4',out=eps,weight=11.),
      2 -> 3 (in='5',out=eps,weight=1.0),
      1 -> 2 (in=eps,out='3',weight=1.0),
      2 -> 2 (in='3',out=eps,weight= -1.0)
    );
  }

    val dsl = new DSL[Int,Boolean];
    import dsl._;
    val a = 
      dsl.transducer(Map(0->true),Map(4->true)) (    
        0 -> 1 (in='a',out='a',weight=true),
        1 -> 2 (in='b',out=eps,weight=true),
        2->3 (in='c',out=eps,weight=true),  
        3->4 (in='d',out='d',weight=true)
      ); 
    val b = 
      dsl.transducer(Map(0->true),Map(3->true)) (    
        0 -> 1 (in='a',out='d',weight=true),
        1 -> 2 (in=eps,out='e',weight=true),
        2->3 (in='d',out='a',weight=true)
      ); 

  sealed class InboundEpsilon;
  case object NoEps extends InboundEpsilon;
  case object LeftEps extends InboundEpsilon;
  case object RightEps extends InboundEpsilon;
}
