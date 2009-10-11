package scalanlp.fst

import scalanlp.math._;
import scala.collection.Traversable;
import scala.collection.Seq;
import scala.collection.mutable.ArrayBuffer;
import scala.collection.mutable.PriorityQueue;

/**
* A Transducer is a graph that assigns scores in some semiring over
*weights W to pairs of sequences of Ins and Outs. They are analgous
*to Finite State Machines with weighted arcs that have input and
*output symbols. Another way of thinking about them is that they
*convert an input string to an output string with some score W, which
* is like a probability.
* 
*The representation is a set of states, and a set of arcs from some
*state to another state, where each arc is labeled with a weight
*W, an input symbol, and an output symbol. A symbol may be None,
*in which case no symbol is emitted for that input or output for
*that transition.
*
*In practice, we only need initial states, the arcs going from one
*state to another state, and the final states.
*
* @author dlwh
*
*/
trait Transducer[W,State,In,Out] { outer =>
  import Transducer._;

  protected implicit val ring: Semiring[W];
  
  /**
  * Maps states to scores W. This is the "probability" that a given sequence starts in this state.
  */
  val initialStateWeights: Map[State,W];
  /**
  * Maps states to scores W. This is the "probability" that a given sequence terminates in this state.
  */
  def finalWeight(s: State): W;
  
  /**
  * Returns all Arcs leaving this node to some other node.
  */
  def edgesFrom(a: State):Seq[Arc[W,State,In,Out]];

  /**
  * Returns all Arcs leaving this node to some other node with this input label. A None is an "epsilon".
  */
  def edgesWithInput(a: State, trans: Option[In]): Seq[Arc[W,State,In,Out]] =   edgesFrom(a).filter(_.in == trans);
  /**
  * Returns all Arcs leaving this node to some other node with this output label. A None is an "epsilon".
  */
  def edgesWithOutput(a: State, trans: Option[Out]): Seq[Arc[W,State,In,Out]] = edgesFrom(a).filter(_.out == trans);

  /**
  * Returns all edges in the FST: will expand all the states.
  */
  def allEdges :Seq[Arc[W,State,In,Out]] = {
    val buf = new ArrayBuffer[Arc[W,State,In,Out]]();
    breadthFirstSearch( buf += _ );
    buf
  }

  /**
  * Returns a transducer where each arc's input and output are swapped.
  */
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

  /**
  * Returns an automaton where each arc is labeled only with the input.
  */
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

  /**
  * Returns an automaton where each arc is labeled only with the output.
  */
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
  * Transforms the weights but otherwise returns the same automata.
  */
  def reweight[W2:Semiring](f: W=>W2) = new Transducer[W2,State,In,Out] {
    val ring = implicitly[Semiring[W2]];

    val initialStateWeights = outer.initialStateWeights.map { case(k,v) => (k,f(v))}
    def finalWeight(s: State) = f(outer.finalWeight(s));
    def edgesFrom(s: State) = outer.edgesFrom(s) map {
      case Arc(from,to,in,out,w) => Arc(from,to,in,out,f(w));
    }
    override def edgesWithInput(s: State, in: Option[In]) = outer.edgesWithInput(s,in) map {
      case Arc(from,to,in,out,w) => Arc(from,to,in,out,f(w));
    }

    override def edgesWithOutput(s: State, out: Option[Out]) = outer.edgesWithOutput(s,out) map {
      case Arc(from,to,in,out,w) => Arc(from,to,in,out,f(w));
    }
  }

  /**
  * Epsilon-free composition. If you have epsilons, this will almost certainly generate incorrect results.
  * Basically, we take the cartesian product over states.
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

  /**
  * Lifts this/that into something like an expectation semiring where, if
  * the weights in the left are "probabilties" p and the weights on the right are
  * the things we are trying to compute expectations for w, this creates new machine
  * where the weights are (p,p * v), addition is (p1 + p2, v1 + v2) and multiplication
  * is (p1p2,p1v1 + p2v2). The requirement is that addition for this's W  is multiplication 
  * for that's W. This implies that this's zero is that's one.
  *
  * See Eisner (2001) "Expectation semirings..."
  */
  def expectationCompose[S,Out2](that: Transducer[W,S,Out,Out2]) = {
    val ring = new Semiring[(W,W)] {
      val one = (outer.ring.one,that.ring.zero);
      val zero = (outer.ring.zero,that.ring.zero);
      def times(x: (W,W), y: (W,W)) = {
        import outer.ring.{plus=>pls,times=> tmes};
        (tmes(x._1,y._1),pls(tmes(x._1,y._2),tmes(y._1,x._2)));
      }
      def plus(x: (W,W), y: (W,W)) = {
        import outer.ring.{plus=>pls};
        (pls(x._1,y._1),pls(x._2,y._2))
      }

      def closure(x: (W,W)) = {
        val pc = outer.ring.closure(x._1);
        (pc,outer.ring.times(pc,outer.ring.times(pc,x._2)));
      }
    }

    def mkExp(thisW: W, thatW: W) = (thisW,outer.ring.times(thisW,thatW));

    compose(that,mkExp)(ring);
  }


  /**
  * Simple composition in the general epsilon-ful case.
  */
  def >>[S,Out2](that: Transducer[W,S,Out,Out2]) = compose[S,Out2,W,W](that,ring.times(_,_));

  /**
  * Composition of two transducers in the general case.
  * Special handling for epsilons described in Mohri (2002). This supports an extension
  * where we can handle two distinct weight types as long as we have a way of composing them
  * into a composite weight. In normal composition, this is just product.
  */
  def compose[S,Out2,W2,W3](that: Transducer[W2,S,Out,Out2],
                            composeW: (W,W2)=>W3)
                          (implicit sr: Semiring[W3]):Transducer[W3,(State,S,InboundEpsilon),In,Out2] = {
    new Transducer[W3,(State,S,InboundEpsilon),In,Out2] {
      protected implicit val ring = sr;

      val initialStateWeights: Map[(State,S,InboundEpsilon),W3] = for {
        (k1,w1) <- outer.initialStateWeights;
        (k2,w2) <-  that.initialStateWeights
      } yield ((k1,k2,NoEps:InboundEpsilon),composeW(w1,w2));
      
      
      def finalWeight(s: (State,S,InboundEpsilon)) = composeW(outer.finalWeight(s._1),that.finalWeight(s._2));

      // ugh a lot of code duplication. What to do? XXX
      override def edgesWithInput(s: (State,S,InboundEpsilon), trans: Option[In]) = {
        val arcs = collection.mutable.ArrayBuffer[Arc[W3,(State,S,InboundEpsilon),In,Out2]]()
        for(a1 @ Arc(from1,to1,in1,out1,w1) <- outer.edgesWithInput(s._1,trans);
            if out1 != None;
            a2 @ Arc(from2,to2,in2,out2,w2) <- that.edgesWithInput(s._2,out1);
            w = composeW(w1,w2)) {
          arcs += Arc(s,(to1,to2,NoEps),in1,out2,w);
        }
        
        // Handle epsilon case
        s._3 match {
          case NoEps =>
            for( Arc(_,to1,`trans`,None,w1)  <- outer.edgesWithOutput(s._1,None) ) {
              // normal semiring composeW: w times 1 = w, as expected
              // expectation semiring: composeW(w,that.ring.one) = composeW(w,outer.ring.zero) = (w,0)
              //    --> i.e. we can take an epsilon step and incur a probability cost,
              //        with no change in expectation
              arcs += Arc(s,(to1,s._2,LeftEps),trans,None,composeW(w1,that.ring.one));
              for( Arc(_,to2,None,out2,w2)  <- that.edgesWithInput(s._2,None) ) {
                arcs += Arc(s,(to1,to2,NoEps),trans,out2,composeW(w1,w2));
              }
            }
            if(trans == None)
              for( Arc(_,to,None,out2,w)  <- that.edgesWithInput(s._2,None) ) {
                // normal semiring composeW: 1 times w = w, as expected
                // expectation semiring: composeW(outer.ring.one,w) = composeW(w,outer.ring.zero) = (w,0)
                //    --> i.e. we can take an epsilon step and incur no probability cost,
                //        and change the expectation
                arcs += Arc(s,(s._1,to,RightEps),None,out2,composeW(outer.ring.one,w));
              }
          case LeftEps =>
            for( Arc(_,to1,`trans`,None,w)  <- outer.edgesWithOutput(s._1,None) ) {
              arcs += Arc(s,(to1,s._2,LeftEps),trans,None,composeW(w,that.ring.one));
            }
          case RightEps if trans == None =>
            for( Arc(_,to,None,out2,w)  <- that.edgesWithInput(s._2,None) ) {
              arcs += Arc(s,(s._1,to,RightEps),None,out2,composeW(outer.ring.one,w));
            }
          case RightEps =>
        }
        arcs;
      }

      override def edgesWithOutput(s: (State,S,InboundEpsilon), trans: Option[Out2]) = {
        val arcs = collection.mutable.ArrayBuffer[Arc[W3,(State,S,InboundEpsilon),In,Out2]]()
        for(a2 @ Arc(from2,to2,out1,out2,w2) <- that.edgesWithOutput(s._2,trans);
            if out1 != None;
            a1 @ Arc(from1,to1,in1,_,w1) <- outer.edgesWithOutput(s._1,out1);
            w = composeW(w1,w2)) {
          arcs += Arc(s,(to1,to2,NoEps),in1,out2,w);
        }
        
        // Handle epsilon case
        s._3 match {
          case NoEps =>
            for( Arc(_,to1,in1,None,w1)  <- outer.edgesWithOutput(s._1,None) ) {
              if(trans == None)
                arcs += Arc(s,(to1,s._2,LeftEps),in1,None,composeW(w1,that.ring.one));
              for( Arc(_,to2,None,_,w2)  <- that.edgesWithOutput(s._2,trans) ) {
                arcs += Arc(s,(to1,to2,NoEps),in1,trans,composeW(w1,w2));
              }
            }
            for( Arc(_,to,None,`trans`,w)  <- that.edgesWithInput(s._2,None) ) {
              arcs += Arc(s,(s._1,to,RightEps),None,`trans`,composeW(outer.ring.one,w));
            }
          case LeftEps if trans == None  =>
            for( Arc(_,to1,in1,None,w)  <- outer.edgesWithOutput(s._1,None) ) {
              arcs += Arc(s,(to1,s._2,LeftEps),in1,None,composeW(w,that.ring.one));
            }
          case LeftEps => // nothing
          case RightEps =>
            for( Arc(_,to,None,out2,w)  <- that.edgesWithInput(s._2,None) ) {
              arcs += Arc(s,(s._1,to,RightEps),None,out2,composeW(outer.ring.one,w));
            }
        }
        arcs;
      }

      def edgesFrom(s: (State,S,InboundEpsilon)) = {
        val arcs = collection.mutable.ArrayBuffer[Arc[W3,(State,S,InboundEpsilon),In,Out2]]()
        for(a1 @ Arc(from1,to1,in1,out1,w1) <- outer.edgesFrom(s._1);
            if out1 != None;
            a2 @ Arc(from2,to2,in2,out2,w2) <- that.edgesWithInput(s._2,out1);
            w = composeW(w1,w2)) {
          arcs += Arc(s,(to1,to2,NoEps),in1,out2,w);
        }
        
        // Handle epsilon case
        s._3 match {
          case NoEps =>
            for( Arc(_,to1,in1,None,w1)  <- outer.edgesWithOutput(s._1,None) ) {
              arcs += Arc(s,(to1,s._2,LeftEps),in1,None,composeW(w1,that.ring.one));
              for( Arc(_,to2,None,out2,w2)  <- that.edgesWithInput(s._2,None) ) {
                arcs += Arc(s,(to1,to2,NoEps),in1,out2,composeW(w1,w2));
              }
            }
            for( Arc(_,to,None,out2,w)  <- that.edgesWithInput(s._2,None) ) {
              arcs += Arc(s,(s._1,to,RightEps),None,out2,composeW(outer.ring.one,w));
            }
          case LeftEps =>
            for( Arc(_,to1,in1,None,w)  <- outer.edgesWithOutput(s._1,None) ) {
              arcs += Arc(s,(to1,s._2,LeftEps),in1,None,composeW(w,that.ring.one));
            }
          case RightEps =>
            for( Arc(_,to,None,out2,w)  <- that.edgesWithInput(s._2,None) ) {
              arcs += Arc(s,(s._1,to,RightEps),None,out2,composeW(outer.ring.one,w));
            }
        }
        arcs;
      }
    }
  }

  /**
  * Prints out a graph in DOT format. Useful for visualization and inspection.
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

  override def hashCode = {
    initialStateWeights.hashCode;
  }

  override def equals(that: Any) =  that match {
    case that: Transducer[_,_,_,_] => (this eq that) || (
        this.ring == that.ring
      &&this.initialStateWeights == that.initialStateWeights
      && { 
        val theseEdges = this.allEdges;
        val thoseEdges = that.allEdges;
        (
         Set(theseEdges:_*) == Set(that.allEdges:_*) &&
         {for(Arc(from,to,_,_,_) <- thoseEdges) yield (
           finalWeight(from.asInstanceOf[State]) == that.finalWeight(from)
           && finalWeight(to.asInstanceOf[State]) == that.finalWeight(to)
         ) }.forall ( x => x )
       )
      }
    )
    case _ => false;
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

      arcs.toSeq;
    }

    def finalWeight(map: Map[State,W]) = {
      val weights = for( (state,v) <- map.elements;
          fW = outer.finalWeight(state))
        yield ring.times(v,fW);
      weights.foldLeft(ring.zero)(ring.plus _);
    }
  }

  /**
  * Computes the total value of all paths through the transducer.
  */
  lazy val cost = {
    val costs = allPathDistances;
    var cost = ring.zero;
    for( (s,w) <- costs) {
      cost = ring.plus(cost,ring.times(w,finalWeight(s)));
    }
    cost;
  }

  /**
  * Implements Generic-Single-Source-Shortest-Distance described in Mohri (2002), with extra support for doing closure operations on 
  * selfloops.
  */
  def allPathDistances:Map[State,W] = {
    val d = new collection.mutable.HashMap[State,W] { 
      override def default(k:State) = ring.zero;
    };
    val r = new collection.mutable.HashMap[State,W] { 
      override def default(k:State) = ring.zero;
    };

    val extraW = new collection.mutable.HashMap[State,W] {
      override def default(k:State) = ring.zero;
    }

    val S = new collection.mutable.Queue[State]();
    for( (s,w) <- initialStateWeights) {
      d(s) = w;
      r(s) = w;
      S += s;
    }

    while(!S.isEmpty) {
      val from = S.head;
      S.dequeue();
      val rFrom = r(from);
      r -= from;
      extraW.clear();
      var selfLoopMass = ring.zero;

      // find all the self-loop map, save everything else
      for( Arc(_,to,_,_,w) <- edgesFrom(from)) {
        if(from == to) {
          selfLoopMass = ring.plus(selfLoopMass,w);
        } else {
          extraW(to) = ring.plus(extraW(to),w);
        }
      }
      // give myself all my selfloops
      d(from) = 
        if(selfLoopMass == ring.zero) d(from)
        else ring.times(d(from),ring.closure(selfLoopMass));
      
      for( (to,w) <- extraW) {
        val dt = d(to);
        val wRFrom = ring.times(rFrom,w);
        val dt_p_wRFrom = ring.plus(dt,wRFrom);
        if(!ring.closeTo(dt,dt_p_wRFrom)) {
          r(to) = ring.plus(r(to),wRFrom);
          d(to) = dt_p_wRFrom;
          if(!S.contains(to)) {
            S += to;
          }
        }
      }
    }

    Map.empty ++ d;

  }

}

object Transducer {
  /**
  * An arc represents an edge from a state to another state, with input label in, output label out, and weight W.
  * None as a label means epsilon.
  */
  final case class Arc[+W,+State,+In, +Out](from: State, to: State, in: Option[In], out: Option[Out], weight: W);

  /**
  * Creates a transducer with the given initial states, final states, and arcs.
  */
  def transducer[W:Semiring,S,In,Out](initialStates: Map[S,W], finalWeights: Map[S,W])(arcs: Arc[W,S,In,Out]*): Transducer[W,S,In,Out] = {
    val arcMap = arcs.groupBy(_.from);
    new Transducer[W,S,In,Out] {
      val ring = implicitly[Semiring[W]];
      val initialStateWeights = initialStates;
      def finalWeight(s: S) = finalWeights.getOrElse(s,ring.zero);
      def edgesFrom(s: S) = arcMap.getOrElse(s,Seq.empty);
      override def allEdges = arcs;
    }
  }

  /**
  * This class can be used to create transducers with a dot-like syntax:
  <code>
   {
    val dsl = new DSL[Int,Double];
    import dsl._;
    dsl.transducer(initialStates=Map(1-&gt;1.0),finalWeights=Map(3-&gt;1.0))(
      1 -&gt; 2 (in='3',out=eps,weight=10.),
      1 -&gt; 3 (in='4',out=eps,weight=11.),
      2 -&gt; 3 (in='5',out=eps,weight=1.0),
      1 -&gt; 2 (in=eps,out='3',weight=1.0),
      2 -&gt; 2 (in='3',out=eps,weight= -1.0)
    );
  }
  </code>
  *
  */
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

/*
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
      */

  /**
  * These classes represent bookkeeping states for doing composition
  * in the presence of epsilons. They are essential, but you can
  * safely ignore them.
  */
  sealed class InboundEpsilon;
  case object NoEps extends InboundEpsilon;
  case object LeftEps extends InboundEpsilon;
  case object RightEps extends InboundEpsilon;

  /**
  * LogExpectedWeight
  */
  case class LogExpectedWeight(sign: Boolean, prob: Double, score: Double) {
    def value = if(sign) Math.exp(score-prob) else -Math.exp(score-prob);
  }

  object LogExpectedWeight {
    def apply(p: Double, s: Double) = {
      new LogExpectedWeight(s >= 0, p, if(s>= 0) s else -s);
    }
  }

  /**
  * Composes when the left hand side is in logspace. See Li and Eisner (2009) 
  *First- and Second-Order Expectation Semirings 
  *with Applications to Minimum-Risk Training on Translation, table 1 and 3.
  *
  */
  def logSpaceExpectationCompose[S1,S2,In,Mid,Out](a: Transducer[Double,S1,In,Mid],
                                                   b: Transducer[Double,S2,Mid,Out]) = {
    val ring = new Semiring[LogExpectedWeight] {
      import scalanlp.math.Semiring.LogSpace.logSum;
      import scalanlp.math.Semiring.LogSpace.doubleIsLogSpace.{zero=>logZero,one=>logOne,closure=>logClosure};
      // scores are in log log space
      val one = LogExpectedWeight(logOne,b.ring.zero);
      val zero = LogExpectedWeight(logZero,b.ring.zero);
      def times(x: LogExpectedWeight, y: LogExpectedWeight) = {
        // +- or -+ ==> negative
        // -- or ++ ==> positive
        LogExpectedWeight(x.sign == y.sign,x.prob + y.prob, logSum(x.prob + y.score, y.prob + x.score));
      }
      def plus(mx: LogExpectedWeight, my: LogExpectedWeight) = {
        import Math._;
        val prob = mx.score + my.score;
        val x = if(mx.score > my.score) mx else my;
        val y = if(mx.score > my.score) my else mx;
        // copy the sign of the larger number:
        val sign = x.sign;
        val score = {
          if(x.sign == y.sign)
            x.score + java.lang.Math.log1p(exp(y.score - x.score));
          else 
            x.score + java.lang.Math.log1p(-exp(y.score - x.score));
        }
        LogExpectedWeight(sign,score,prob);
      }
      def closure(x: LogExpectedWeight) = {
        val pc = logClosure(x.prob);
        LogExpectedWeight(x.sign,pc,pc + pc + x.score);
      }
    }
    a.compose(b,LogExpectedWeight(_:Double,_:Double))(ring);
  }

}
