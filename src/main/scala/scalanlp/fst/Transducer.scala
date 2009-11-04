package scalanlp.fst

import scalanlp.math._;
import scalanlp.util.Log._;
import scala.collection.Traversable;
import scala.collection.Seq;
import scala.collection.{mutable=>muta}
import scala.collection.mutable.ArrayBuffer;
import scala.collection.mutable.PriorityQueue;
import scala.collection.immutable.IntMap;
import scalanlp.collection.mutable.ArrayMap;

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
*W, an input symbol, and an output symbol. Epsilons are determined
*by an alphabet implicit for each input and output symbol
*
*In practice, we only need initial states, the arcs going from one
*state to another state, and the final states.
*
* @author dlwh
*
*/
abstract class Transducer[W,State,In,Out](implicit protected final val ring: Semiring[W],
                                          protected final val inAlpha: Alphabet[In],
                                          protected final val outAlpha: Alphabet[Out]) { outer =>
  import Transducer._;

  type Arc = scalanlp.fst.Arc[W,State,In,Out];

  /**
  * Maps states to scores W. This is the "probability" that a given sequence starts in this state.
  */
  val initialStateWeights: Map[State,W];
  /**
  * Maps states to scores W. This is the "probability" that a given sequence terminates in this state.
  */
  def finalWeight(s: State): W;

  def edgesMatching(s: State, in: In, out: Out): Iterator[Arc];
  
  /**
  * Returns all Arcs leaving this node to some other node.
  */
  final def edgesFrom(s: State):Iterator[Arc] = edgesMatching(s,inAlpha.sigma,outAlpha.sigma);

  /**
  * Returns all Arcs leaving this node to some other node with this input label.
  */
  final def edgesWithInput(a: State, trans: In): Iterator[Arc] = edgesMatching(a,trans,outAlpha.sigma)

  /**
  * Returns all Arcs leaving this node to some other node with this output label.
  */
  final def edgesWithOutput(a: State, trans: Out): Iterator[Arc] = edgesMatching(a,inAlpha.sigma,trans);

  /**
  * Returns all edges in the FST: will expand all the states.
  */
  def allEdges :Seq[Arc] = {
    val buf = new ArrayBuffer[Arc]();
    breadthFirstSearch( buf += _ );
    buf
  }

  /**
  * Retyrns a map from states to all edges in the FST. Will expand all states.
  */
  def allEdgesByOrigin = allEdges.groupBy(_.from);

  def allStates = Set() ++ initialStateWeights.keysIterator ++ allEdges.iterator.map(_.to);

  /**
  * Returns a map from states to its final weight. may expand all nodes.
  */
  def finalStateWeights = Map.empty ++ allStates.map { s => (s,finalWeight(s)) }


  /**
  * Returns a transducer where each arc's input and output are swapped.
  */
  def swapLabels: Transducer[W,State,Out,In] = new Transducer[W,State,Out,In]()(ring, outAlpha, inAlpha) {
    val initialStateWeights = outer.initialStateWeights;
    def finalWeight(s: State) = outer.finalWeight(s);
    def edgesMatching(s: State, out: Out, in: In) = outer.edgesMatching(s,in,out).map {
      case Arc(from,to,in,out,w) => Arc(from,to,out,in,w);
    }
  }

  /**
  * Returns an automaton where each arc is labeled only with the input.
  */
  def inputProjection:Automaton[W,State,In] = new Automaton[W,State,In]()(ring, inAlpha) {
    val initialStateWeights = outer.initialStateWeights;
    def finalWeight(s: State) = outer.finalWeight(s);
    def edgesMatching(a: State, trans: In) = outer.edgesMatching(a,trans, outer.outAlpha.sigma).map { 
      case Arc(from,to,in,out,w) => Arc(from,to,in,in,w)
    }
  }

  /**
  * Returns an automaton where each arc is labeled only with the output.
  */
  def outputProjection: Automaton[W,State,Out] = new Automaton[W,State,Out]()(ring, outAlpha) {
    val initialStateWeights = outer.initialStateWeights;
    def finalWeight(s: State) = outer.finalWeight(s);
    def edgesMatching(a: State, trans: Out) = outer.edgesMatching(a, outer.inAlpha.sigma, trans).map { 
      case Arc(from,to,in,out,w) => Arc(from,to,out,out,w)
    }
  }

  /**
  * Transforms the weights but otherwise returns the same automata.
  */
  def scaleInitialWeights(f: W) = new Transducer[W,State,In,Out]()(ring, inAlpha, outAlpha) {
    val initialStateWeights = outer.initialStateWeights.map { case(k,v) => (k,ring.times(f,v)) }
    def finalWeight(s: State) = outer.finalWeight(s);
    def edgesMatching(s: State, in:In, out: Out) = outer.edgesMatching(s,in,out);
  }


  /**
  * Transforms the weights but otherwise returns the same automata.
  */
  def reweight[W2:Semiring](f: W=>W2): Transducer[W2,State,In,Out] = new Transducer[W2,State,In,Out]()(implicitly[Semiring[W2]], inAlpha, outAlpha) {
    val initialStateWeights = outer.initialStateWeights.map { case(k,v) => (k,f(v))}
    def finalWeight(s: State) = f(outer.finalWeight(s));
    def edgesMatching(s: State, in: In, out: Out) = outer.edgesMatching(s,in,out) map {
      case Arc(from,to,in,out,w) => Arc(from,to,in,out,f(w));
    }
  }

  /**
  * Epsilon-free composition. If you have epsilons, this will almost certainly generate incorrect results.
  * Basically, we take the cartesian product over states.
  */ 
  def >!>[S,Out2](that: Transducer[W,S,Out,Out2]):Transducer[W,(State,S),In,Out2] = new Transducer[W,(State,S),In,Out2] {
    val initialStateWeights = for {
      (k1,w1) <- outer.initialStateWeights;
      (k2,w2) <-  that.initialStateWeights
    } yield ((k1,k2),ring.times(w1,w1))
    
    
    def finalWeight(s: (State,S)) = ring.times(outer.finalWeight(s._1),that.finalWeight(s._2));

    override def edgesMatching(s: (State,S), in: In, out: Out2): Iterator[Arc] = {
      for(Arc(_,to1,in1,out1,w1) <- outer.edgesMatching(s._1,in,outer.outAlpha.sigma);
          Arc(_,to2,_,out2,w2) <- that.edgesMatching(s._2,out1,out)) yield {
        Arc(s,(to1,to2),in1,out2,ring.times(w1,w2));
      }
    }
  }

  /**
  * Simple composition in the general epsilon-ful case.
  */
  def >>[S,Out2](that: Transducer[W,S,Out,Out2]) = compose[S,Out2,W,W](that,implicitly[Semiring[W]].times(_,_));

  /**
  * Composition of two transducers in the general case.
  * Special handling for epsilons described in Mohri (2002). This supports an extension
  * where we can handle two distinct weight types as long as we have a way of composing them
  * into a composite weight. In normal composition, this is just product.
  */
  def compose[S,Out2,W2,W3](that: Transducer[W2,S,Out,Out2],
                            composeW: (W,W2)=>W3)
                          (implicit sr: Semiring[W3]):Transducer[W3,(State,S,InboundEpsilon),In,Out2] = {
    val InEps = implicitly[Alphabet[In]].epsilon;
    val Out1Eps = implicitly[Alphabet[Out]].epsilon;
    val Out2Eps = that.outAlpha.epsilon;
    new Transducer[W3,(State,S,InboundEpsilon),In,Out2]()(sr,inAlpha, that.outAlpha) {
      val initialStateWeights: Map[(State,S,InboundEpsilon),W3] = for {
        (k1,w1) <- outer.initialStateWeights;
        (k2,w2) <-  that.initialStateWeights
      } yield ((k1,k2,NoEps:InboundEpsilon),composeW(w1,w2));
      
      
      def finalWeight(s: (State,S,InboundEpsilon)) = composeW(outer.finalWeight(s._1),that.finalWeight(s._2));

      override def edgesMatching(s: (State,S,InboundEpsilon), in: In, out: Out2) = {
        val nonEpsArcs = (for {
          a1 @ Arc(from1,to1,in1,out1,w1) <- outer.edgesMatching(s._1,in,outer.outAlpha.sigma);
          if out1 != Out1Eps
        } yield {
          // sum together all weights
          val newArcWeights = that.makeMap[collection.mutable.Map[Out2,W2]]( new muta.HashMap[Out2,W2] {
              override def default(o: Out2) = that.ring.zero
            }
          );
          for(a2 @ Arc(from2,to2,in2,out2,w2) <- that.edgesMatching(s._2,out1,out)) {
            val currentW = newArcWeights(to2)(out2);
            newArcWeights(to2)(out2) = that.ring.plus(w2,currentW);
          }
          val newArcs = {
            for((to2,chMap) <- newArcWeights.iterator;
                (out2,w2) <- chMap.iterator)
              yield {
                Arc(s, (to1,to2,NoEps), in1,out2,composeW(w1,w2));
              }
          }
          newArcs
        }).flatten;

        val epsArcs = {
          val arcs = new ArrayBuffer[Arc];
          s._3 match {
            case NoEps =>
              if(out == Out2Eps || out == that.outAlpha.sigma)
                for( Arc(_,to1,in1,_,w)  <- outer.edgesMatching(s._1,in,Out1Eps) ) {
                  arcs += Arc(s,(to1,s._2,LeftEps),in1,Out2Eps,composeW(w,that.ring.one));
                }
              if(in == InEps || in == outer.inAlpha.sigma)
                for( Arc(_,to,_,out2,w)  <- that.edgesMatching(s._2,Out1Eps,out) ) {
                  arcs += Arc(s,(s._1,to,RightEps),InEps,out2,composeW(outer.ring.one,w));
                }
              if( (in == InEps || in == outer.inAlpha.sigma) && (out == Out2Eps || out == that.outAlpha.sigma)) 
                for(Arc(_,to1,in1,_,w)  <- outer.edgesMatching(s._1,in,Out1Eps);
                    Arc(_,to2,_,out2,w2) <- that.edgesMatching(s._2,Out1Eps,out)) {
                  arcs += Arc(s,(to1,to2,NoEps),in1,out2,composeW(w,w2));
                }
            case LeftEps=>
              if(out == Out2Eps || out == that.outAlpha.sigma)
                for( Arc(_,to1,in1,_,w)  <- outer.edgesMatching(s._1,in,Out1Eps) ) {
                  arcs += Arc(s,(to1,s._2,LeftEps),in1,Out2Eps,composeW(w,that.ring.one));
                }
            case RightEps =>
              if(in == InEps || in == outer.inAlpha.sigma)
                for( Arc(_,to,_,out2,w)  <- that.edgesMatching(s._2,Out1Eps,out) ) {
                  arcs += Arc(s,(s._1,to,RightEps),InEps,out2,composeW(outer.ring.one,w));
                }
          }
          arcs iterator;
        }

        epsArcs ++ nonEpsArcs
      }
    }
  }

  /**
  * Prints out a graph in DOT format. Useful for visualization and inspection.
  */
  override def toString = {
    def escape(s: String) = s.replaceAll("\"","\\\"");
    val inEps = inAlpha.epsilon;
    val inSig = inAlpha.sigma;
    def transformIn(c: In) = c match {
      case `inEps` => "&epsilon;"
      case `inSig` => "&sigma;"
      case x => x;
    }
    val outEps = outAlpha.epsilon;
    val outSig = outAlpha.sigma;
    def transformOut(c: Out) = c match {
      case `outEps` => "&epsilon;"
      case `outSig` => "&sigma;"
      case x => x;
    }
    val sb = new StringBuilder;
    sb ++= "digraph A {\n";
    
    val states = collection.mutable.Set[State]();
    for(Arc(s,to,in,out,weight) <- allEdges) {
	    sb ++= "    \"" + escape(s.toString) + "\"->\"" + escape(to.toString) +"\"";
		  sb ++= "[ label=\""+transformIn(in);
		  sb ++= ":" + transformOut(out);
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

  
  protected final def breadthFirstSearch(func: Arc=>Unit) = {
    val visited = collection.mutable.Set[State]();
    val queue = new collection.mutable.Queue[State]();
    for(s <- initialStateWeights.keysIterator) {
      queue += s;
    }
    
    while(!queue.isEmpty) {
      val s = queue.dequeue(); 
      if(!visited(s)) {
        for(arc@Arc(_,to,_,_,_) <- edgesFrom(s)) {
          func(arc);
          if(!visited(to)){
            queue += to; 
          }
        }
        visited += s;
      }
    }
  }


  /**
   *  Relabels this transducer's states with integers. This is a strict algorithm.
   */
  def relabel:Transducer[W,Int,In,Out] = {
    var _nextIndex = 0;
    def nextIndex = {
      val nn = _nextIndex;
      _nextIndex += 1;
      nn;
    }
    val (arcs,stateToU) = {
      val newStateMap = collection.mutable.Map[State,Int]();
      val seqArcs = new collection.mutable.ArrayBuffer[scalanlp.fst.Arc[W,Int,In,Out]];
      outer.breadthFirstSearch { case Arc(from,to,in,out,score) =>
        val newFrom = newStateMap.getOrElseUpdate(from,nextIndex);
        seqArcs += Arc(newFrom,newStateMap.getOrElseUpdate(to,nextIndex),in,out,score);
      }
      (seqArcs,newStateMap)
    }
    
    println("XXX States: "+ _nextIndex + " Arcs: " + arcs.length);

    val myFinalWeights = IntMap(stateToU.iterator.map { case (s,u) =>
      (u, outer.finalWeight(s));
    }.toSeq:_*);

    val initialStateWeights = IntMap(outer.initialStateWeights.map { case (k,v) =>
      (stateToU(k),v) 
    }.toSeq:_*);

    intTransducer[W,In,Out](initialStateWeights,myFinalWeights)(arcs:_*);
  }
      

  /**
  * Determinizes the transducer lazily.
  */
  def determinize(implicit wld: WLDSemiring[W]): Transducer[W,Map[State,W],In,Out] = new Transducer[W,Map[State,W],In,Out]()(wld,inAlpha,outAlpha) {
    val initialStateWeights = {
      Map(outer.initialStateWeights -> ring.one);
    }

    def edgesMatching(map: Map[State,W], in: In, out: Out) = {
      import collection.mutable._; 
      val labeledWeights = Map[(In,Out),W]();
      val labeledStates = Map[(In,Out), Map[State,W]]();
      import wld._;

      for((s,v) <- map;
          Arc(_,to,in1,out1,w) <- outer.edgesMatching(s,in,out)) {
          val label = (in1,out1);
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
          wld.leftDivide(w,v);
        }
        Arc(map, collection.immutable.Map() ++ newState,label._1,label._2,w);
      } 

      arcs
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

  protected def makeMap[T](dflt: =>T): collection.mutable.Map[State,T] = {
    new collection.mutable.HashMap[State,T] {
      override def default(k: State) = getOrElseUpdate(k,dflt);
    }
  }

  /**
  * Implements Generic-Single-Source-Shortest-Distance described
  * in Mohri (2002), with extra support for doing closure operations
  * on selfloops.
  *
  * Returns State --&gt; distance to that state from the start
  */
  def allPathDistances:Map[State,W] = {
    val d = makeMap[W](ring.zero);
    val r = makeMap[W](ring.zero);
    val extraW = makeMap[W](ring.zero);

    val S = new collection.mutable.Queue[State]();
    for( (s,w) <- initialStateWeights) {
      d(s) = w;
      r(s) = w;
      S += s;
    }

    while(!S.isEmpty) {
      val from = S.head;
      S.dequeue();
      //println("State" + from);
      r -= from;
      extraW.clear();
      var selfLoopMass = ring.zero;

      // find all the self-loop mass, save everything else
      for( a@Arc(_,to,_,_,w) <- edgesFrom(from)) {
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
      globalLog.log(DEBUG)((from,d(from),selfLoopMass));

      r(from) = d(from);
      val rFrom = r(from);
      
      for( (to,w) <- extraW) {
        //println( from + " " + (to,w));
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

  def reverse: Transducer[W,State,In,Out] = {
    val buf = allEdges map { case Arc(from,to,in,out,w) => Arc(to,from,in,out,w) }

    val myInit = Map.empty ++ (
      for(a <- buf;
          s = a.from;
          w = finalWeight(s);
          if w != ring.zero)
        yield (s,w)
    );

    val finalWeights = initialStateWeights;

    Transducer.transducer(myInit,finalWeights)(buf:_*);
  }

  def pushWeights(implicit r2: WLDSemiring[W]):Transducer[W,State,In,Out] = {
    import r2._;
    require(r2.zero == ring.zero);
    require(r2.one == ring.one);

    val rev = reverse;
    globalLog.log(DEBUG)("rev" + rev);
    val costs = reverse.allPathDistances; // \sum_{state q in final} weight(path(p,q))
    globalLog.log(DEBUG)("rev costs" + costs);
    val initWeights = initialStateWeights map { case (k,v) => (k,times(v,costs(k))) }
    val finalWeights = for( (s,w) <- rev.initialStateWeights;
      d = costs(s);
      if d != zero)
      yield (s,leftDivide(d,w));

    // re-reverse and reweight
    val arcs = {
      for(Arc(to,from,in,out,w) <- rev.allEdges;
        d = costs(from);
        if d != zero)
        yield Arc(from,to,in,out,leftDivide(d,times(w,costs(to))));
    }

    Transducer.transducer(initWeights,finalWeights)(arcs:_*);
  }

  def minimize(implicit r2: WLDSemiring[W]): Transducer[W,State,In,Out] = {
    val pushed = this.pushWeights;
    globalLog.log(DEBUG)("pushed"+pushed);
    val edgesByOrigin = pushed.allEdgesByOrigin;

    val equivalentStates = pushed.allStates.toSeq.groupBy{ state =>
      val myEdges = edgesByOrigin.getOrElse(state,Seq.empty).view;
      val edgesWithoutOrigin = Set() ++ myEdges.map { case Arc(_,to,in,out,w) => (to,in,out,w) };
      edgesWithoutOrigin
    } map (_._2);
    val stateMap = (Map.empty ++ (
      for(klass <- equivalentStates;
          val chosenOne = klass.head;
          s <- klass)
        yield (s,chosenOne)
    ))


    val initWeights = new muta.HashMap[State,W] {
      override def default(k: State) = r2.zero;
    }
    for( (s,w) <- pushed.initialStateWeights) {
      initWeights(stateMap(s)) = r2.plus(initWeights(stateMap(s)),w);
    }
      
    val finalWeights = new muta.HashMap[State,W] {
      override def default(k: State) = r2.zero;
    }
    for( (s,w) <- pushed.finalStateWeights) {
      finalWeights(stateMap(s)) = r2.plus(finalWeights(stateMap(s)),w);
    }

    Transducer.transducer(Map.empty ++ initWeights,Map.empty ++ finalWeights)( (Set() ++ pushed.allEdges.map {
        case Arc(from,to,in,out,w) => Arc(stateMap(from),stateMap(to),in,out,w)
      }).toSeq :_*
    );
          
  }
}

object Transducer {
  /**
  * Creates a transducer with the given initial states, final states, and arcs.
  */
  def intTransducer[W:Semiring,In:Alphabet,Out:Alphabet](initialStates: Map[Int,W], finalWeights: Map[Int,W])(arcs: Arc[W,Int,In,Out]*): Transducer[W,Int,In,Out] = {
    val arcMap = {
      val map = new ArrayMap[ArrayBuffer[Arc[W,Int,In,Out]]] {
        override def default(i: Int) = getOrElseUpdate(i,new ArrayBuffer[Arc[W,Int,In,Out]]);
      }
      for(a <- arcs) {
        map(a.from) += a;
      }
      map
    }

    new Transducer[W,Int,In,Out]()(implicitly[Semiring[W]], implicitly[Alphabet[In]], implicitly[Alphabet[Out]]) {
      override def allEdgesByOrigin = arcMap;
      val initialStateWeights = initialStates;
      def finalWeight(s: Int) = finalWeights.getOrElse(s,ring.zero);
      override val finalStateWeights = finalWeights;


      override protected def makeMap[T](dflt: => T): ArrayMap[T] = {
        new ArrayMap[T] {
          override def default(k: Int) = getOrElseUpdate(k,dflt);
        }
      }

      def edgesMatching(s: Int, in: In, out: Out) = {
        if(in == inAlpha.sigma && out == outAlpha.sigma) {
          arcMap.getOrElse(s,Seq.empty).iterator
        } else {
          arcMap.getOrElse(s,Seq.empty).iterator filter { arc =>
            (in == inAlpha.sigma || in == arc.in) && (out == outAlpha.sigma || out == arc.out)
          };
        }
      }
      override def allEdges = arcs;
    }
  }


  /**
  * Creates a transducer with the given initial states, final states, and arcs.
  */
  def transducer[W:Semiring,S,In:Alphabet,Out:Alphabet](initialStates: Map[S,W], finalWeights: Map[S,W])(arcs: Arc[W,S,In,Out]*): Transducer[W,S,In,Out] = {
    val arcMap = arcs.groupBy(_.from);
    new Transducer[W,S,In,Out]()(implicitly[Semiring[W]], implicitly[Alphabet[In]], implicitly[Alphabet[Out]]) {
      override def allEdgesByOrigin = arcMap;
      val initialStateWeights = initialStates;
      def finalWeight(s: S) = finalWeights.getOrElse(s,ring.zero);
      override val finalStateWeights = finalWeights;
      def edgesMatching(s: S, in: In, out: Out) = {
        if(in == inAlpha.sigma && out == outAlpha.sigma) {
          arcMap.getOrElse(s,Seq.empty) iterator
        } else {
          arcMap.getOrElse(s,Seq.empty).iterator filter { arc =>
            (in == inAlpha.sigma || in == arc.in) && (out == outAlpha.sigma || out == arc.out)
          };
        }
      }
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
  class DSL[S,W:Semiring,In:Alphabet,Out:Alphabet] {
    private val inEps = implicitly[Alphabet[In]].epsilon;
    private val outEps = implicitly[Alphabet[Out]].epsilon;
    class Extras(to: S) {
      def apply(in: In, out: Out, weight: W) = (to,in,out,weight);
      def apply(in: In, out: eps.type, weight: W) = (to,in,outEps,weight);
      def apply(in: eps.type, out: Out, weight: W) = (to,inEps,out,weight);
      def apply(in: eps.type, out: eps.type, weight: W) = (to,inEps,outEps,weight);
    }
    object eps

    implicit def extras(t: S) = new Extras(t);

    def transducer(initialStates: Map[S,W], finalWeights: Map[S,W])(arcs: (S,(S,In,Out,W))*): Transducer[W,S,In,Out] = {
      val realArcs = for( (from,(to,in,out,w)) <- arcs) yield Arc(from,to,in,out,w);
      Transducer.transducer(initialStates,finalWeights)(realArcs:_*);
    }
  }
  
  {
    val dsl = new DSL[Int,Double,Char,Char];
    import dsl._;
    dsl.transducer(initialStates=Map(1->1.0),finalWeights=Map(3->1.0))(
      1 -> 2 (in='3',out=eps,weight=10.),
      1 -> 3 (in='4',out=eps,weight=11.),
      2 -> 3 (in='5',out=eps,weight=1.0),
      1 -> 2 (in=eps,out='3',weight=1.0),
      2 -> 2 (in='3',out=eps,weight= -1.0)
    );
  }

  /**
  * These classes represent bookkeeping states for doing composition
  * in the presence of epsilons. They are essential, but you can
  * safely ignore them.
  */
  sealed abstract class InboundEpsilon;
  case object NoEps extends InboundEpsilon;
  case object LeftEps extends InboundEpsilon;
  case object RightEps extends InboundEpsilon;


}
