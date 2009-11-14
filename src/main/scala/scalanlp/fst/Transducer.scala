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
abstract class Transducer[W,State,In,Out](implicit final val ring: Semiring[W],
                                          final val inAlpha: Alphabet[In],
                                          final val outAlpha: Alphabet[Out]) { outer =>
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
    protected[fst] override def makeMap[T](default: =>T) = outer.makeMap(default);
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
    protected[fst] override def makeMap[T](default: =>T) = outer.makeMap(default);
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
    protected[fst] override def makeMap[T](default: =>T) = outer.makeMap(default);
  }

  /**
  * Transforms the weights but otherwise returns the same automata.
  */
  def scaleInitialWeights(f: W) = new Transducer[W,State,In,Out]()(ring, inAlpha, outAlpha) {
    val initialStateWeights = outer.initialStateWeights.map { case(k,v) => (k,ring.times(f,v)) }
    def finalWeight(s: State) = outer.finalWeight(s);
    def edgesMatching(s: State, in:In, out: Out) = outer.edgesMatching(s,in,out);
    protected[fst] override def makeMap[T](default: =>T) = outer.makeMap(default);
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
    protected[fst] override def makeMap[T](default: =>T) = outer.makeMap(default);
  }

  /**
  * Epsilon-free composition. If you have epsilons, this will almost certainly generate incorrect results.
  * Basically, we take the cartesian product over states.
  */ 
  def >!>[S,Out2](that: Transducer[W,S,Out,Out2]):Transducer[W,(State,S),In,Out2] = new Transducer[W,(State,S),In,Out2]()(ring,inAlpha,that.outAlpha) {
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
  * Creates a new Transducer that collapses
  */
  def collapseEdges:Transducer[W,State,In,Out] = new Transducer[W,State,In,Out]()(ring,inAlpha,outAlpha) {

    override val initialStateWeights = outer.initialStateWeights;
    def finalWeight(s: State) = outer.finalWeight(s);
    protected[fst] override def makeMap[T](default: =>T) = outer.makeMap(default);

    def edgesMatching(s: State, in: In, out: Out) = {
      // group edges by their follow, input and output arcs
      // i.e. everything except their weight.
      val matchingEdges = outer.edgesMatching(s,in,out).toSeq.groupBy { case Arc(from,to,in,out,w) =>
        (to,in,out);
      }

      // collapse all the weights
      val collapsedArcs = for {
        ( (to,in,out), arcs ) <- matchingEdges.iterator
      } yield {
        val totalWeight = arcs.iterator.map(_.weight).reduceLeft(ring.plus(_,_));
        Arc(s,to,in,out,totalWeight);
      }

      collapsedArcs;
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
        val nonEpsArcs = for {
          a1 @ Arc(from1,to1,in1,out1,w1) <- outer.edgesMatching(s._1,in,outer.outAlpha.sigma);
          if out1 != Out1Eps
          a2 @ Arc(from2,to2,in2,out2,w2) <- that.edgesMatching(s._2,out1,out)
        } yield {
          Arc(s, (to1,to2,NoEps), in1,out2,composeW(w1,w2));
        }

        // todo XXX: make this lazy.
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
  * Prints out a graph in DOT format showing only connectivity
  */
  def toConnectivityDot = {
    def escape(s: String) = s.replaceAll("\"","\\\"");
    val sb = new StringBuilder;
    sb ++= "digraph C {\n";
    
    val connected = allEdges.map(a => (a.from,a.to)).toSet
    for((s,to) <- connected) {
	    sb ++= "    \"" + escape(s.toString) + "\"->\"" + escape(to.toString) +"\"\n";
	  }
    
    sb ++= "}";
    sb.toString;
  }

  /**
  * Prints out a graph in DOT format. Useful for visualization and inspection.
  */
  override def toString = {
    def escape(s: String) = s.replaceAll("\"","\\\"");
    val inEps = inAlpha.epsilon;
    val inSig = inAlpha.sigma;
    val inRho = inAlpha.rho;
    def transformIn(c: In) = c match {
      case `inEps` => "&epsilon;"
      case `inSig` => "&sigma;"
      case `inRho` => "&rho;"
      case x => x;
    }
    val outEps = outAlpha.epsilon;
    val outSig = outAlpha.sigma;
    val outRho = outAlpha.rho;
    def transformOut(c: Out) = c match {
      case `outEps` => "&epsilon;"
      case `outSig` => "&sigma;"
      case `outRho` => "&rho;"
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
  * Assume the graph has no loops other than self loops.
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
  * Computes the total value of all paths through the transducer.
  */
  lazy val cyclicCost = {
    val costs = allPairDistances;
    var cost = ring.zero;
    for( (from,initWeight) <- initialStateWeights;
         (to,pathWeight) <- costs(from)) {
      cost = ring.plus(cost,ring.times(initWeight,ring.times(pathWeight,finalWeight(to))));

    }
    cost;
  }

  protected[fst] def makeMap[T](dflt: =>T): collection.mutable.Map[State,T] = {
    new collection.mutable.HashMap[State,T] {
      override def default(k: State) = getOrElseUpdate(k,dflt);
    }
  }

  /**
  * Implements Gen-All-Pairs described in Mohri (2002).
  * Finds all pair-wise distances between all points in O(n^3),
  * where n is the number of states. Works for any complete semiring.
  */
  def allPairDistances:Map[State,Map[State,W]] = {
    val distances = makeMap(makeMap(ring.zero));
    val allStates = makeMap[State](null.asInstanceOf[State]); // XXX
    breadthFirstSearch { case Arc(from,to,_,_,w) =>
      val current = distances(from)(to);
      distances(from)(to) = ring.plus(current,w);
      allStates(from) = from;
      allStates(to) = to;
    }
    
    for {
      k <- allStates.keysIterator
    } {
      // cache some commonly used values
      val dkk = distances(k)(k);
      val dkkStar = ring.closure(dkk);

      for {
        i <- allStates.keysIterator if i != k;
        j <- allStates.keysIterator if j != k
      } {
        val current = distances(i)(j);
        val pathsThroughK = ring.times(distances(i)(k),ring.times(dkkStar,distances(k)(j)));
        distances(i)(j) = ring.plus(current,pathsThroughK);
      }

      for { 
        i <- allStates.keysIterator if i != k
      } {
        distances(k)(i) = ring.times(dkkStar,distances(k)(i));
        distances(i)(k) = ring.times(distances(i)(k),dkkStar);
      }
      distances(k)(k) = dkkStar;
    }

    Map.empty ++ distances.map { case (from,map) => 
      (from,Map.empty ++ map  withDefaultValue ring.zero)
    } withDefaultValue (Map.empty.withDefaultValue(ring.zero)) 

  }

  /**
  * Implements Generic-Single-Source-Shortest-Distance described
  * in Mohri (2002), with extra support for doing closure operations
  * on selfloops. Only works for acyclic graphs, k-closed semirings,
  * or graphs that are acyclic except for selfloops.
  *
  * Returns State --&gt; distance to that state from the start
  */
  def allPathDistances:Map[State,W] = {
    import ring._
    val d = makeMap[W](zero);
    val r = makeMap[W](zero);
    val extraW = makeMap[W](zero);
    val selfLoops = makeMap[W](zero);

    val S = new collection.mutable.Queue[State]();
    val visited = makeMap(0);
    val enqueued = makeMap(false);
    for( (s,w) <- initialStateWeights if w != zero) {
      d(s) = w;
      r(s) = w;
      S += s;
      enqueued(s) = true;
    }

    while(!S.isEmpty) {
      val from = S.head;
      S.dequeue();
      enqueued(from) = false;

      extraW.clear();
      var selfLoopMass = zero;
      if(visited(from) > 2) {
        //globalLog(WARN)("Already visited state " + from + "! Cycle?!");
      }
      visited(from) += 1;

      // find all the self-loop mass, save everything else
      for( a@Arc(_,to,_,_,w) <- edgesFrom(from)) {
        if(from == to) {
          selfLoopMass = plus(selfLoopMass,w);
        } else {
          extraW(to) = plus(extraW(to),w);
        }
      }
      // give myself all my selfloops
      r(from) = times(r(from),closure(selfLoopMass));

      selfLoops(from) = closure(selfLoopMass);

      val rFrom = r(from);
      r -= from;
      
      for( (to,w) <- extraW if w != zero) {
        val dt = d(to);
        val wRFrom = times(rFrom,w);
        val dt_p_wRFrom = plus(dt,wRFrom);
        if( !closeTo(dt,dt_p_wRFrom) ) {
          r(to) = plus(r(to),wRFrom);
          d(to) = dt_p_wRFrom;
          if(!enqueued(to)) {
            S += to;
            enqueued(to) = true
          }
        }
      }
    }

    for(  (s,mass) <- selfLoops if mass != zero) {
      d(s) = times(d(s),mass);
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
    val costs = rev.allPathDistances; // \sum_{state q in final} weight(path(p,q))
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

}

object Transducer {
  /**
  * Creates a transducer with the given initial states, final states, and arcs.
  */
  def intTransducer[W:Semiring,In:Alphabet,Out:Alphabet](initialStates: Map[Int,W], finalWeights: Map[Int,W])(arcs: Arc[W,Int,In,Out]*): Transducer[W,Int,In,Out] = {
    
    val arcMap =  arcs.groupBy(_.from);

    val map = new ArrayMap[Seq[Arc[W,Int,In,Out]]] {
      override def defValue = Seq.empty;
    }
    map ++= arcMap;
    
    new Transducer[W,Int,In,Out]()(implicitly[Semiring[W]], implicitly[Alphabet[In]], implicitly[Alphabet[Out]]) {
      override def allEdgesByOrigin = map;
      val initialStateWeights = initialStates;
      def finalWeight(s: Int) = finalWeights.getOrElse(s,ring.zero);
      override val finalStateWeights = finalWeights withDefaultValue(ring.zero);

      override protected[fst] def makeMap[T](dflt: => T): ArrayMap[T] = {
        new ArrayMap[T] {
          override def default(k: Int) = {
            val result = dflt;
            update(k,dflt);
            result;
          }
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
  def transducer
      [W:Semiring,S,In:Alphabet,Out:Alphabet]
      (initialStates: Map[S,W], finalWeights: Map[S,W])
      (arcs: Arc[W,S,In,Out]*)
      : Transducer[W,S,In,Out] = {
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
