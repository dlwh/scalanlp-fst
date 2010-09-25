package scalanlp.fst

/*
 Copyright 2010 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import scala.collection.immutable.IntMap
import scala.collection.mutable.ArrayBuffer
import scalanlp.collection.mutable.ArrayMap
import scalanlp.math._
import scalanlp.util.Index;

import Transducer._;

/**
 * A weighted automaton is just a transducer where the input label is the same as the output label.
 */
abstract class Automaton[@specialized(Double) W:Semiring,State,@specialized(Char) T:Alphabet] { outer =>
  import Automaton._;
  type Arc = scalanlp.fst.Arc[W,State,T];
  protected final def ring = implicitly[Semiring[W]];
  protected final def alphabet = implicitly[Alphabet[T]];

  /**
   * Maps states to scores W. This is the "probability" that a given sequence starts in this state.
   */
  val initialStateWeights: Map[State,W];
  /**
   * Maps states to scores W. This is the "probability" that a given sequence terminates in this state.
   */
  def finalWeight(s: State): W;

  /**
   * Returns all edges from state s with output label l
   */
  def edgesMatching(s: State, l: T):Iterator[Arc] 

  /**
   * Returns all Arcs leaving this node to some other node.
   */
  final def edgesFrom(s: State):Iterator[Arc] = edgesMatching(s,alphabet.sigma);

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
  def allEdgesByOrigin:scala.collection.Map[State,Seq[Arc]] = {
    val m = allEdges.groupBy(_.from)
    val statesWithoutEdges = for( (s,_) <- finalStateWeights if !m.contains(s)) yield (s,Seq.empty);
    m ++ statesWithoutEdges;
  }

  def allStates = Set() ++ initialStateWeights.keysIterator ++ allEdges.iterator.map(_.to);

  /**
   * Returns a map from states to its final weight. may expand all nodes.
   */
  def finalStateWeights = Map.empty ++ allStates.map { s => (s,finalWeight(s)) }

  /**
   * Computes the total value of all paths through the transducer.
   * Automatically selects the algorithm based on cyclicity
   */
  lazy val cost = if(false) { //if(isCyclic) {
    val costs = Distance.allPairDistances(this);
    var cost = ring.zero;
    for( (from,initWeight) <- initialStateWeights;
        (to,pathWeight) <- costs(from)) {
      cost = ring.plus(cost,ring.times(initWeight,ring.times(pathWeight,finalWeight(to))));

    }
    cost;
  } else {
    val costs = Distance.singleSourceShortestDistances(this);
    var cost = ring.zero;
    for( (s,w) <- costs) {
      cost = ring.plus(cost,ring.times(w,finalWeight(s)));
    }
    cost;
  }

  protected[fst] def makeMap[T](dflt: =>T): collection.mutable.Map[State,T] = {
    new collection.mutable.HashMap[State,T] {
      override def default(k: State) = getOrElseUpdate(k,dflt);
    }
  }

  protected[fst] final def breadthFirstSearch(func: Arc=>Unit) = {
    val visited = makeMap(false);
    val queue = new collection.mutable.Queue[State]();
    for(s <- initialStateWeights.keysIterator) {
      queue += s;
    }

    while(!queue.isEmpty) {
      val s = queue.dequeue();
      if(!visited(s)) {
        for(arc@Arc(_,to,_,_) <- edgesFrom(s)) {
          func(arc);
          if(!visited(to)){
            queue += to;
          }
        }
        visited(s) = true;
      }
    }
  }

  //import Transducer._;
  /**
   * Computes the weighted intersection of two automata.
   */
  def &[S](that: Automaton[W,S,T]):Automaton[W,(State,S,scalanlp.fst.Composition.InboundEpsilon),T] = {
    (asTransducer >> that.asTransducer).outputProjection;
  }

  def asTransducer: Transducer[W,State,T,T] = new Transducer[W,State,T,T] {
    def edgesMatching(s: State, t: (T,T)) = if (t._1 == outer.alphabet.sigma || t._1 == t._2) {
      outer.edgesMatching(s,t._2) map { case Arc(s,to,l1,w) => Arc(s,to,(l1,l1),w) }
    } else if(t._2 == outer.alphabet.sigma) {
      outer.edgesMatching(s,t._1) map { case Arc(s,to,l1,w) => Arc(s,to,(l1,l1),w) }
    } else {
      Iterator.empty;
    }
    val initialStateWeights = outer.initialStateWeights
    def finalWeight(x: State) = outer.finalWeight(x);
  }
  
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
        for(Arc(_,to,label,weight) <- outer.edgesMatching(os,label))
          yield Arc(l,Left(to),label,weight);
      case l@Right(os) => 
        for(Arc(_,to,label,weight) <- that.edgesMatching(os,label))
          yield Arc(l,Right(to),label,weight);
    }
    
    def finalWeight(s: Either[State,S]) = s match {
      case Left(s) => outer.finalWeight(s);
      case Right(s) => that.finalWeight(s);
    }
  }

  /**
   * Transforms the weights but otherwise returns the same automata.
   */
  def scaleInitialWeights(f: W) = new Automaton[W,State,T] {
    val initialStateWeights = outer.initialStateWeights.map { case(k,v) => (k,ring.times(f,v)) }
    def finalWeight(s: State) = outer.finalWeight(s);
    def edgesMatching(s: State, x: T) = outer.edgesMatching(s,x);
    protected[fst] override def makeMap[T](default: =>T) = outer.makeMap(default);
  }

  /**
   * Transforms the weights but otherwise returns the same automata.
   */
  def reweight[W2:Semiring](f: Arc=>W2, initReweight: W=>W2): Automaton[W2,State,T] = new Automaton[W2,State,T] {
    val initialStateWeights = outer.initialStateWeights.map { case(k,v) => (k,initReweight(v))}
    def finalWeight(s: State) = initReweight(outer.finalWeight(s));
    def edgesMatching(s: State, label: T) = outer.edgesMatching(s,label) map {
      case a@Arc(from,to,label,w) => Arc(from,to,label,f(a));
    }
    protected[fst] override def makeMap[T](default: =>T) = outer.makeMap(default);
  }

  /**
   * Keeps only arcs that match the filter f. Lazy.
   */
  def filterArcs(f: Arc=>Boolean): Automaton[W,State,T] = new Automaton[W,State,T] {
    val initialStateWeights = outer.initialStateWeights;
    def finalWeight(s: State) = outer.finalWeight(s);
    def edgesMatching(s: State, label: T) = outer.edgesMatching(s,label) filter f;
    override def allEdges = outer.allEdges filter f;
    // Need this because we might lose all states otherwise.
    override def allStates = outer.allStates;
    protected[fst] override def makeMap[T](default: =>T) = outer.makeMap(default);
  }


  /**
   * Creates a new automaton that collapses edges with the same label ane destination
   */
  def collapseEdges:Automaton[W,State,T] = new Automaton[W,State,T] {

    val initialStateWeights = outer.initialStateWeights;
    def finalWeight(s: State) = outer.finalWeight(s);
    protected[fst] override def makeMap[T](default: =>T) = outer.makeMap(default);

    def edgesMatching(s: State, t: T) = {
      // group edges by their follow, input and output arcs
      // i.e. everything except their weight.
      val matchingEdges = outer.edgesMatching(s,t).toSeq.groupBy { case Arc(from,to,t,w) =>
          (to,t);
      }

      // collapse all the weights
      val collapsedArcs = for {
        ( (to,t), arcs ) <- matchingEdges.iterator
      } yield {
        val totalWeight = arcs.iterator.map(_.weight).reduceLeft(ring.plus(_,_));
        Arc(s,to,t,totalWeight);
      }

      collapsedArcs;
    }
  }

  def reverse = {
    val buf = allEdges map { case Arc(from,to,label,w) => Arc(to,from,label,w) }

    val myInit = Map.empty ++ (
      for(a <- buf;
          s = a.from;
          w = finalWeight(s);
          if w != ring.zero)
            yield (s,w)
    );

    val finalWeights = initialStateWeights;

    automaton(myInit,finalWeights)(buf:_*);
  }

  def pushWeights(implicit r2: WLDSemiring[W]): Automaton[W,State,T] = {
    import r2._;
    require(r2.zero == ring.zero);
    require(r2.one == ring.one);

    val rev: Automaton[W,State,T] = reverse;
    val costs = Distance.allPathDistances(rev); // \sum_{state q in final} weight(path(p,q))
    val initWeights = initialStateWeights map { case (k,v) => (k,times(v,costs(k))) }
    val finalWeights = for( (s,w) <- rev.initialStateWeights;
                           d = costs(s);
                           if d != zero)
                             yield (s,leftDivide(d,w));

    // re-reverse and reweight
    val arcs = {
      for(Arc(to,from,label,w) <- rev.allEdges;
          d = costs(from);
          if d != zero)
            yield Arc(from,to,label,leftDivide(d,times(w,costs(to))));
    }

    automaton(initWeights,finalWeights)(arcs:_*);
  }

    /**
   *  Relabels this transducer's states with integers. This is a strict algorithm.
   */
  def relabel:Automaton[W,Int,T] = {
    relabelWithIndex._1
  } 


  def relabelWithIndex: (Automaton[W,Int,T], Index[State]) = {
    val index = Index[State]();
    
    val arcs = {
      val seqArcs = new collection.mutable.ArrayBuffer[scalanlp.fst.Arc[W,Int,T]];
      outer.breadthFirstSearch { case Arc(from,to,label,score) =>
        val newFrom = index.index(from);
        val newTo = index.index(to);
        seqArcs += Arc(newFrom,newTo,label,score);
      }
      seqArcs
    }

    val myFinalWeights = IntMap(index.pairs.map { case (s,u) =>
      (u, outer.finalWeight(s));
    }.toSeq:_*);

    val initialStateWeights = IntMap(outer.initialStateWeights.collect { case (k,v) if v != ring.zero =>
      (index(k),v)
    }.toSeq:_*);

    val auto = intAutomaton(initialStateWeights,myFinalWeights)(arcs:_*);
    (auto,index);
  }

  /**
   * Determinizes the automaton lazily.
   */
  def determinize(implicit wld: WLDSemiring[W]): Automaton[W,Map[State,W],T] = new Automaton[W,Map[State,W],T] {
    val initialStateWeights = {
      Map(outer.initialStateWeights -> ring.one);
    }

    def edgesMatching(map: Map[State,W], label: T) = {
      import collection.mutable._; 
      val labeledWeights = Map[T,W]();
      val labeledStates = Map[T, Map[State,W]]();
      import wld._;

      for((s,v) <- map;
          Arc(_,to,label,w) <- outer.edgesMatching(s,label)) {
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
        Arc(map, collection.immutable.Map() ++ newState,label,w);
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

  override def hashCode = {
    initialStateWeights.hashCode;
  }

  override def equals(that: Any) =  that match {
    case that: Automaton[_,_,_] => (this eq that) || (
        this.ring == that.ring
        &&this.initialStateWeights == that.initialStateWeights
        && {
          val theseEdges = this.allEdges;
          val thoseEdges = that.allEdges;
          (
            Set(theseEdges:_*) == Set(thoseEdges:_*) &&
            {for(Arc(from,to,_,_) <- thoseEdges) yield (
                finalWeight(from.asInstanceOf[State]) == that.finalWeight(from)
                && finalWeight(to.asInstanceOf[State]) == that.finalWeight(to)
              ) }.forall ( x => x )
          )
        }
      )
    case _ => false;
  }



  /**
   * Outputs the automaton's graph in DOT format for easy visualization.
   * It shouldn't be too hard to read this input either...
   */
  override def toString = {
    def escape2(s: String) = s.replaceAll("\"","\\\"");

    val Eps = alphabet.epsilon;
    val Sig = alphabet.sigma;
    val Rho = alphabet.rho;
    def transform(c: T) = c match {
      case Eps => "&epsilon;"
      case Sig => "&sigma;"
      case Rho => "&rho;"
      case x => x;
    }

    val sb = new StringBuilder;
    sb ++= "digraph A {\n";
    
    val states = collection.mutable.Set[State]();
    allEdges.foreach { case Arc(s,to,label,weight) =>
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
   * True iff the graph contains a non self-loop cycle
   */
  lazy val isCyclic = {
    val WHITE = 0
    val GREY = 1
    val BLACK = 2
    val visited = makeMap(WHITE);
    var cyclic = false;
    import scala.util.control.Breaks._;


    def visit(p: State) {
      assert(visited(p) != GREY);
      visited(p) = GREY;
      for ( Arc(_,q,_,_) <- edgesFrom(p) if q != p ) {
        if(visited(q) == GREY) {
          cyclic = true;
          break;
        } else if(visited(q) == WHITE) {
          visit(q);
        }
      }
      visited(p) = BLACK;
    }
          

    breakable {
      for {
        p <- initialStateWeights.keysIterator;
        if visited(p) == WHITE
      } {
        visit(p);
      }
    }

    cyclic;
  }
  
}

object Automaton {


  /**
   * Create an automaton that accepts this word and only this word with the given weight.
   */
  def constant[@specialized(Char) T:Alphabet,W:Semiring](x: Seq[T], w: W): Automaton[W,Int,T] = new Automaton[W,Int,T] {
    val initialStateWeights = Map(0 -> implicitly[Semiring[W]].one);
    def finalWeight(s: Int) = if(s == x.length) w else implicitly[Semiring[W]].zero;

    final val myEdges:Seq[Arc] = for(s <- Array.range(0,x.length)) yield {
      Arc(s,s+1,x(s),implicitly[Semiring[W]].one);
    };

    def edgesMatching(s: Int, l: T) = {
      if(s < x.length && s >= 0 && implicitly[Alphabet[T]].matches(myEdges(s).label,l)) Iterator(myEdges(s)) else Iterator.empty
    }
  }

  /**
   * Factory method for automaton. Creates an automaton with the
   * given initial states, final weights, and arcs.
   */
  def automaton[W:Semiring,S,T:Alphabet](initialStates: Map[S,W], finalWeights: Map[S,W])(arcs: Arc[W,S,T]*): Automaton[W,S,T] = {
    val arcMap = arcs.groupBy(_.from);
    new Automaton[W,S,T]()(implicitly[Semiring[W]], implicitly[Alphabet[T]]) {
      val initialStateWeights = initialStates;
      def finalWeight(s: S) = finalWeights.getOrElse(s,ring.zero);
      def edgesMatching(s: S, l: T) = {
        if(l == alphabet.sigma) {
          arcMap.getOrElse(s,Seq.empty).iterator
        } else {
          arcMap.getOrElse(s,Seq.empty) filter { arc =>
            arc.label == l || alphabet.matches(arc.label,l);
          } iterator
        }
      };
    }
  }

/**
   * Creates a transducer with the given initial states, final states, and arcs.
   */
  def intAutomaton[W:Semiring,T:Alphabet](initialStates: Map[Int,W], finalWeights: Map[Int,W])(arcs: Arc[W,Int,T]*): Automaton[W,Int,T] = {
    val arcMap =  arcs.groupBy(_.from);

    val map = new ArrayMap[Seq[Arc[W,Int,T]]] {
      override def defValue = Seq[Arc[W,Int,T]]();
    }
    map ++= arcMap;
    for( (s,_) <- finalWeights.iterator if !map.contains(s)) {
      map(s) = Seq.empty;
    }

    new Automaton[W,Int,T] {
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

      def edgesMatching(s: Int, t: T) = {
        if(t == alphabet.sigma) {
          arcMap.getOrElse(s,Seq.empty).iterator
        } else {
          arcMap.getOrElse(s,Seq.empty).iterator filter { arc =>
            arc.label == t || alphabet.matches(arc.label,t);
          };
        }
      }
      override def allEdges = arcs;
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
      val realArcs = for( (from,(to,label,w)) <- arcs) yield Arc(from,to,label,w);
      Automaton.automaton(initialStates,finalWeights)(realArcs:_*);
    }
  }
  
}
