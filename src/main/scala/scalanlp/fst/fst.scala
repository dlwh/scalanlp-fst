package scalanlp

import scalanlp.collection.mutable.ArrayMap
import scalanlp.math.Semiring

import scalanlp.fst._;

package object fst {
  /**
   * A Transducer is an Automaton with an input label and an output label
   */
  type Transducer[W,State,In,Out] = Automaton[W,State,(In,Out)];

  object Transducer {
    /**
     * Creates a transducer with the given initial states, final states, and arcs.
     */
    def intTransducer[W:Semiring:ClassManifest,In:Alphabet,Out:Alphabet](initialStates: Map[Int,W], finalWeights: Map[Int,W])(arcs: Arc[W,Int,(In,Out)]*): Transducer[W,Int,In,Out] = {
      val inAlpha = implicitly[Alphabet[In]];
      val outAlpha = implicitly[Alphabet[Out]];

      val arcMap =  arcs.groupBy(_.from);

      val map = new ArrayMap[Seq[Arc[W,Int,(In,Out)]]] {
        override def defValue = Seq.empty;
      }
      map ++= arcMap;
      for( (s,_) <- finalWeights.iterator if !map.contains(s)) {
        map(s) = Seq.empty;
      }

      new Transducer[W,Int,In,Out] {
        override def allEdgesByOrigin = map;
        val initialStateWeights = initialStates;
        def finalWeight(s: Int) = finalWeights.getOrElse(s,ring.zero);
        override val finalStateWeights = finalWeights withDefaultValue(ring.zero);

        override protected[fst] def makeMap[T:ClassManifest](dflt: => T): ArrayMap[T] = {
          new ArrayMap[T] {
            override def defValue = dflt;
          }
        }

        def edgesMatching(s: Int, inout: (In,Out)) = {
          val (in,out) = inout;
          if(in == inAlpha.sigma && out == outAlpha.sigma) {
            arcMap.getOrElse(s,Seq.empty).iterator
          } else {
            arcMap.getOrElse(s,Seq.empty).iterator filter { arc =>
              (in == inAlpha.sigma || in == arc.label._1) && (out == outAlpha.sigma || out == arc.label._2)
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
    [W:Semiring:ClassManifest,S,In:Alphabet,Out:Alphabet]
    (initialStates: Map[S,W], finalWeights: Map[S,W])
    (arcs: Arc[W,S,(In,Out)]*)
    : Transducer[W,S,In,Out] = {
      val inAlpha = implicitly[Alphabet[In]];
      val outAlpha = implicitly[Alphabet[Out]];

      new Transducer[W,S,In,Out] {
        val initialStateWeights = initialStates;
        def finalWeight(s: S) = finalWeights.getOrElse(s,implicitly[Semiring[W]].zero);
        override val finalStateWeights = finalWeights;
        override val allEdgesByOrigin = super.allEdgesByOrigin;
        def edgesMatching(s: S, inout: (In,Out)) = {
          val (in,out) = inout;
          if(in == inAlpha.sigma && out == outAlpha.sigma) {
            allEdgesByOrigin.getOrElse(s,Seq.empty) iterator
          } else {
            allEdgesByOrigin.getOrElse(s,Seq.empty).iterator filter { arc =>
              (in == inAlpha.sigma || in == arc.label._1) && (out == outAlpha.sigma || out == arc.label._2)
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
    class DSL[S,W:Semiring:ClassManifest,In:Alphabet,Out:Alphabet] {
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
        val realArcs = for( (from,(to,in,out,w)) <- arcs) yield Arc(from,to,(in,out),w);
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

    implicit def transducerExtras[W:Semiring:ClassManifest,State,In:Alphabet,Out:Alphabet](t: Transducer[W,State,In,Out]) = new TransducerExtras(t);
  }

  implicit def transducerExtras[W:Semiring:ClassManifest,State,In:Alphabet,Out:Alphabet](t: Transducer[W,State,In,Out]) = new TransducerExtras(t);
  class TransducerExtras[W:Semiring:ClassManifest,State,In:Alphabet,Out:Alphabet](outer: Transducer[W,State,In,Out]) {
    private def ring = implicitly[Semiring[W]];
    private def inAlpha = implicitly[Alphabet[In]];
    private def outAlpha = implicitly[Alphabet[Out]];

    def >>[S2,Out2:Alphabet](t2: Transducer[W,S2,Out,Out2]) = Composition.compose(outer,t2,ring.times _ )

    /**
     * Returns a transducer where each arc's input and output are swapped.
     */
    def swapLabels: Transducer[W,State,Out,In] = new Transducer[W,State,Out,In] {
      val initialStateWeights = outer.initialStateWeights;
      def finalWeight(s: State) = outer.finalWeight(s);
      def edgesMatching(s: State, outin: (Out,In)) = outer.edgesMatching(s,outin.swap).map {
        case Arc(from,to,outin,w) => Arc(from,to,outin.swap,w);
      }
      protected[fst] override def makeMap[T:ClassManifest](default: =>T) = outer.makeMap(default);
    }

    /**
     * Returns an automaton where each arc is labeled only with the input.
     */
    def inputProjection:Automaton[W,State,In] = new Automaton[W,State,In] {
      val initialStateWeights = outer.initialStateWeights;
      def finalWeight(s: State) = outer.finalWeight(s);
      def edgesMatching(a: State, trans: In) = outer.edgesMatching(a,(trans, outAlpha.sigma)).map {
        case Arc(from,to,(in,out),w) => Arc(from,to,in,w)
      }
      protected[fst] override def makeMap[T:ClassManifest](default: =>T) = outer.makeMap(default);
    }

    /**
     * Returns an automaton where each arc is labeled only with the output.
     */
    def outputProjection: Automaton[W,State,Out] = new Automaton[W,State,Out] {
      val initialStateWeights = outer.initialStateWeights;
      def finalWeight(s: State) = outer.finalWeight(s);
      def edgesMatching(a: State, trans: Out) = outer.edgesMatching(a, (inAlpha.sigma, trans)).map {
        case Arc(from,to,(in,out),w) => Arc(from,to,out,w)
      }
      protected[fst] override def makeMap[T:ClassManifest](default: =>T) = outer.makeMap(default);


    }
  }

}
