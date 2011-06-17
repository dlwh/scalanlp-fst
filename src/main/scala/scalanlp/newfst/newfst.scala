package scalanlp

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


import scalanlp.collection.mutable.ArrayMap
import scalanlp.math.Semiring

import scalanlp.newfst._;

import scalala.collection.sparse.DefaultArrayValue

package object newfst {
  /**
   * A Transducer is an Automaton with an input label and an output label
   */
  type Transducer[W,State,In,Out] = Automaton[W,State,(In,Out)];

  object Transducer {
    /**
     * Creates a transducer with the given initial states, final states, and arcs.
     */
    def transducer
    [W:Semiring,S,In,Out]
    (initialStates: Map[S,W], finalWeights: Map[S,W])
    (arcs: Arc[W,S,(In,Out)]*)
    : Transducer[W,S,In,Out] = {

      val allEdgesByOrigin = arcs.groupBy(_.source);

      new Transducer[W,S,In,Out] {
        val initialStateWeights = initialStates;
        def finalWeight(s: S) = finalWeights.getOrElse(s,implicitly[Semiring[W]].zero);
        def edgesFrom(s: S) = allEdgesByOrigin.getOrElse(s,Seq.empty) iterator
        override def edges = arcs;
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
      val inEps = implicitly[Alphabet[In]].epsilon;
      val outEps = implicitly[Alphabet[Out]].epsilon;
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

    implicit def transducerExtras[W:Semiring,State,In,Out](t: Transducer[W,State,In,Out]) = new TransducerExtras(t);
  }

  implicit def transducerExtras[W,State,In,Out](t: Transducer[W,State,In,Out]) = new TransducerExtras(t);
  /**
  * provides extra methods for transducers.
  */
  class TransducerExtras[W,State,In,Out](outer: Transducer[W,State,In,Out]) {

    /**
    * shorthand for composition
    */
    def >>[S2,Out2](t2: Transducer[W,S2,Out,Out2])
                   (implicit alpha: Alphabet[In],
                    midAlpha: Alphabet[Out],
                    outAlpha: Alphabet[Out2],
                    ring: Semiring[W]) = {
      Composition.compose(outer,t2)
    }

    /**
     * Returns a transducer where each arc's input and output are swapped.
     */
    def swapLabels: Transducer[W,State,Out,In] = new Transducer[W,State,Out,In] {
      val initialStateWeights = outer.initialStateWeights;
      def finalWeight(s: State) = outer.finalWeight(s);
      def edgesFrom(s: State) = outer.edgesFrom(s).map {
        case Arc(from,to,outin,w) => Arc(from,to,outin.swap,w);
      }
    }

    /**
     * Returns an automaton where each arc is labeled only with the input.
     */
    def inputProjection:Automaton[W,State,In] = new Automaton[W,State,In] {
      val initialStateWeights = outer.initialStateWeights;
      def finalWeight(s: State) = outer.finalWeight(s);
      def edgesFrom(a: State) = outer.edgesFrom(a).map {
        case Arc(from,to,(in,out),w) => Arc(from,to,in,w)
      }
    }

    /**
     * Returns an automaton where each arc is labeled only with the output.
     */
    def outputProjection: Automaton[W,State,Out] = new Automaton[W,State,Out] {
      val initialStateWeights = outer.initialStateWeights;
      def finalWeight(s: State) = outer.finalWeight(s);
      def edgesFrom(a: State) = outer.edgesFrom(a).map {
        case Arc(from,to,(in,out),w) => Arc(from,to,out,w)
      }
    }
  }

}
