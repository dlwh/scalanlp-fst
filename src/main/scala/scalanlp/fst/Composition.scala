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

/**
 * Handles the composition of automata and transducers
 */
import scala.collection.mutable.ArrayBuffer
import scalanlp.math.Semiring

object Composition {

  /**
   * Composition of two transducers in the general case.
   * Special handling for epsilons described in Mohri (2002). This supports an extension
   * where we can handle two distinct weight types as long as we have a way of composing them
   * into a composite weight. In normal composition, this is just product.
   */
  def compose[W1:Semiring,W2:Semiring,S1,S2,In:Alphabet,Mid:Alphabet,Out:Alphabet,W3:Semiring:ClassManifest]
            (transA: Transducer[W1,S1,In,Mid], transB: Transducer[W2,S2,Mid,Out], composeW: (W1,W2)=>W3)
            :Transducer[W3,(S1,S2,InboundEpsilon),In,Out] = {
    compose(transA,transB,{(a:Option[Any],b:W1,c:W2) => composeW(b,c)});
  }

  /**
   * Composition of two transducers in the general case.
   * Special handling for epsilons described in Mohri (2002). This supports an extension
   * where we can handle two distinct weight types as long as we have a way of composing them
   * into a composite weight. In normal composition, this is just product.
   */
  def compose[W1:Semiring,W2:Semiring,S1,S2,In:Alphabet,Mid:Alphabet,Out:Alphabet,W3:Semiring:ClassManifest]
              (transA: Transducer[W1,S1,In,Mid], transB: Transducer[W2,S2,Mid,Out], composeW: (Option[(In,Mid,Out)],W1,W2)=>W3)
    :Transducer[W3,(S1,S2,InboundEpsilon),In,Out] = {

    val InEps = implicitly[Alphabet[In]].epsilon;
    val MidEps = implicitly[Alphabet[Mid]].epsilon;
    val OutEps = implicitly[Alphabet[Out]].epsilon;
    val InSigma = implicitly[Alphabet[In]].sigma;
    val MidSigma = implicitly[Alphabet[Mid]].sigma;
    val OutSigma = implicitly[Alphabet[Out]].sigma;
    val ring1 = implicitly[Semiring[W1]];
    val ring2 = implicitly[Semiring[W2]];
    val sr = implicitly[Semiring[W3]];

    new Transducer[W3,(S1,S2,InboundEpsilon),In,Out] {
      val initialStateWeights: Map[(S1,S2,InboundEpsilon),W3] = for {
        (k1,w1) <- transA.initialStateWeights;
        (k2,w2) <-  transB.initialStateWeights
        w = composeW(None,w1,w2)
        if w != sr.zero
      } yield ((k1,k2,NoEps:InboundEpsilon),w);


      def finalWeight(s: (S1,S2,InboundEpsilon)) = composeW(None,transA.finalWeight(s._1),transB.finalWeight(s._2));

      override def edgesMatching(s: (S1,S2,InboundEpsilon), inout: (In, Out)) = {
        val (in,out) = inout;
        val nonEpsArcs = for {
          a1 @ Arc(from1,to1,(in1,out1),w1) <- transA.edgesMatching(s._1,(in,MidSigma));
          if out1 != MidEps
          a2 @ Arc(from2,to2,(in2,out2),w2) <- transB.edgesMatching(s._2,(out1,out))
        } yield {
          Arc(s, (to1,to2,NoEps), (in1,out2),composeW(Some(in1,out1,out2),w1,w2));
        }

        // todo XXX: make this lazy.
        val epsArcs = {
          val arcs = new ArrayBuffer[Arc];
          s._3 match {
            case NoEps =>
              if(out == OutEps || out == OutSigma)
                for( Arc(_,to1,(in1,_),w)  <- transA.edgesMatching(s._1,(in,MidEps)) ) {
                  arcs += Arc(s,(to1,s._2,LeftEps),(in1,OutEps),composeW(Some(in1,MidEps,OutEps),w,ring2.one));
                }
              if(in == InEps || in == InSigma)
                for( Arc(_,to,(_,out2),w)  <- transB.edgesMatching(s._2,(MidEps,out)) ) {
                  arcs += Arc(s,(s._1,to,RightEps),(InEps,out2),composeW(Some(InEps,MidEps,out2),ring1.one,w));
                }
              if( (in == InEps || in == InSigma) && (out == OutEps || out == OutSigma))
                for(Arc(_,to1,(in1,_),w)  <- transA.edgesMatching(s._1,(in,MidEps));
                    Arc(_,to2,(_,out2),w2) <- transB.edgesMatching(s._2,(MidEps,out))) {
                  arcs += Arc(s,(to1,to2,NoEps),(in1,out2),composeW(Some(in1,MidEps,out2),w,w2));
                }
            case LeftEps=>
              if(out == OutEps || out == OutSigma)
                for( Arc(_,to1,(in1,_),w)  <- transA.edgesMatching(s._1,(in,MidEps)) ) {
                  arcs += Arc(s,(to1,s._2,LeftEps),(in1,OutEps),composeW(Some(in1,MidEps,OutEps),w,ring2.one));
                }
            case RightEps =>
              if(in == InEps || in == InSigma)
                for( Arc(_,to,(_,out2),w)  <- transB.edgesMatching(s._2,(MidEps,out)) ) {
                  arcs += Arc(s,(s._1,to,RightEps),(InEps,out2),composeW(Some(InEps,MidEps,out2),ring1.one,w));
                }
          }
          arcs iterator;
        }

        epsArcs ++ nonEpsArcs
      }
    }
  };


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
