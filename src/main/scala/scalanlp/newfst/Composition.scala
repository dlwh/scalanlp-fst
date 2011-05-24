package scalanlp.newfst

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
  def compose[CC1,CC2,
              W1,W2,
              S1,S2,
              In,Mid,Out,W3](transA: CC1,
                             transB: CC2,
                             composeW: (W1,W2)=>W3)
                            (implicit ev1: CC1<:<Transducer[W1,S1,In,Mid],
                             ev2: CC2<:< Transducer[W2,S2,Mid,Out],
                             ring1: Semiring[W1],
                             ring2: Semiring[W2],
                             ring3: Semiring[W3],
                             inAlpha: Alphabet[In],
                             midAlpha: Alphabet[Mid],
                             outAlpha: Alphabet[Out],
                             inputSigmaMatcher: ArcMatcher[CC1,W1,S1,(In,Mid),(Sigma.type,Mid)],
                             outputSigmaMatcher: ArcMatcher[CC2,W2,S2,(Mid,Out),(Mid,Sigma.type)])
            :Transducer[W3,(S1,S2,InboundEpsilon),In,Out] = {
    compose(transA,transB,{(a:Option[(In,Mid,Out)],b:W1,c:W2) => composeW(b,c)});
  }


  /**
   * Composition of two transducers in the general case.
   * Special handling for epsilons described in Mohri (2002). This supports an extension
   * where we can handle two distinct weight types as long as we have a way of composing them
   * into a composite weight. In normal composition, this is just product.
   */
  def compose[CC1,CC2,W1,W2,S1,S2,In,Mid,Out,W3, S3]
              (transA: CC1,
               transB: CC2,
               composeW: (Option[(In,Mid,Out)],W1,W2)=>W3)
              (implicit ev1: CC1<:<Transducer[W1,S1,In,Mid],
               ev2: CC2<:< Transducer[W2,S2,Mid,Out],
               ring1: Semiring[W1],
               ring2: Semiring[W2],
               ring3: Semiring[W3],
               inAlpha: Alphabet[In],
               midAlpha: Alphabet[Mid],
               outAlpha: Alphabet[Out],
               inputSigmaMatcher: ArcMatcher[CC1,W1,S1,(In,Mid),(Sigma.type,Mid)],
               outputSigmaMatcher: ArcMatcher[CC2,W2,S2,(Mid,Out),(Mid,Sigma.type)]):Transducer[W3,(S1,S2,InboundEpsilon),In,Out] = {

    val InEps = implicitly[Alphabet[In]].epsilon;
    val MidEps = implicitly[Alphabet[Mid]].epsilon;
    val OutEps = implicitly[Alphabet[Out]].epsilon;

    new Transducer[W3,(S1,S2,InboundEpsilon),In,Out] {
      val initialStateWeights: Map[(S1,S2,InboundEpsilon),W3] = {for {
        (k1,w1) <- transA.initialStateWeights;
        (k2,w2) <-  transB.initialStateWeights
        w = composeW(None,w1,w2)
        if w != ring3.zero
      } yield ((k1,k2,NoEps:InboundEpsilon),w) } withDefaultValue (ring3.zero)


      def finalWeight(s: (S1,S2,InboundEpsilon)) = composeW(None,transA.finalWeight(s._1),transB.finalWeight(s._2));

      override def edgesFrom(s: (S1,S2,InboundEpsilon)) = {
        val nonEpsArcs = for {
          a1 @ Arc(from1,to1,(in1,out1),w1) <- transA.edgesFrom(s._1);
          if out1 != MidEps
          a2 @ Arc(from2,to2,(in2,out2),w2) <- outputSigmaMatcher.arcsMatching(transB,s._2,(out1,Sigma))
        } yield {
          Arc(s, (to1,to2,NoEps), (in1,out2),composeW(Some(in1,out1,out2),w1,w2));
        }

        // todo XXX: make this lazy.
        val epsArcs = {
          val arcs = new ArrayBuffer[Arc[W3,(S1,S2,InboundEpsilon),(In,Out)]];
          s._3 match {
            case NoEps =>
                for( Arc(_,to1,(in1,_),w)  <- inputSigmaMatcher.arcsMatching(transA,s._1,(Sigma,MidEps)) ) {
                  arcs += Arc(s,(to1,s._2,LeftEps),(in1,OutEps),composeW(Some(in1,MidEps,OutEps),w,ring2.one));
                }
                for( Arc(_,to,(_,out2),w)  <- outputSigmaMatcher.arcsMatching(transB,s._2,(MidEps,Sigma)) ) {
                  arcs += Arc(s,(s._1,to,RightEps),(InEps,out2),composeW(Some(InEps,MidEps,out2),ring1.one,w));
                }
                for(Arc(_,to1,(in1,_),w)  <- inputSigmaMatcher.arcsMatching(transA,s._1,(Sigma,MidEps));
                    Arc(_,to2,(_,out2),w2) <- outputSigmaMatcher.arcsMatching(transB,s._2,(MidEps,Sigma))) {
                  arcs += Arc(s,(to1,to2,NoEps),(in1,out2),composeW(Some(in1,MidEps,out2),w,w2));
                }
            case LeftEps=>
                for( Arc(_,to1,(in1,_),w)  <- inputSigmaMatcher.arcsMatching(transA,s._1,(Sigma,MidEps))  ) {
                  arcs += Arc(s,(to1,s._2,LeftEps),(in1,OutEps),composeW(Some(in1,MidEps,OutEps),w,ring2.one));
                }
            case RightEps =>
                for( Arc(_,to,(_,out2),w)  <- outputSigmaMatcher.arcsMatching(transB,s._2,(MidEps,Sigma))) {
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
  sealed abstract class InboundEpsilon(val num: Int);
  case object NoEps extends InboundEpsilon(0)
  case object LeftEps extends InboundEpsilon(1)
  case object RightEps extends InboundEpsilon(2);


}


