package scalanlp.fst

/*
 Copyright 2010 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
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

import breeze.math.Semiring

import scala.collection.mutable.ArrayBuffer
import scalanlp.fst.Composition.InboundEpsilon
import Composition._
import annotation.switch

object Composition {

  /**
   * Composition of two transducers in the general case.
   * Special handling for epsilons described in Mohri (2002). This supports an extension
   * where we can handle two distinct weight types as long as we have a way of composing them
   * into a composite weight. In normal composition, this is just product.
   */
  def compose[CC1,CC2,CCR](transA: CC1, transB: CC2)(implicit comp: Composer[CC1,CC2,CCR]):CCR = {
    comp.compose(transA,transB)
  }


  /**
   * These classes represent bookkeeping states for doing composition
   * in the presence of epsilons. They are essential, but you can
   * safely ignore them.
   */
  sealed abstract class InboundEpsilon(val num: Int)
  case object NoEps extends InboundEpsilon(0)
  case object LeftEps extends InboundEpsilon(1)
  case object RightEps extends InboundEpsilon(2)


}

trait Composer[-CC1, -CC2, +CCR] {
  def compose(cc1: CC1, cc2: CC2):CCR
}

object Composer {
  implicit def defaultComposer[CC1,CC2,W, S1, In, Mid, Out, S2, SR](implicit ev1: CC1<:<Transducer[W,S1,In,Mid],
               ev2: CC2<:< Transducer[W,S2,Mid,Out],
               comp: StateComposer[CC1,CC2,S1,S2,SR],
               ring1: Semiring[W],
               inAlpha: Alphabet[In],
               midAlpha: Alphabet[Mid],
               outAlpha: Alphabet[Out],
               inputSigmaMatcher: ArcMatcher[CC1,W,S1,(In,Mid),(Sigma.type,Mid)],
               outputSigmaMatcher: ArcMatcher[CC2,W,S2,(Mid,Out),(Mid,Sigma.type)]):Composer[CC1,CC2,ComposedTransducer[CC1,CC2,W,W,S1,S2,SR,In,Mid,Out,W]] = {
    new Composer[CC1,CC2,ComposedTransducer[CC1,CC2,W,W,S1,S2,SR,In,Mid,Out,W]] {
      def compose(cc1: CC1, cc2: CC2) = new ComposedTransducer(cc1,cc2,(x: Any, a: W, b: W) => ring1.*(a,b))
    }

  }
}

class ComposedTransducer[CC1,CC2,W1,W2,S1,S2,SR,In,Mid,Out,W3]
              (transA: CC1,
               transB: CC2,
               composeW: (Option[(In,Mid,Out)],W1,W2)=>W3)
              (implicit ev1: CC1<:<Transducer[W1,S1,In,Mid],
               ev2: CC2<:< Transducer[W2,S2,Mid,Out],
               comp: StateComposer[CC1,CC2,S1,S2,SR],
               ring1: Semiring[W1],
               ring2: Semiring[W2],
               ring3: Semiring[W3],
               inAlpha: Alphabet[In],
               midAlpha: Alphabet[Mid],
               outAlpha: Alphabet[Out],
               inputSigmaMatcher: ArcMatcher[CC1,W1,S1,(In,Mid),(Sigma.type,Mid)],
               outputSigmaMatcher: ArcMatcher[CC2,W2,S2,(Mid,Out),(Mid,Sigma.type)]) extends Transducer[W3,SR,In,Out] {
  private val InEps = implicitly[Alphabet[In]].epsilon
  private val MidEps = implicitly[Alphabet[Mid]].epsilon
  private val OutEps = implicitly[Alphabet[Out]].epsilon

  def leftState(s: SR) = comp.leftState(transA,transB,s)
  def rightState(s: SR) = comp.rightState(transA,transB,s)
  def epsState(s: SR) = comp.epsState(transA,transB,s)

  val initialStateWeights: Map[SR,W3] = {for {
    (k1,w1) <- transA.initialStateWeights
    (k2,w2) <-  transB.initialStateWeights
    w = composeW(None,w1,w2)
    if w != ring3.zero
  } yield (comp.compose(transA,transB,k1,k2,NoEps),w) } withDefaultValue (ring3.zero)


  def finalWeight(s: SR) = composeW(None,transA.finalWeight(comp.leftState(transA,transB,s)),transB.finalWeight(comp.rightState(transA,transB,s)))

  override def edgesFrom(s: SR) = {
    val left = comp.leftState(transA,transB,s)
    val right = comp.rightState(transA,transB,s)
    val epsStatus = comp.epsState(transA,transB,s)
    val nonEpsArcs = for {
      a1 @ Arc(from1,to1,(in1,out1),w1) <- transA.edgesFrom(left)
      if out1 != MidEps
      a2 @ Arc(from2,to2,(in2,out2),w2) <- outputSigmaMatcher.arcsMatching(transB,right,(out1,Sigma))
    } yield {
      Arc(s, comp.compose(transA,transB,to1,to2,NoEps), (in1,out2),composeW(Some((in1,out1,out2)),w1,w2))
    }

    // todo XXX: make this lazy.
    val epsArcs = {
      val arcs = new ArrayBuffer[Arc[W3,SR,(In,Out)]]
      epsStatus match {
        case NoEps =>
          for( Arc(_,to1,(in1,_),w)  <- inputSigmaMatcher.arcsMatching(transA,left,(Sigma,MidEps)) ) {
            arcs += Arc(s,comp.compose(transA,transB,to1,right,LeftEps),(in1,OutEps),composeW(Some(in1,MidEps,OutEps),w,ring2.one))
          }
          for( Arc(_,to,(_,out2),w)  <- outputSigmaMatcher.arcsMatching(transB,right,(MidEps,Sigma)) ) {
            arcs += Arc(s,comp.compose(transA,transB,left,to,RightEps),(InEps,out2),composeW(Some(InEps,MidEps,out2),ring1.one,w))
          }
          for(Arc(_,to1,(in1,_),w)  <- inputSigmaMatcher.arcsMatching(transA,left,(Sigma,MidEps));
              Arc(_,to2,(_,out2),w2) <- outputSigmaMatcher.arcsMatching(transB,right,(MidEps,Sigma))) {
            arcs += Arc(s,comp.compose(transA,transB,to1,to2,NoEps),(in1,out2),composeW(Some(in1,MidEps,out2),w,w2))
          }
        case LeftEps=>
          for( Arc(_,to1,(in1,_),w)  <- inputSigmaMatcher.arcsMatching(transA,left,(Sigma,MidEps))  ) {
            arcs += Arc(s,comp.compose(transA,transB,to1,right,LeftEps),(in1,OutEps),composeW(Some(in1,MidEps,OutEps),w,ring2.one))
          }
        case RightEps =>
          for( Arc(_,to,(_,out2),w)  <- outputSigmaMatcher.arcsMatching(transB,right,(MidEps,Sigma))) {
            arcs += Arc(s,comp.compose(transA,transB,left,to,RightEps),(InEps,out2),composeW(Some(InEps,MidEps,out2),ring1.one,w))
          }
      }
      arcs iterator
    }

    epsArcs ++ nonEpsArcs
  }
}

/**
 * Helper type class used by Composition to efficiently (or inefficiently) represent states
 */
trait StateComposer[-CC1,-CC2,S1,S2,ResultState] {
  def compose(cc1: CC1, cc2: CC2, s1: S1, s2: S2, epsState: InboundEpsilon):ResultState
  def leftState(cc1: CC1, cc2: CC2, sr: ResultState):S1
  def rightState(cc1: CC1, cc2: CC2, sr: ResultState):S2
  def epsState(cc1: CC1, cc2: CC2, sr: ResultState):InboundEpsilon
}

trait LowPriorityComposer {
  implicit def defaultComposer[W1, S1, T1, W2, S2, T2]:StateComposer[Automaton[W1,S1,T1],Automaton[W2,S2,T2],S1,S2,(S1,S2,InboundEpsilon)] = {
    new StateComposer[Automaton[W1,S1,T1],Automaton[W2,S2,T2],S1,S2,(S1,S2,InboundEpsilon)] {
      def epsState(cc1: Automaton[W1, S1, T1], cc2: Automaton[W2, S2, T2], sr: (S1, S2, InboundEpsilon)) = sr._3

      def rightState(cc1: Automaton[W1, S1, T1], cc2: Automaton[W2, S2, T2], sr: (S1, S2, InboundEpsilon)) = sr._2

      def leftState(cc1: Automaton[W1, S1, T1], cc2: Automaton[W2, S2, T2], sr: (S1, S2, InboundEpsilon)) = sr._1

      def compose(cc1: Automaton[W1, S1, T1], cc2: Automaton[W2, S2, T2], s1: S1, s2: S2, epsState: InboundEpsilon) = {
        (s1,s2,epsState)
      }
    }
  }
}

object StateComposer extends LowPriorityComposer {
  implicit def denseComposer[W1, T1, W2, T2]:StateComposer[DenseAutomaton[W1,T1],DenseAutomaton[W2,T2], Int, Int, Int] = {
    new StateComposer[DenseAutomaton[W1,T1],DenseAutomaton[W2,T2], Int, Int, Int] {
      def compose(cc1: DenseAutomaton[W1, T1], cc2: DenseAutomaton[W2, T2], s1: Int, s2: Int, epsState: InboundEpsilon) = {
        epsState.num + 3 * (s1 + cc1.states.size * s2)
      }

      def leftState(cc1: DenseAutomaton[W1, T1], cc2: DenseAutomaton[W2, T2], sr: Int) = sr/3%cc1.states.size
      def rightState(cc1: DenseAutomaton[W1, T1], cc2: DenseAutomaton[W2, T2], sr: Int) = sr/3/cc1.states.size
      def epsState(cc1: DenseAutomaton[W1, T1], cc2: DenseAutomaton[W2, T2], sr: Int) = (sr%3: @switch) match {
        case 0 => NoEps
        case 1 => LeftEps
        case 2 => RightEps
      }
    }
  }
}
