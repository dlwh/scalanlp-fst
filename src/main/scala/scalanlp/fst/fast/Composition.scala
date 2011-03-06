package scalanlp.fst.fast

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
import scalala.tensor.sparse.SparseVector
import scalanlp.collection.mutable.{ArrayMap, SparseArray}

/**
 * Composition of two transducers in the general case.
 * Special handling for epsilons described in Mohri (2002).
 *
 * @author dlwh
 */
trait Composition[T] { this: AutomatonFactory[T] =>

  trait Composed {
    def underlyingLeftState(state: Int):Int
    def underlyingRightState(state: Int):Int
    def underlyingEpsilonStatus(state: Int):InboundEpsilon
    def stateFor(a: Int, b: Int, eps: InboundEpsilon):Int;
  }

  protected trait IntegerComposed extends Composed {
    protected val leftStates: Int

    final def stateFor(a:Int, b: Int, eps: InboundEpsilon) = eps.num + 3 * (a + b * leftStates);

    final def underlyingEpsilonStatus(s: Int) =  (s % 3) match {
      case 0 => NoEps
      case 1 => LeftEps
      case 2 => RightEps
    }

    final def underlyingRightState(s: Int) = {
      s / 3 / leftStates
    }

    final def underlyingLeftState(s: Int) = {
      (s/3)%leftStates;
    }
  }

  def intersect(transA: Automaton, transB: Automaton):Automaton with Composed = {
    val lt = lazyIntersect(transA,transB);
    val stateMap = new ArrayMap[Int](-1); // old state -> newState
    val states = new ArrayBuffer[Int](); // new state -> old state
    def getNewDest(oldDest: Int) = {
      val index = stateMap(oldDest)
      if(index < 0) {
        states += oldDest;
        val r = states.size - 1;
        stateMap(oldDest) = r;
        r
      } else {
        index;
      }

    }

    states += lt.initialState;
    stateMap(lt.initialState) = 0;

    // newState -> inch -> outch -> dest -> weight
    val allArcs = new ArrayBuffer[SparseArray[SparseVector]];

    var nextIndex = 0;
    while(nextIndex < states.size) {
      val oldState = states(nextIndex);
      val newState = nextIndex;
      val arcs = encoder.fillSparseArray(mkSparseVector(lt.numStates));
      allArcs += arcs;
      nextIndex += 1;
      // ch1 -> old destinations -> score
      val oldArcs: SparseArray[SparseVector] = lt.arcsFrom(oldState);
      var ch1Index = 0;
      while(ch1Index < oldArcs.used) {
        val ch1 = oldArcs.indexAt(ch1Index);
        val weights = oldArcs.valueAt(ch1Index);
        ch1Index += 1;
        val newWeights = new SparseVector(lt.numStates,weights.used);
        newWeights.default = ring.zero
        arcs(ch1) = newWeights;
        var oldDestIndex = 0;
        while(oldDestIndex < weights.used) {
          val oldDest = weights.index(oldDestIndex);
          val weight = weights.data(oldDestIndex);
          oldDestIndex += 1;
          // will enqueue:
          val newDest = getNewDest(oldDest);
          newWeights(newDest) = weight;
        }
      }
    }

    val newFinalWeights = states.toArray.map(lt.finalWeight _);
    val leftStates = states.map(lt.underlyingLeftState _);
    val rightStates = states.map(lt.underlyingRightState _);
    val epsStates = states.map(lt.underlyingEpsilonStatus _);
    val startState = lt.initialState;
    val startWeight = lt.initialWeight;
    new Automaton with Composed {
      def numStates = allArcs.length;
      def arcsFrom(s:Int) = allArcs(s);
      def arcsFrom(s:Int, ch: Int) = allArcs(s)(ch);
      def initialState = startState
      def initialWeight: Double = startWeight;
      def finalWeights = newFinalWeights;

      def stateFor(a: Int, b: Int, eps: InboundEpsilon) = {
        // TODO
        error("not implemented");
      }

      def underlyingEpsilonStatus(state: Int) = {
        epsStates(state);
      }

      def underlyingRightState(state: Int): Int = {
        rightStates(state);
      }

      def underlyingLeftState(state: Int) = {
        leftStates(state);
      }
    }

  }

  def lazyIntersect(transA: Automaton, transB: Automaton):Automaton with Composed = {

    new Automaton with IntegerComposed {
      val initialState = stateFor(0,0,NoEps)
      val initialWeight = ring.times(transA.initialWeight,transB.initialWeight);

      val leftStates = transA.numStates;

      lazy val finalWeights = {
        Array.tabulate(numStates) { i =>
          finalWeight(i);
        }
      }

      override def finalWeight(s: Int) = {
        val a = underlyingLeftState(s);
        val b = underlyingRightState(s);
        ring.times(transA.finalWeight(a),transB.finalWeight(b));
      }

      def numStates = 3 * transA.numStates * transB.numStates;

      def arcsFrom(s: Int, ch: Int) = {
        val a = underlyingLeftState(s)
        val b = underlyingRightState(s);
        val eps = underlyingEpsilonStatus(s);
        val arcs = new SparseVector(numStates);
        arcs.default = ring.zero;
        // non-eps arcs
        if(ch != epsilonIndex) {
          for {
            (aTarget,aWeight) <- transA.arcsFrom(a,ch).activeElements
            (bTarget,bWeight) <- transB.arcsFrom(b,ch).activeElements
          } {
            val target = stateFor(aTarget,bTarget,NoEps);
            val weight = ring.times(aWeight,bWeight);
            arcs(target) = weight;
          }
        } else if(epsilonIndex >= 0) {
          // eps arcs:
          if(eps != RightEps) {
            for {
              (aTarget,aWeight) <- transA.arcsFrom(a,epsilonIndex).activeElements
            } {
              val target = stateFor(aTarget,b,LeftEps);
              val weight = ring.times(aWeight,ring.one);
              arcs(target) = weight;
            }
          }
          if(eps != LeftEps) {
            for {
              (bTarget,bWeight) <- transB.arcsFrom(b,epsilonIndex).activeElements
            } {
              val target = stateFor(a,bTarget,RightEps);
              val weight = ring.times(ring.one,bWeight);
              arcs(target) = weight;
            }
          }
          if(eps == NoEps) {
            for {
              (aTarget,aWeight) <- transA.arcsFrom(a,epsilonIndex).activeElements
              (bTarget,bWeight) <- transB.arcsFrom(b,epsilonIndex).activeElements
            } {
              val target = stateFor(aTarget,bTarget,NoEps);
              val weight = ring.times(aWeight,bWeight);
              arcs(target) = weight;
            }
          }
        }
        arcs
      }

      def arcsFrom(s: Int) = {
        val a = (s / 3)%transA.numStates;
        val b = (s / 3) / transA.numStates;
        val eps = (s % 3) match {
          case 0 => NoEps
          case 1 => LeftEps
          case 2 => RightEps
        }
        val arcs = encoder.fillSparseArray{
          val sp = new SparseVector(numStates);
          sp.default = ring.zero;
          sp
        }
        // non-eps arcs
        var aChIndex = 0;
        val arcsA = transA.arcsFrom(a);
        while(aChIndex < arcsA.used) {
          val aCh = arcsA.indexAt(aChIndex);
          val aTargets = arcsA.valueAt(aChIndex);
          aChIndex += 1;
          if(aCh != epsilonIndex) {
            var aIndex = 0;
            val bTargets = transB.arcsFrom(b,aCh)
            while(aIndex < aTargets.used) {
              val aTarget = aTargets.index(aIndex);
              val aWeight = aTargets.data(aIndex);
              aIndex += 1;
              var bIndex = 0
              while(bIndex < bTargets.used) {
                val bTarget = bTargets.index(bIndex);
                val bWeight = bTargets.data(bIndex);
                bIndex += 1;
                val target = stateFor(aTarget,bTarget,NoEps);
                val weight = ring.times(aWeight,bWeight);
                arcs.getOrElseUpdate(aCh)(target) = weight;
              }
            }
          }
        }
        // eps arcs:
        if(epsilonIndex >= 0)  {
          if(eps != RightEps) {
            for {
              (aTarget,aWeight) <- transA.arcsFrom(a,epsilonIndex).activeElements
            } {
              val target = stateFor(aTarget,b,LeftEps);
              val weight = ring.times(aWeight,ring.one);
              arcs.getOrElseUpdate(epsilonIndex)(target) = weight;
            }
          }
          if(eps != LeftEps) {
            for {
              (bTarget,bWeight) <- transB.arcsFrom(b,epsilonIndex).activeElements
            } {
              val target = stateFor(a,bTarget,RightEps);
              val weight = ring.times(ring.one,bWeight);
              arcs.getOrElseUpdate(epsilonIndex)(target) = weight;
            }
          }
          if(eps == NoEps) {
            for {
              (aTarget,aWeight) <- transA.arcsFrom(a,epsilonIndex).activeElements
              (bTarget,bWeight) <- transB.arcsFrom(b,epsilonIndex).activeElements
            } {
              val target = stateFor(aTarget,bTarget,NoEps);
              val weight = ring.times(aWeight,bWeight);
              arcs.getOrElseUpdate(epsilonIndex)(target) = weight;
            }
          }
        }

        arcs

      }
    };

  }

  def compose(transA: Transducer, transB: Transducer):Transducer = {
    val lt = lazyCompose(transA,transB);
    val stateMap = new ArrayMap[Int](-1); // old state -> newState
    val states = new ArrayBuffer[Int](); // new state -> old state
    def getNewDest(oldDest: Int) = {
      val index = stateMap(oldDest)
      if(index < 0) {
        states += oldDest;
        val r = states.size - 1;
        stateMap(oldDest) = r;
        r
      } else {
        index;
      }

    }

    states += lt.initialState;
    stateMap(lt.initialState) = 0;

    // newState -> inch -> outch -> dest -> weight
    val allArcs = new ArrayBuffer[SparseArray[SparseArray[SparseVector]]];

    var nextIndex = 0;
    while(nextIndex < states.size) {
      val oldState = states(nextIndex);
      val newState = nextIndex;
      val arcs = encoder.fillSparseArray(encoder.fillSparseArray(mkSparseVector(lt.numStates)));
      allArcs += arcs;
      nextIndex += 1;
      // ch1 -> ch2 -> old destinations -> score
      val oldArcs: SparseArray[SparseArray[SparseVector]] = lt.arcsFrom(oldState);
      var ch1Index = 0;
      while(ch1Index < oldArcs.used) {
        val ch1 = oldArcs.indexAt(ch1Index);
        val outs = oldArcs.valueAt(ch1Index);
        ch1Index += 1;
        val ch1Arcs = arcs.getOrElseUpdate(ch1);
        var ch2Index = 0;
        while(ch2Index < outs.used) {
          val ch2 = outs.indexAt(ch2Index);
          val weights = outs.valueAt(ch2Index);
          val newWeights = new SparseVector(lt.numStates,weights.used);
          newWeights.default = ring.zero
          ch1Arcs(ch2) = newWeights;
          ch2Index += 1;
          var oldDestIndex = 0;
          while(oldDestIndex < weights.used) {
            val oldDest = weights.index(oldDestIndex);
            val weight = weights.data(oldDestIndex);
            oldDestIndex += 1;
            // will enqueue:
            val newDest = getNewDest(oldDest);
            newWeights(newDest) = weight;
          }
        }
      }
    }

    val newFinalWeights = states.toArray.map(lt.finalWeight _);

    transducer(allArcs.toArray, newFinalWeights, startState = 0, startWeight = lt.initialWeight);
  }


  /**
   * Composition of two transducers in the general case.
   * Special handling for epsilons described in Mohri (2002). This supports an extension
   * where we can handle two distinct weight types as long as we have a way of composing them
   * into a composite weight. In normal composition, this is just product.
   */
  def lazyCompose(transA: Transducer, transB: Transducer):Transducer with Composed = {

    new Transducer with IntegerComposed {
      val leftStates = transA.numStates;
      val initialState = stateFor(0,0,NoEps)
      val initialWeight = ring.times(transA.initialWeight,transB.initialWeight);

      lazy val finalWeights = {
        Array.tabulate(numStates) { i =>
          finalWeight(i);
        }
      }

      override def finalWeight(s: Int) = {
        val a = (s / 3)%transA.numStates;
        val b = (s / 3) / transA.numStates;
        ring.times(transA.finalWeight(a),transB.finalWeight(b));
      }

      def numStates = 3 * transA.numStates * transB.numStates;

      def arcsFrom(s: Int, ch1: Int, ch2: Int) = {
        val a = (s / 3)%transA.numStates;
        val b = (s / 3) / transA.numStates;
        val eps = (s % 3) match {
          case 0 => NoEps
          case 1 => LeftEps
          case 2 => RightEps
        }
        val arcs = new SparseVector(numStates);
        arcs.default = ring.zero;
        for {
          (midCh,targets) <- transA.arcsWithInput(a,ch1) if midCh != epsilonIndex
          (bTarget,bWeight) <- transB.arcsFrom(b,midCh,ch2).activeElements
          (aTarget,aWeight) <- targets.activeElements
        } {
          val target = stateFor(aTarget,bTarget,NoEps);
          val weight = ring.times(aWeight,bWeight);
          arcs(target) = ring.plus(arcs(target),weight);
        }
        if(epsilonIndex >= 0) {
          // eps arcs:
          if(eps != RightEps && ch2 == epsilonIndex)
            for {
              (aTarget,aWeight) <- transA.arcsFrom(a,ch1,epsilonIndex).activeElements
            } {
              val target = stateFor(aTarget,b,LeftEps);
              val weight = ring.times(aWeight,ring.one);
              arcs(target) = weight;
            }
          if(eps != LeftEps && ch1 == epsilonIndex) {
            for {
              (bTarget,bWeight) <- transB.arcsFrom(b,epsilonIndex,ch2).activeElements
            } {
              val target = stateFor(a,bTarget,RightEps);
              val weight = ring.times(ring.one,bWeight);
              arcs(target) = weight;
            }
          }
          if(eps == NoEps) {
            for {
              (aTarget,aWeight) <- transA.arcsFrom(a,ch1,epsilonIndex).activeElements
              (bTarget,bWeight) <- transB.arcsFrom(b,epsilonIndex,ch2).activeElements
            } {
              val target = stateFor(aTarget,bTarget,NoEps);
              val weight = ring.times(aWeight,bWeight);
              arcs(target) = weight;
            }
          }
        }
        arcs
      }

      def arcsWithOutput(s: Int, ch2: Int) = {
        val a = (s / 3)%transA.numStates;
        val b = (s / 3) / transA.numStates;
        val eps = (s % 3) match {
          case 0 => NoEps
          case 1 => LeftEps
          case 2 => RightEps
        }
        val arcs = encoder.fillSparseArray{
          val sp = new SparseVector(numStates);
          sp.default = ring.zero;
          sp
        };
        for {
          (midCh,bTargets) <- transB.arcsWithOutput(b,ch2)if midCh != epsilonIndex
          (bTarget,bWeight) <- bTargets.activeElements;
          (ch1,targets) <- transA.arcsWithOutput(a,midCh)
          (aTarget,aWeight) <- targets.activeElements
        } {
          val target = stateFor(aTarget,bTarget,NoEps);
          val weight = ring.times(aWeight,bWeight);
          arcs.getOrElseUpdate(ch1)(target) = ring.plus(arcs.getOrElseUpdate(ch1)(target),weight);
        }
        if(epsilonIndex >= 0) {
          // eps arcs:
          if(eps != RightEps && ch2 == epsilonIndex)
            for {
              (inCh,targets) <- transA.arcsWithOutput(a,epsilonIndex);
              (aTarget,aWeight) <- targets.activeElements
            } {
              val target = stateFor(aTarget,b,LeftEps);
              val weight = ring.times(aWeight,ring.one);
              arcs.getOrElseUpdate(inCh)(target) = weight;
            }
          if(eps != LeftEps) {
            for {
              (bTarget,bWeight) <- transB.arcsFrom(b,epsilonIndex, ch2)
            } {
              val target = stateFor(a,bTarget,RightEps);
              val weight = ring.times(ring.one,bWeight);
              arcs.getOrElseUpdate(epsilonIndex)(target) = weight;
            }
          }
          if(eps == NoEps) {
            for {
              (bTarget,bWeight) <- transB.arcsFrom(b,epsilonIndex, ch2).activeElements
              (inCh,aTargets) <- transA.arcsWithOutput(a,epsilonIndex)
              (aTarget,aWeight) <- aTargets.activeElements
            } {
              val target = stateFor(aTarget,bTarget,NoEps);
              val weight = ring.times(aWeight,bWeight);
              arcs.getOrElseUpdate(inCh)(target) = weight;
            }
          }
        }
        arcs
      }



      def arcsFrom(s: Int) = {
        val a = (s / 3)%transA.numStates;
        val b = (s / 3) / transA.numStates;
        val eps = (s % 3) match {
          case 0 => NoEps
          case 1 => LeftEps
          case 2 => RightEps
        }
        val arcs = encoder.fillSparseArray(encoder.fillSparseArray{
          val sp = new SparseVector(numStates);
          sp.default = ring.zero;
          sp
        });
        // Yuck.
        var ch1Index = 0;
        val arcsA = transA.arcsFrom(a);
        // for each (input,arc pair) in A
        while(ch1Index < arcsA.used) {
          val ch1 = arcsA.indexAt(ch1Index);
          val mids = arcsA.valueAt(ch1Index);
          ch1Index += 1;
          var midChIndex = 0;
          // for each (output 'mid',destination pair) in A
          while(midChIndex < mids.used) {
            val midCh = mids.indexAt(midChIndex);
            val aTargets = mids.valueAt(midChIndex)
            midChIndex += 1;
            if(midCh != epsilonIndex) {
              // for each (output, arc pair) in B with input matching mid
              val arcsB = transB.arcsWithInput(b,midCh);
              var ch2Index = 0;
              while(ch2Index < arcsB.used) {
                val ch2 = arcsB.indexAt(ch2Index);
                val bTargets = arcsB.valueAt(ch2Index);
                ch2Index += 1;
                var bTargetIndex = 0;
                while(bTargetIndex < bTargets.used) {
                  // for each (bTarget, weight)
                  val bTarget = bTargets.index(bTargetIndex);
                  val bWeight = bTargets.data(bTargetIndex);
                  bTargetIndex += 1;
                  var aTargetIndex = 0
                  // for each (aTarget, weight)
                  while(aTargetIndex < aTargets.used) {
                    // new arc: ((a,b),ch1,ch2,(aTarget,bTarget)
                    val aTarget = aTargets.index(aTargetIndex);
                    val aWeight = aTargets.data(aTargetIndex);
                    val target = stateFor(aTarget,bTarget,NoEps);
                    val weight = ring.times(aWeight,bWeight);
                    arcs.getOrElseUpdate(ch1).getOrElseUpdate(ch2)(target) = ring.plus(arcs.getOrElseUpdate(ch1).getOrElseUpdate(ch2)(target),weight);
                    aTargetIndex += 1;
                  }
                }
              }
            }
          }
        }

        if(epsilonIndex >= 0) {
          // eps arcs:
          if(eps != RightEps)
            for {
              (inCh,targets) <- transA.arcsWithOutput(a,epsilonIndex);
              (aTarget,aWeight) <- targets.activeElements
            } {
              val target = stateFor(aTarget,b,LeftEps);
              val weight = ring.times(aWeight,ring.one);
              arcs.getOrElseUpdate(inCh).getOrElseUpdate(epsilonIndex)(target) = weight;
            }
          if(eps != LeftEps) {
            for {
              (outCh,bTargets) <- transB.arcsWithInput(b,epsilonIndex);
              (bTarget,bWeight) <- bTargets.activeElements
            } {
              val target = stateFor(a,bTarget,RightEps);
              val weight = ring.times(ring.one,bWeight);
              arcs.getOrElseUpdate(epsilonIndex).getOrElseUpdate(outCh)(target) = weight;
            }
          }
          if(eps == NoEps) {
            for {
              (inCh,aTargets) <- transA.arcsWithOutput(a,epsilonIndex)
              (outCh,bTargets) <- transB.arcsWithInput(b,epsilonIndex)
              (aTarget,aWeight) <- aTargets.activeElements
              (bTarget,bWeight) <- bTargets.activeElements
            } {
              val target = stateFor(aTarget,bTarget,NoEps);
              val weight = ring.times(aWeight,bWeight);
              arcs.getOrElseUpdate(inCh).getOrElseUpdate(outCh)(target) = weight;
            }
          }
        }
        arcs
      }

      def arcsWithInput(s: Int, ch1: Int) = {
        val a = (s / 3)%transA.numStates;
        val b = (s / 3) / transA.numStates;
        val eps = (s % 3) match {
          case 0 => NoEps
          case 1 => LeftEps
          case 2 => RightEps
        }
        val arcs = encoder.fillSparseArray{
          val sp = new SparseVector(numStates);
          sp.default = ring.zero;
          sp
        };
        for {
          (midCh,targets) <- transA.arcsWithInput(a,ch1) if midCh != epsilonIndex
          (ch2,bTargets) <- transB.arcsWithInput(b,midCh)
          (bTarget,bWeight) <- bTargets.activeElements;
          (aTarget,aWeight) <- targets.activeElements
        } {
          val target = stateFor(aTarget,bTarget,NoEps);
          val weight = ring.times(aWeight,bWeight);
          arcs.getOrElseUpdate(ch2)(target) = ring.plus(arcs.getOrElseUpdate(ch2)(target),weight);
        }
        if(epsilonIndex >= 0) {
          // eps arcs:
          if(eps != RightEps)
            for {
              (aTarget,aWeight) <- transA.arcsFrom(a,ch1,epsilonIndex).activeElements
            } {
              val target = stateFor(aTarget,b,LeftEps);
              val weight = ring.times(aWeight,ring.one);
              arcs.getOrElseUpdate(epsilonIndex)(target) = weight;
            }
          if(eps != LeftEps && ch1 == epsilonIndex) {
            for {
              (outCh,bTargets) <- transB.arcsWithInput(b,epsilonIndex);
              (bTarget,bWeight) <- bTargets.activeElements
            } {
              val target = stateFor(a,bTarget,RightEps);
              val weight = ring.times(ring.one,bWeight);
              arcs.getOrElseUpdate(outCh)(target) = weight;
            }
          }
          if(eps == NoEps) {
            for {
              (aTarget,aWeight) <- transA.arcsFrom(a,ch1,epsilonIndex).activeElements
              (outCh,bTargets) <- transB.arcsWithInput(b,epsilonIndex)
              (bTarget,bWeight) <- bTargets.activeElements
            } {
              val target = stateFor(aTarget,bTarget,NoEps);
              val weight = ring.times(aWeight,bWeight);
              arcs.getOrElseUpdate(outCh)(target) = weight;
            }
          }
        }
        arcs
      }

    };


  }

  /*
  /**
   * Composition of two transducers in the general case.
   * Special handling for epsilons described in Mohri (2002). This supports an extension
   * where we can handle two distinct weight types as long as we have a way of composing them
   * into a composite weight. In normal composition, this is just product.
   */
  def compose[W1:Semiring,W2:Semiring,S1,S2,In:Alphabet,Mid:Alphabet,Out:Alphabet,W3:Semiring:ClassManifest, S3]
              (transA: Transducer[W1,S1,In,Mid],
               transB: Transducer[W2,S2,Mid,Out],
               composeW: (Option[(In,Mid,Out)],W1,W2)=>W3)
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

  */

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


