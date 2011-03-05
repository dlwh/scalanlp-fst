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

import scalanlp.math.Semiring.LogSpace._
import scalanlp.fst.Alphabet
import scalanlp.util.Index
;

/**
 * Levenshtein transducer over sum of alignments (not viterbi
 * alignment) with the given parameters, which must be less than 0.
 *
 * A match is assumed to be 0.0, though these will be rescaled to ensure
 * that the edit distance encodes a (log space) probability distribution, or at
 * least that conditioned on either the input or the output, there is a distribution.
 * logDecay (&lt;=0.0) adds a preference for the length of the alignments.
 * That is, the scores will be shifted by some constant.
 *
 * @author dlwh
 */
trait EditDistanceFactory[T] { this: AutomatonFactory[T] =>
  class EditDistance(subRatio: Double, insRatio: Double) extends Transducer {
    require(subRatio < 0);
    require(insRatio < 0);

    private def totalChars = index.size - 1; // -1 for epsilon
    /**
     * Costs for each kind of parameter.
     */
    val (insCost,subCost,matchCost) = {
      val n = totalChars;
      import math.{exp,log};
      // we want to make edges out be markovian.
      // for any input label (resp. output label), there is 1 match, n-1 subs, and and 1 deletion
      // but we also have to accept an insertion of any kind, so there are n of those.
      //  c * ((n - 1 ) exp(sub) + 1 * exp(0) + (n+1) exp(del)) = 1.0
      // log c + log ((n -1 ) exp(sub) + 1 * exp(0) + (n+1) exp(del)) = 0.0
      val logC = - log( (n-1) * exp(subRatio) + 1 + (n+1) * exp(insRatio))
      (insRatio + logC,subRatio + logC,logC)
    }

    def initialState = 0;
    def initialWeight = 0.0;
    val finalWeights = Array(math.log(1 - math.exp(math.log(totalChars) + insCost)));
    def numStates = 1;
    val arcs = encoder.fillSparseArray(encoder.fillSparseArray(mkSparseVector(1)));
    for(i <- 0 until index.size; j <- 0 until index.size) {
      arcs.getOrElseUpdate(i).getOrElseUpdate(j)(0) = costOf(i,j)
    }
    def arcsFrom(s: Int) = {
      arcs
    }


    // symmetric
    def arcsWithOutput(s: Int, outCh: Int) = {
      arcs(outCh)
    }

    def arcsWithInput(s: Int, inCh: Int) = {
      arcs(inCh)
    }

    def arcsFrom(s: Int, ch1: Int, ch2: Int) = arcs(ch1)(ch2);

    private def costOf(a: Int, b: Int) = {
      if(a == epsilonIndex && b == epsilonIndex) Double.NegativeInfinity
      else if(a == epsilonIndex || b == epsilonIndex) insCost
      else if(a==b) matchCost
      else subCost;
    }
  }
}

object SpeedTest {
  def main(args: Array[String]) {
    import scalanlp.math.Semiring.LogSpace._;
    val allChars = Set() ++ ('a' to 'z')

    import scalanlp.util.Profiling
    {
      val factory = new AutomatonFactory[Char](Index( allChars + implicitly[Alphabet[Char]].epsilon));
      import factory._;
      val result1 = Profiling.time(10000) { () =>
        val ed = new EditDistance(-3,-4);
        val x = constant("abcdef",0.0);
        val rr = compose(x.asTransducer,ed).outputProjection;
        rr.cost
      }
      println(result1);
    }
    {
      import scalanlp.fst._;
      val result2 = Profiling.time(10000) { () =>
        val ed = new EditDistance(-3,-4, allChars);
        val x = Automaton.constant("abcdef",0.0);
        val rr = (x.asTransducer >> ed).outputProjection.relabel;
        rr.cost
      }
      println(result2);
    }

     {
      val factory = new AutomatonFactory[Char](Index( allChars + implicitly[Alphabet[Char]].epsilon));
      import factory._;
      val result1 = Profiling.time(10000) { () =>
        val ed = new EditDistance(-3,-4);
        val x = constant("abcdef",0.0);
        val rr = compose(x.asTransducer,ed).outputProjection;
        rr.cost
      }
      println(result1);
    }
    {
      import scalanlp.fst._;
      val result2 = Profiling.time(10000) { () =>
        val ed = new EditDistance(-3,-4, allChars);
        val x = Automaton.constant("abcdef",0.0);
        val rr = (x.asTransducer >> ed).outputProjection.relabel;
        rr.cost
      }
      println(result2);
    }

         {
      val factory = new AutomatonFactory[Char](Index( allChars + implicitly[Alphabet[Char]].epsilon));
      import factory._;
      val result1 = Profiling.time(10000) { () =>
        val ed = new EditDistance(-3,-4);
        val x = constant("abcdef",0.0);
        val rr = compose(x.asTransducer,ed).outputProjection;
        rr.cost
      }
      println(result1);
    }
    {
      import scalanlp.fst._;
      val result2 = Profiling.time(10000) { () =>
        val ed = new EditDistance(-3,-4, allChars);
        val x = Automaton.constant("abcdef",0.0);
        val rr = (x.asTransducer >> ed).outputProjection.relabel;
        rr.cost
      }
      println(result2);
    }


  }
}
