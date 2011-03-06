package scalanlp.fst.fast.templates

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

import scalanlp.math.Semiring
import scalanlp.fst.fast._
import scalanlp.util.Index
import scalanlp.fst.Alphabet

trait PositionalUnigramModelFactory[T] { this: AutomatonFactory[T] =>
  class PositionalUnigramModel(init: T, maxLength: Int) extends Automaton {
    val initialState = 0;
    def initialWeight = ring.one;

    override def finalWeight(s: Int) = if(s == maxLength) ring.one else ring.zero

    private def next(s: Int) = (s+1) min (maxLength-1);

    lazy val finalWeights = Array.tabulate(numStates)(finalWeight _);

    def numStates = maxLength + 1;

    val arcs = Array.tabulate(numStates) { i =>
       val arr = encoder.fillSparseArray(mkSparseVector(numStates));
       if(i < maxLength)
         for(ch <- 0 until index.size) {
           if(ch == epsilonIndex) arr.getOrElseUpdate(ch)(maxLength) = ring.one;
           else arr.getOrElseUpdate(ch)(next(i)) = ring.one;
         }
       arr
    }

    def arcsFrom(s: Int, ch: Int) = {
      arcs(s)(ch);
    }

    def arcsFrom(s: Int) = {
      arcs(s);
    }

  }
}

object PSpeedTest {
  import scalanlp.util.Profiling;
  def main(args: Array[String]) {
    import scalanlp.math.Semiring.LogSpace._;
    val alphabet:Set[Char] = ("helloworld").toSet ++ ('a' to 'z');
    val factory = new AutomatonFactory[Char](Index(alphabet + implicitly[Alphabet[Char]].epsilon + '#'));
    import factory._;
    val ed = new EditDistance(-3,-4,alphabet);
    val base = constant("helloworld",0.0);
    val auto = compose(base.asTransducer,ed).outputProjection;

    val model = new PositionalUnigramModel('#',10);
    val score1 = Profiling.time(1000) { () =>
      val counts = expectedCounts(auto, model);
    }
    println(score1);

  }

}

