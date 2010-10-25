package scalanlp.fst.templates
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
import scalanlp.fst._

class PositionalUnigramModel[W:Semiring:ClassManifest,T:Alphabet](init: T, chars: Set[T], maxLength: Int) extends Automaton[W,Int,T] {
  val initialStateWeights = Map( 0 -> ring.one);

  def finalWeight(s: Int) = if(s == -1) ring.one else ring.zero;

  private def next(s: Int) = (s+1) min (maxLength-1);

  def edgesMatching(s: Int, a: T) = {
    val pos = next(s);
    if(s < 0 || s >= maxLength) {
      Iterator.empty
    } else if (a == alphabet.sigma) {
      val realArcs = for(a <- chars.iterator)
        yield Arc(s,pos,a,ring.one)
      val epsArc = Arc(s,-1,alphabet.epsilon,ring.one);
      Iterator.single(epsArc) ++ realArcs;
    } else if(a == alphabet.epsilon) {
      Iterator.single(Arc(s,-1,a,ring.one));
    } else {
      Iterator.single(Arc(s,pos,a,ring.one));
    }
  }
}

object SpeedTest {
  import scalanlp.util.Profiling;
  def main(args: Array[String]) {
    import scalanlp.math.Semiring.LogSpace._;
    val alphabet:Set[Char] = ("helloworld").toSet;
    val ed = new EditDistance(-3,-4,alphabet);
    val base = Automaton.constant("helloworld",0.0);
    val auto = (base.asTransducer >> ed).outputProjection.relabel;

    val posuni = new PositionalUnigramSemiring(10, alphabet, '#', true);
    val score2 = Profiling.time(1000) { () =>
      import posuni._;
      val cost = auto.reweight(posuni.promote _, posuni.promoteOnlyWeight _).cost;
    }
    println(score2);
    val model = new PositionalUnigramModel[Double,Char]('#',alphabet,10);
    val score1 = Profiling.time(1000) { () =>
      val counts = ExpectedCounts.counts(auto, model);
    }
    println(score1);

  }

}
