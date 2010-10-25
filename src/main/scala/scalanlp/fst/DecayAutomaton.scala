package scalanlp.fst;
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


import scalanlp.math.Semiring.LogSpace._;

/**
* Generates a geometric distribution over string lengths, and a uniform
* distribution over individual characters. Can simulate fake characters
* to remove chars.
*
* @author dlwh
*/
class DecayAutomaton(val expectedLength:Double, chars: Set[Char]) extends Automaton[Double,Int,Char] {
  import Transducer._;
  require( expectedLength > 0);
  // E[|X|] = p / (1-p)
  // p = E[X] / (1+E[X])
  val mass = math.log(expectedLength / (1+expectedLength));
  val stopProb = math.log( 1 / (1+expectedLength));

  val arcCost = {
    val n = chars.size;
    // we have n emissions
    // We want transitions to be markovian, so:
    //  c * n = exp(mass)
    mass - math.log(n)
  }

  val initialStateWeights = Map( 0 -> 0.0);

  def finalWeight(s: Int) = stopProb;

  override lazy val allEdges:Seq[Arc] = edgesMatching(0,alphabet.sigma).toSeq;

  def edgesMatching(s: Int, a: Char) = {
    if(s == 0) {
      if(a == alphabet.sigma) {
        val subs = for(a <- chars.iterator)
          yield Arc(0,0,a,arcCost)
        subs
      } else if(a != alphabet.epsilon)
        Iterator.single(Arc(0,0,a,arcCost))
      else Iterator.empty;
    } else {
      Iterator.empty;
    }
  }
}

