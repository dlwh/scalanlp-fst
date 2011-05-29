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
 * Generates a geometric distribution over string lengths, and a uniform
 * distribution over individual characters.
 * log semiring.
 *
 * @author dlwh
 */
class DecayAutomaton(val expectedLength:Double, chars: Set[Char]) extends DenseAutomaton[Double,Char] with SelectingAutomaton[Double,Int,Char] with AutomatonLike[Double,Int,Char,DecayAutomaton] {
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

  override lazy val edges = edgesFrom(0).toIndexedSeq

  override def edgesFrom(s: Int) = {
    for(a <- chars.iterator) yield Arc(0,0,a,arcCost)
  }

  def selectEdges(from: Int, label: Char) = {
    if(chars(label)) Iterator(Arc(0,0,label,arcCost));
    else Iterator.empty;
  }
}

