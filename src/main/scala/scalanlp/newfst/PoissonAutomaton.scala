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




import scalanlp.stats.sampling.Poisson;

/**
* Generates a poisson distribution over string lengths, and a uniform
* distribution over individual characters, up to some maximum length.
* Afterwards, it is a geometric distrbution.
*
* Uses log probabilities.
*
* @author dlwh
*/
class PoissonAutomaton(val expectedLength: Double,
                       maxLength: Int,
                       chars: Set[Char],
                       geometricStoppingProb: Double = 0.9) extends DenseAutomaton[Double,Char] with AutomatonLike[Double,Int,Char,PoissonAutomaton] {
  require( expectedLength > 0);

  private val poi = new Poisson(expectedLength);
  private val arcCost = -math.log(chars.size);
  private val leftOverProb = scalala.library.Numerics.logDiff(0.0,poi.logCdf(maxLength-1));

  val initialStateWeights = Map(0 -> 0.0);
  def finalWeight(s: Int) = if(s < maxLength) poi.logProbabilityOf(s) else leftOverProb + math.log(geometricStoppingProb);

  def edgesFrom(s: Int) = {
    if(s < maxLength) {
      for(a <- chars.iterator) yield Arc(s,s+1,a,arcCost)
    } else if(s == maxLength) {
      val arcCost = leftOverProb + math.log(1-geometricStoppingProb) - math.log(chars.size);
        for(a <- chars.iterator) yield Arc(s,s,a,arcCost)
    } else Iterator.empty;
  }
}

