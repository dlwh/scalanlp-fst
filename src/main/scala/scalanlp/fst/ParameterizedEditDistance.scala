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



import scalanlp.math.Semiring.LogSpace.doubleIsLogSpace;
import scalala.library.Numerics._;

/**
 * Levhenstein transducer over sum of alignments (not viterbi
 * alignment) with the given parameters;
 *
 * The cost function must handle Epsilon.
 *
 * When composed with an input transducer that is markovian, will
 * produce a markovian marginal.
 *
 * @author dlwh
 */
class ParameterizedEditDistance(costFunction: (Char,Char)=>Double, alphabet: Set[Char]) extends Transducer[Double,Int,Char,Char] {
  import Transducer._;

  def totalChars = alphabet.size;
  private def inAlpha = implicitly[Alphabet[Char]];

  /**
   * total costs for each input character
   */
  private val inputNormalizer: Map[Char,Double] = {
    import scala.collection.breakOut
    alphabet.map { inCh => 
      val allCosts = alphabet.iterator.map( outCh => costFunction(inCh,outCh)).toSeq;
      val totalCost = logSum(allCosts);
      (inCh,totalCost);
    } (breakOut);
  }


  private val Eps = inAlpha.epsilon;
  private val Sigma = inAlpha.sigma;
  private val allChars = alphabet + Eps;

  val initialStateWeights = Map( 0 -> 0.0);
  
  // 1 - all insertions possible 
  private val theFinalWeight = {
    val edges = edgesMatching(0,(Eps,Sigma)).map(_.weight).toSeq;
    val totalInsertMass = logSum(edges);
    math.log(1- math.exp(totalInsertMass));
  }
  def finalWeight(s: Int) = theFinalWeight;

  override def allEdges:Seq[Arc] = (edgesMatching(0,(inAlpha.sigma,inAlpha.sigma))).toSeq;

  def edgesMatching(s: Int, ab: (Char, Char)) = {
    val (a,b) = ab;

    if(a == Sigma) {
      for {
        a <- allChars.iterator
      } yield {
        Arc(0,0,(a,b), cost);
      }
    } else if(b == Sigma) {
      for(b <- allChars.iterator) yield
        Arc(0,0,(a,b),costFunction(a,b));
    } else if(a == b && b == Eps) {
      Iterator.empty;
    } else { 
      Iterator.single(Arc(0,0,(a,b),costFunction(a,b)));
    }
  }
}
