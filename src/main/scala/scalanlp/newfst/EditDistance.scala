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

import scalanlp.math.Semiring.LogSpace._;


/**
 * Levhenstein transducer over sum of alignments (not viterbi
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
class EditDistance(subRatio: Double, insRatio: Double, alpha: Set[Char])
  extends Transducer[Double,Int,Char,Char] with DenseAutomaton[Double,(Char,Char)] with SelectingAutomaton[Double,Int,(Char,Char)] with AutomatonLike[Double,Int,(Char,Char),EditDistance] {

  require( subRatio < 0);
  require( insRatio < 0);

  val totalChars = alpha.size;
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


  private val inAlpha = implicitly[Alphabet[Char]];
  private val Eps = inAlpha.epsilon;
  private val allChars = alpha + Eps;

  val initialStateWeights = Map( 0 -> 0.0);

  def finalWeight(s: Int) = math.log(1 - math.exp(math.log(totalChars) + insCost));

  override def edges = (edgesFrom(0)).toIndexedSeq;

  def edgesFrom(s: Int) = {
    for {
      a <- allChars.iterator;
      b <- allChars.iterator;
      cost = costOf(a,b)
      if cost != Double.NegativeInfinity
    } yield {
      Arc(0,0,(a,b), cost);
    }
  }


  def selectEdges(from: Int, label: (Char,Char)) = {
    if( allChars(label._1) && allChars(label._2) )
      Iterator.single(Arc(0,0,label,costOf(label._1,label._2)))
    else Iterator.empty
  }

  /*
  def edgesMatching(s: Int, ab: (Char,Char)) = if(s != 0) Iterator.empty else {
    val (a,b) = ab;
    if(a == Sigma && b == Sigma) {
      for {
        a <- allChars.iterator;
        b <- allChars.iterator;
        cost = costOf(a,b)
        if cost != Double.NegativeInfinity
      } yield {
        Arc(0,0,(a,b), cost);
      }
    } else if(a == Sigma) {
      if(b == Eps) {
        for(a <- alpha.iterator)
          yield Arc(0,0,(a,Eps),costOf(a,Eps));
      } else {
        for(a <- allChars.iterator)
          yield Arc(0,0,(a,b),costOf(a,b));
      }
    } else if(b == Sigma) {
      if(a == Eps) {
        for(b <- alpha.iterator)
          yield Arc(0,0,(Eps,b),costOf(Eps,b));
      } else {
        for(b <- allChars.iterator)
          yield Arc(0,0,(a,b),costOf(a,b));
      }
    } else if(a == b && b == Eps) {
      Iterator.empty;
    } else {
      Iterator.single(Arc(0,0,(a,b),costOf(a,b)));
    }

  }
  */


  private def costOf(a: Char, b: Char) = {
    if(a == Eps && b == Eps) Double.NegativeInfinity
    else if(a == Eps || b == Eps) {
      insCost
    } else if(a==b) matchCost
    else subCost;
  }
}

object EditDistance {
  implicit def tSigmaMatcher = new ArcMatcher[EditDistance,Double,Int,(Char,Char),(Char,Sigma.type)] {
    def arcsMatching(cc: EditDistance, s: Int, mt: (Char, Sigma.type)) = {
      import cc._;
      val a = mt._1
      if (allChars(a)) {
        for {
          b <- allChars.iterator;
          cost = costOf(a,b)
          if cost != Double.NegativeInfinity
        } yield {
          Arc(0,0,(a,b), cost);
        }
      } else Iterator.empty;
    }
  }

  implicit def sigmaTMatcher = new ArcMatcher[EditDistance,Double,Int,(Char,Char),(Sigma.type, Char)] {
    def arcsMatching(cc: EditDistance, s: Int, mt: (Sigma.type, Char)) = {
      import cc._;
      val b = mt._2
      if (allChars(b)) {
        for {
          a <- allChars.iterator;
          cost = costOf(a,b)
          if cost != Double.NegativeInfinity
        } yield {
          Arc(0,0,(a,b), cost);
        }
      } else Iterator.empty;
    }
  }
}