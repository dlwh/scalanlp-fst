package scalanlp.newfst
package templates

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

import scalanlp.math.Semiring
import scalala.collection.sparse.DefaultArrayValue

class PositionalUnigramModel[W:Semiring:ClassManifest:DefaultArrayValue,T:Alphabet](init: T, chars: Set[T], maxLength: Int) extends Automaton[W,Int,T] {
  protected val alphabet = implicitly[Alphabet[T]]
  protected val ring = implicitly[Semiring[W]]
  val initialStateWeights = Map( 0 -> ring.one)

  def finalWeight(s: Int) = if(s == -1) ring.one else ring.zero

  private def next(s: Int) = (s+1) min (maxLength-1)

  def edgesFrom(s: Int) = {
    val pos = next(s)
    if(s < 0 || s >= maxLength) {
      Iterator.empty
    } else {
      val realArcs = for(a <- chars.iterator)
        yield Arc(s,pos,a,ring.one)
      val epsArc = Arc(s,-1,alphabet.epsilon,ring.one)
      Iterator.single(epsArc) ++ realArcs
    }
  }
}
