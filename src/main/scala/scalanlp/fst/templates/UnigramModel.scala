package scalanlp.fst
package templates

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
import scalala.collection.sparse.DefaultArrayValue

class UnigramModel[W:Semiring:ClassManifest:DefaultArrayValue,T:Alphabet](init: T, chars: Set[T]) extends Automaton[W,Boolean,T] {
  protected val alphabet = implicitly[Alphabet[T]]
  protected val ring = implicitly[Semiring[W]]
  val initialStateWeights = Map( true -> ring.one);

  def finalWeight(s: Boolean) = if(s) ring.zero else ring.one

  def edgesFrom(s: Boolean) = {
    if(!s) Iterator.empty
    else{
      val nonEpsArcs = for(a <- chars.iterator)
      yield Arc(s,s,a,ring.one)
      val epsArc = Iterator.single(Arc(s,false,alphabet.epsilon,ring.one))
      nonEpsArcs ++ epsArc;
    }
  }
}
