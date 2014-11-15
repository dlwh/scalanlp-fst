package scalanlp.fst
package templates

import breeze.math.Semiring
import breeze.storage.Zero

import scala.reflect.ClassTag

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


class BigramModel[W:Semiring:ClassTag:Zero,T:Alphabet](init: T, chars: Set[T]) extends Automaton[W,T,T] {
  protected val alphabet = implicitly[Alphabet[T]]
  protected val ring = implicitly[Semiring[W]]
  val initialStateWeights = Map( init -> ring.one)

  def finalWeight(s: T) = if (s == alphabet.epsilon) ring.one else ring.zero

  def edgesFrom(s: T) = {
    val realArcs = for(a <- chars.iterator) yield Arc(s,a,a,ring.one)
    val epsArc = Arc(s,alphabet.epsilon,alphabet.epsilon,ring.one)

    if(s == alphabet.epsilon) Iterator.empty
    else if(s != init)
      Iterator.single(epsArc) ++ realArcs
    else
      realArcs
  }
}
