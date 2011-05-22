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
import scalanlp.fst.{Arc, Automaton, Alphabet}
import scalala.collection.sparse.DefaultArrayValue

class BigramModel[W:Semiring:ClassManifest:DefaultArrayValue,T:Alphabet](init: T, chars: Set[T]) extends Automaton[W,T,T] {
  val initialStateWeights = Map( init -> ring.one);

  def finalWeight(s: T) = if (s == alphabet.epsilon) ring.one else ring.zero

  def edgesMatching(s: T, a: T) = {
    if(s == alphabet.epsilon) Iterator.empty
    else if(a == alphabet.sigma) {
      val realArcs = for(a <- chars.iterator)
      yield Arc(s,a,a,ring.one)
      val epsArc = Arc(s,alphabet.epsilon,alphabet.epsilon,ring.one);
      if(s != init)
        Iterator.single(epsArc) ++ realArcs;
      else
        realArcs
    } else {
      Iterator.single(Arc(s,a,a,ring.one))
    }

  }
}
