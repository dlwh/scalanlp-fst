package scalanlp.fst.fast
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

trait BigramModelFactory[T] { this: AutomatonFactory[T]  =>

  class BigramModel(init: T) extends Automaton {
    override def finalWeight(s: Int) = ring.one

    lazy val finalWeights = Array.fill(numStates) {ring.one}

    def initialWeight = ring.one

    def initialState = index(init);

    def numStates = index.size;

    val theArcs = encoder.fillSparseArray(mkSparseVector(numStates));
    for(i <- 0 until index.size if i != epsilonIndex) {
      theArcs.getOrElseUpdate(i)(i) = ring.one;
    }

    def arcsFrom(s: Int, ch: Int) = {
      theArcs(ch);
    }

    def arcsFrom(s: Int) = {
      theArcs;
    }

  }
}

