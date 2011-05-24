package scalanlp.newfst

/*
 Copyright 2011 David Hall

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
 * An arc represents an edge from a state to another state, with input label in, output label out, and weight W.
 * @author dlwh
 */
case class Arc[@specialized(Double) W, State,@specialized(Char) T](source: State, sink: State, label: T, weight: W);

