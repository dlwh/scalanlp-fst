package scalanlp.fst
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
* An alphabet contains special characters that
* have special meaning to an automaton.
*
* @author dlwh
*/
abstract class Alphabet[@specialized(Char) T] {
  /**
  * epsilon represents the null character.
  */
  val epsilon: T;
  /**
  * Sigma represents any of the "normal" non-null characters.
  */
  val sigma: T;

  /**
  * Returns true if a character matches another. Usually this is just equals, but 
  * it should handle sigma as well.
  */ 
  def matches(a: T, b:T):Boolean
}

object Alphabet {
  def apply[T:Alphabet] = implicitly[Alphabet[T]];
  implicit object zeroEpsCharBet extends Alphabet[Char] {
    val epsilon = '\0';
    val sigma = '\1';
    def matches(a: Char, b:Char) = (a == sigma) || (b == sigma) || a == b;
  }

  implicit def tupleize[A:Alphabet,B:Alphabet] = new TupleAlphabet[A,B];


  final class TupleAlphabet[A:Alphabet,B:Alphabet] extends Alphabet[(A,B)] {
    val epsilon = (implicitly[Alphabet[A]].epsilon,implicitly[Alphabet[B]].epsilon);
    val sigma = (implicitly[Alphabet[A]].sigma,implicitly[Alphabet[B]].sigma);
    private val aSigma = sigma._1;
    private val bSigma = sigma._2;
    def matches(a: (A,B), b: (A,B)) = (
      (a._1 == aSigma || b._1 == aSigma || b._1 == a._1) && (a._2 == bSigma || b._2 == bSigma || b._2 == a._2)
    );
  }
}
