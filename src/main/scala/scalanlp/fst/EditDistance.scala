package scalanlp.fst;

import scalanlp.math.Semiring.LogSpace._;

/**
* Levhenstein transducer over sum of alignments (not viterbi
* alignment) with the given parameters, which must be less than 0.
*/
class EditDistance(sub: Double, del: Double, alphabet: Set[Char]) 
    extends Transducer[Double,Unit,Char,Char]()(doubleIsLogSpace,implicitly[Alphabet[Char]],implicitly[Alphabet[Char]]) {
  import Transducer._;
  require( sub < 0);
  require( del < 0);

  val initialStateWeights = Map( () -> 0.0);

  def finalWeight(s: Unit) = 0.0;
  val epsilon = inAlpha.epsilon;
  val alphabetSeq = alphabet.toSeq;

  override val allEdges = {
    val subs = for(a <- alphabetSeq;
        b <- alphabetSeq)
      yield Arc((),(),a,b,if(a != b) sub else 0.0);
    val dels = for(a <- alphabet iterator)
      yield Arc((),(),a,`epsilon`,del);
    val dels2 = for(a <- alphabet iterator)
      yield Arc((),(),`epsilon`,a,del);

    subs ++ dels ++ dels2;
  }

  def edgesMatching(s: Unit, a: Char, b: Char) = {
    if(a == inAlpha.sigma && b == outAlpha.sigma) {
      allEdges
    } else if(a == inAlpha.sigma) {
      if(b == outAlpha.epsilon) {
        for(a <- alphabetSeq)
          yield Arc((),(),a,a,del);
      } else {
        for(a <- alphabetSeq)
          yield Arc((),(),a,b,if(a != b) sub else 0.0);
      }
    } else if(b == outAlpha.sigma) {
      if(a == outAlpha.epsilon) {
        for(b <- alphabetSeq)
          yield Arc((),(),a,b,del);
      } else {
        for(b <- alphabetSeq)
          yield Arc((),(),a,b,if(a != b) sub else 0.0);
      }
    } else {
      Seq(Arc((),(),a,b,if(a != b) sub else 0.0));
    }
  }
}

