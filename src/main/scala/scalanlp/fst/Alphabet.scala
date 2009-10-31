package scalanlp.fst

abstract class Alphabet[T] {
  val epsilon: T;
  /**
  * Sigma represents any of the "normal" characters.
  */
  val sigma: T;

  /**
  * Rho will eventually mean "rest", but for now 
  * it's used as a placeholder in edit distance
  */
  val rho: T
}

object Alphabet {
  implicit val zeroEpsCharBet = new Alphabet[Char] {
    val epsilon = '\0';
    val sigma = '\1';
    val rho = '\2';
  }
}
