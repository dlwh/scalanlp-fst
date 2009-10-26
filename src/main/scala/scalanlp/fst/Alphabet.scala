package scalanlp.fst

abstract class Alphabet[T] {
  val epsilon: T;
  /**
  * Sigma represents any of the "normal" characters.
  */
  val sigma: T;
}

object Alphabet {
  implicit val zeroEpsCharBet = new Alphabet[Char] {
    val epsilon = '\0';
    val sigma = '\1';
  }
}
