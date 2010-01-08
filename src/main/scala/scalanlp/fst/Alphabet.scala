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

  implicit def tupleize[A:Alphabet,B:Alphabet] = new Alphabet[(A,B)] {
    val epsilon = (implicitly[Alphabet[A]].epsilon,implicitly[Alphabet[B]].epsilon);
    val sigma = (implicitly[Alphabet[A]].sigma,implicitly[Alphabet[B]].sigma);
    val rho = (implicitly[Alphabet[A]].rho,implicitly[Alphabet[B]].rho);
  }
}
