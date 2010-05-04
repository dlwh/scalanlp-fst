package scalanlp.fst

abstract class Alphabet[@specialized(Char) T] {
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

  def matches(a: T, b:T):Boolean
}

object Alphabet {
  implicit val zeroEpsCharBet = new Alphabet[Char] {
    val epsilon = '\0';
    val sigma = '\1';
    val rho = '\2';
    def matches(a: Char, b:Char) = (a == sigma) || (b == sigma) || a == b;
  }

  implicit def tupleize[A:Alphabet,B:Alphabet] = new Alphabet[(A,B)] {
    val epsilon = (implicitly[Alphabet[A]].epsilon,implicitly[Alphabet[B]].epsilon);
    val sigma = (implicitly[Alphabet[A]].sigma,implicitly[Alphabet[B]].sigma);
    val rho = (implicitly[Alphabet[A]].rho,implicitly[Alphabet[B]].rho);
    private val aSig = sigma._1;
    private val bSig = sigma._2;
    def matches(a: (A,B), b: (A,B)) = (
      (a._1 == aSig || b._1 == aSig || b._1 == a._1) && (a._2 == bSig || b._2 == bSig || b._2 == a._2)
    );
  }
}
