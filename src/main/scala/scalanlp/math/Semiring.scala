package scalanlp.math

trait Semiring[T] {
  def plus(t1: T, t2: T):T ;
  def times(t1: T, t2: T): T;
  val zero : T
  val one: T
  // Not a real semiring operation, but we need it
  def closeTo(t1: T, t2: T): Boolean = t1 == t2;
}

trait WLDSemiring[T] extends Semiring[T] {
  /**
  * Return z^-1 times x
  */
  def leftDivide(z: T, x: T): T;
}

object Semiring {
  implicit val booleanSemiring = new Semiring[Boolean] {
    def plus(t1: Boolean, t2: Boolean) = t1 || t2;
    def times(t1:Boolean, t2: Boolean) = t1 && t2;
    val one = true;
    val zero = false;
  }
  
  implicit def integralIsSemiring[@specialized T:Integral] = new Semiring[T] {
    private val ops = implicitly[Integral[T]];
    import ops._;
    def plus(t1: T, t2: T) = t1 + t2;
    def times(t1: T, t2: T) = t1 * t2;
    val one = ops.one;
    val zero = ops.zero;
  }

  implicit def fractionalIsDivSemiring[@specialized T:Fractional] = new WLDSemiring[T] {
    private val ops = implicitly[Fractional[T]];
    import ops._;
    def plus(t1: T, t2: T) = t1 + t2;
    def times(t1: T, t2: T) = t1 * t2;
    def leftDivide(t1: T, t2: T) = t2 / t1;
    val one = ops.one;
    val zero = ops.zero;
  }

  /**
   * Provides access to the tropical algebra. The implicit is segregated because it conflicts with numericIsSemiring
   */
  object Tropical {
    implicit val doubleIsTropical:WLDSemiring[Double] = new WLDSemiring[Double] {
      def plus(t1: Double, t2: Double) = t1 max t2;
      def leftDivide(t1: Double, t2: Double) = t2 - t1;
      def times(t1: Double, t2: Double) = t1 + t2;
      val one = 0.0;
      val zero = Math.NEG_INF_DOUBLE;
    }
  }
  
  /**
   * Provides access to the tropical algebra. The implicit is segregated because it conflicts with numericIsSemiring
   */
  object LogSpace {
    implicit val doubleIsLogSpace:WLDSemiring[Double] = new WLDSemiring[Double] {
	    private def logSum(a : Double, b : Double) = {
	      import Math._;
	      if(a == NEG_INF_DOUBLE) b
	      else if (b == NEG_INF_DOUBLE) a
	      else if(a < b) b + log(1 + exp(a-b))
	      else a + log(1+exp(b-a));    
	    }
      def plus(t1: Double, t2: Double) = logSum(t1,t2);
      def leftDivide(t1: Double, t2: Double) = t2 - t1;
      def times(t1: Double, t2: Double) = t1 + t2;
      val one = 0.0;
      val zero = Math.NEG_INF_DOUBLE;
      override def closeTo(x: Double, y: Double) = Math.abs( (x-y)/x)  < 1E-6;
    }
  }
  
}
