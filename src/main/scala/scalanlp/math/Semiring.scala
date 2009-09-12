package scalanlp.math

trait Semiring[T] {
  def plus(t1: T, t2: T):T ;
  def times(t1: T, t2: T): T;
  val zero : T
  val one: T
}

object Semiring {
  implicit val booleanSemiring = new Semiring[Boolean] {
    def plus(t1: Boolean, t2: Boolean) = t1 || t2;
    def times(t1:Boolean, t2: Boolean) = t1 && t2;
    val one = true;
    val zero = true;
  }
  
  implicit def numericIsSemiring[@specialized T:Numeric] = new Semiring[T] {
    private val ops = evidence[Numeric[T]];
    import ops._;
    def plus(t1: T, t2: T) = t1 + t2;
    def times(t1: T, t2: T) = t1 * t2;
    val one = ops.one;
    val zero = ops.zero;
  }
  
  /**
   * Provides access to the tropical algebra. The implicit is segregated because it conflicts with numericIsSemiring
   */
  object Tropical {
    implicit val doubleIsTropical:Semiring[Double] = new Semiring[Double] {
      def plus(t1: Double, t2: Double) = t1 max t2;
      def times(t1: Double, t2: Double) = t1 + t2;
      val one = 0.0;
      val zero = Math.NEG_INF_DOUBLE;
    }
  }
  
  /**
   * Provides access to the tropical algebra. The implicit is segregated because it conflicts with numericIsSemiring
   */
  object LogSpace {
    implicit val doubleIsLogSpace:Semiring[Double] = new Semiring[Double] {
      def plus(t1: Double, t2: Double) = Numerics.logSum(t1,t2);
      def times(t1: Double, t2: Double) = t1 + t2;
      val one = 0.0;
      val zero = Math.NEG_INF_DOUBLE;
    }
  }
  
}