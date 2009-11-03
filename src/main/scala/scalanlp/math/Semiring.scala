package scalanlp.math

trait Semiring[T] {
  def plus(t1: T, t2: T):T ;
  def times(t1: T, t2: T): T;
  val zero : T
  val one: T
  // Not a real semiring operation, but we need it
  def closeTo(t1: T, t2: T): Boolean = t1 == t2;
  /**
  * Also not a guaranteed property, but we need it for most ops.
  * Should be equiv to \sum_{k=1}^\infty t^k.
  */
  def closure(t: T):T
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
    def closure(t: Boolean) = t;
    val one = true;
    val zero = false;
  }

  object Probability {
    implicit val semiring = new ProbSemiring;
    class ProbSemiring extends WLDSemiring[Double] {
      def plus(t1: Double, t2: Double) = t1 + t2;
      def times(t1: Double, t2: Double) = t1 * t2;
      def leftDivide(t1: Double, t2: Double) = t2 / t1;
      def closure(t: Double) = {
        if(t < 1 && t > 0) 1 / (1-t);
        else if(t == 0) 0.0
        else error("Closure arg must be in [0,1), not "  +t);
      }
      val one = 1.0;
      val zero = 0.0;
    }
  }
  
  implicit def fractionalIsDivSemiring[@specialized T:Fractional] = new WLDSemiring[T] {
    private val ops = implicitly[Fractional[T]];
    import ops._;
    def plus(t1: T, t2: T) = t1 + t2;
    def times(t1: T, t2: T) = t1 * t2;
    def leftDivide(t1: T, t2: T) = t2 / t1;
    def closure(t: T) = error("Sorry, not implemented.");
    val one = ops.one;
    val zero = ops.zero;
  }

  /**
   * Provides access to the tropical algebra. The implicit is segregated because it conflicts with numericIsSemiring
   */
  object Tropical {
    implicit val doubleIsTropical:WLDSemiring[Double] = new WLDSemiring[Double] {
      def plus(t1: Double, t2: Double) = t1 min t2;
      def leftDivide(t1: Double, t2: Double) = t2 - t1;
      def times(t1: Double, t2: Double) = t1 + t2;
      def closure(t: Double) = if(t >= 0) t else Math.NEG_INF_DOUBLE;
      val one = 0.0;
      val zero = Math.POS_INF_DOUBLE;
    }
  }
  
  /**
   * Provides access to the logspace algebra. The implicit is segregated because it conflicts with numericIsSemiring
   */
  object LogSpace {
    def logSum(a : Double, b : Double) = {
      import Math._;
      if(a == NEG_INF_DOUBLE) b
      else if (b == NEG_INF_DOUBLE) a
      else if(a < b) b + java.lang.Math.log(1+exp(a-b))
        else a + java.lang.Math.log(1+exp(b-a));    
    }
    implicit val doubleIsLogSpace:WLDSemiring[Double] = new WLDSemiring[Double] {
      def plus(t1: Double, t2: Double) = logSum(t1,t2);
      def leftDivide(t1: Double, t2: Double) = t2 - t1;
      def times(t1: Double, t2: Double) = t1 + t2;
      val one = 0.0;
      val zero = Math.NEG_INF_DOUBLE;
      override def closeTo(x: Double, y: Double) = Math.abs( (x-y)/x)  < 1E-6;
      /**
      * p =&gt; 1/(1-p)
      * becomes
      * t =&lt; log(1/(1-exp(t))) = -log(1-exp(t));
      */ 
      def closure(t: Double) = {
        -Math.log(1 - Math.exp(t));
      }
    }
  }
  
}
