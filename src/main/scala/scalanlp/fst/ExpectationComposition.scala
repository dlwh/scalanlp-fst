package scalanlp.fst;
import scalanlp.math._;

/**
* LogExpectedWeight
*/
final case class LogExpectedWeight(sign: Boolean, prob: Double, score: Double) {
  assert(!prob.isNaN);
  assert(prob != Math.POS_INF_DOUBLE);
  assert(!score.isNaN);
  def value = if(sign) Math.exp(score-prob) else -Math.exp(score-prob);
}

object LogExpectedWeight {
  def apply(p: Double, s: Double) = {
    new LogExpectedWeight(s >= 0, p, if(s>= 0) s else -s);
  }
}

object ExpectationComposition {
  implicit val logExpectedWeightIsSemiring:Semiring[LogExpectedWeight] = new Semiring[LogExpectedWeight] {
    import scalanlp.math.Semiring.LogSpace.logSum;
    import scalanlp.math.Semiring.LogSpace.doubleIsLogSpace.{zero=>logZero,one=>logOne,closure=>logClosure};
    // scores are in log log space
    val one = LogExpectedWeight(logOne,logZero);
    val zero = LogExpectedWeight(logZero,logZero);
    import Math._;

    private def signOfAdd(mx: Double, sx: Boolean, my: Double, sy: Boolean) = {
      if(mx > my) sx else sy;
    }

    private def signedAdd(mx: Double, sx: Boolean, my: Double, sy: Boolean) = {
      val x = if(mx > my) mx else my;
      val y = if(mx > my) my else mx;
      // stupid infinity. This is kind of bad code, but I don't care. It carries
      // its intent pretty well!
      val `y - x` = if( x == y) 0.0 else y - x;
      val score = {
        if(sy == sx)
          x + log(1 + exp(`y - x`));
        else 
          x + log(1 - exp(`y - x`));
      }
      score
    }


    def times(x: LogExpectedWeight, y: LogExpectedWeight) = {
      import Math._;
      // +- or -+ ==> negative
      // -- or ++ ==> positive
      val score = signedAdd(x.score + y.prob,x.sign,y.score + x.prob,y.sign);
      val sign  = signOfAdd(x.score + y.prob,x.sign,y.score + x.prob,y.sign);
      assert(!score.isNaN,"timesing " + x + y);
      LogExpectedWeight(sign,
                        x.prob + y.prob,
                        score
                       );
    }
    def plus(x: LogExpectedWeight, y: LogExpectedWeight) = {
      import Math._;
      val prob = x.prob + y.prob;
      val score = signedAdd(x.score,x.sign,y.score,y.sign);
      val sign  = signOfAdd(x.score,x.sign,y.score,y.sign);
      assert(!score.isNaN,"summing" + x + y);
      LogExpectedWeight(sign,prob,score);
    }
    def closure(x: LogExpectedWeight) = {
      val pc = logClosure(x.prob);
      LogExpectedWeight(x.sign,pc,pc + pc + x.score);
    }
  }

  /**
  * Composes when the left hand side is in logspace. See Li and Eisner (2009) 
  *First- and Second-Order Expectation Semirings 
  *with Applications to Minimum-Risk Training on Translation, table 1 and 3.
  *
  */
  def logSpaceExpectationCompose[S1,S2,In,Mid,Out](a: Transducer[Double,S1,In,Mid],
    b: Transducer[Double,S2,Mid,Out]) = {
    a.compose(b,LogExpectedWeight(_:Double,_:Double))(logExpectedWeightIsSemiring);
  }
}
