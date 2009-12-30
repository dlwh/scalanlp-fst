package scalanlp.fst;

import scalanlp.math.Semiring.LogSpace._;
import scala.annotation.switch;

/**
 * Levhenstein transducer over sum of alignments (not viterbi
 * alignment) with the given parameters, which must be less than 0.
 *
 * A match is assumed to be 0.0, though these will be rescaled to ensure
 * that the edit distance encodes a (log space) probability distribution, or at
 * least that conditioned on either the input or the output, there is a distribution.
 * logDecay (&lt;=0.0) adds a preference for the length of the alignments.
 * That is, the scores will be shifted by some constant. rhoSize represents
 * phantom characters that are implicitly represented as a single other character (Alphabet.rho)
 *
 * @author dlwh
 */
class EditDistance( subRatio: Double, insRatio: Double, alphabet: Set[Char], rhoSize: Int = 0)
        extends Transducer[Double,Int,Char,Char]()(doubleIsLogSpace,implicitly[Alphabet[Char]],implicitly[Alphabet[Char]]) {
  import Transducer._;
  require( subRatio < 0);
  require( insRatio < 0);
  require( rhoSize >= 0);

  val totalChars = alphabet.size + rhoSize;
  /**
   * Costs for each kind of parameter.
   */
  val (insCost,subCost,matchCost) = {
    val n = totalChars;
    import Math.{exp,log};
    // we want to make edges out be markovian.
    // for any input label (resp. output label), there is 1 match, n-1 subs, and and 1 deletion
    // but we also have to accept an insertion of any kind, so there are n of those.
    //  c * ((n - 1 ) exp(sub) + 1 * exp(0) + (n+1) exp(del)) = 1.0
    // log c + log ((n -1 ) exp(sub) + 1 * exp(0) + (n+1) exp(del)) = 0.0
    val logC = - log( (n-1) * exp(subRatio) + 1 + (n+1) * exp(insRatio))
    (insRatio + logC,subRatio + logC,logC)
  }


  def rhoInsCost = if(rhoSize == 0) Double.NegativeInfinity else logRhoSize + insCost;
  // |alpha|^2 - |alpha| subs will be covered by "real" characters
  // we need (|alpha| + rhoSize)^2 - |alpha| - rhoSize total substitutions
  //
  // 2|alpha|rho + rhoSize^2 - rhoSize need to be covered.
  // the left half will be covered by 2 |alpha| rho "real" substitutions involving 
  // a real char and one rho.
  // so we need rhoSize^2 - rhoSize other substitutions
  def rhoSubCost = {
    val size = Math.log( (rhoSize * rhoSize - rhoSize));
    if(rhoSize == 0) Double.NegativeInfinity else size + subCost;
  }
  def rhoMatchCost = if(rhoSize == 0) Double.NegativeInfinity else logRhoSize + matchCost;

  private val logRhoSize = Math.log(rhoSize.toDouble);
  private val Eps = inAlpha.epsilon;
  private val Rho = inAlpha.rho;
  private val Sigma = inAlpha.sigma;
  // only include Rho if we need it.
  private val alphaAndRho = if(rhoSize == 0) alphabet else alphabet + Rho;
  private val allChars = alphaAndRho + Eps;

  val rhoSubMatchCost = ring.plus(rhoSubCost,rhoMatchCost);

  val initialStateWeights = Map( 0 -> 0.0);

  def finalWeight(s: Int) = Math.log(1 - Math.exp(Math.log(totalChars) + insCost));

  override def allEdges:Seq[Arc] = (edgesMatching(0,inAlpha.sigma,outAlpha.sigma)).toSeq;

  def edgesMatching(s: Int, a: Char, b: Char) = if(s != 0) Iterator.empty else {
    if(a == Sigma && b == Sigma) {
      for {
        a <- allChars.iterator;
        b <- allChars.iterator;
        if a != Eps || b != Eps
        cost = costOf(a,b)
        if cost != Double.NegativeInfinity
      } yield {
        Arc(0,0,a,b, cost);
      }
    } else if(a == Sigma) {
      if(b == Eps) {
        for(a <- alphaAndRho.iterator)
          yield Arc(0,0,a,Eps,costOf(a,Eps));
      } else {
        for(a <- allChars.iterator)
          yield Arc(0,0,a,b,costOf(a,b));
      }
    } else if(b == Sigma) {
      if(a == Eps) {
        for(b <- alphaAndRho.iterator)
          yield Arc(0,0,Eps,b,costOf(Eps,b));
      } else {
        for(b <- allChars.iterator)
          yield Arc(0,0,a,b,costOf(a,b));
      }
    } else if(a == b && b == Eps) {
      Iterator.empty;
    } else {
      Iterator.single(Arc(0,0,a,b,costOf(a,b)));
    }

  }

  private val RhoRho = (Rho << 2)|Rho;
  private val RhoEps = (Rho << 2)|Eps;
  private val EpsRho = (Eps << 2)|Rho;
  private val EpsEps = (Rho << 2)|Rho;

  private def costOf(a: Char, b: Char) = {
    val ab = (a<<2)|b;
    (ab: /*@switch*/ Int) match {
      case RhoRho => rhoSubMatchCost;
      case RhoEps => rhoInsCost
      case EpsRho => rhoInsCost
      case EpsEps => error("Shouldn't be here")
      case _ =>
        if(a == Eps || b == Eps) {
          insCost
        } else if (a == Rho || b == Rho) {
          logRhoSize + subCost;
        } else if(a==b)
          matchCost
        else subCost;
    }
  }
}

