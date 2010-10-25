package scalanlp.fst;

import scalanlp.math.Semiring

/**
 * The one best semiirng will reweight an automaton to extract the one-best path through
 * it.
 *
 * @author dlwh
 */
class OneBestSemiring[T,W](implicit wOrd: Ordering[W], wRing: Semiring[W], alphabet: Alphabet[T]) {
  case class Elem(str: Seq[T], weight: W);
  import wOrd.mkOrderingOps;
  implicit val ring : Semiring[Elem] = new Semiring[Elem] {
    def plus(a: Elem, b: Elem) = if(a.weight < b.weight) b else a;
    def times(a: Elem, b: Elem) = if(a == zero || b == zero) zero else Elem(a.str ++ b.str,wRing.times(a.weight,b.weight));
    def closure(a: Elem) = one;
    override def closeTo(a: Elem, b: Elem) = wRing.closeTo(a.weight,b.weight);
    val one = Elem(Seq.empty,wRing.one);
    val zero = Elem(Seq.empty,wRing.zero);
  }

  def promote[S](a: Arc[W,S,T]) = {
    if(a.label == alphabet.epsilon)
      Elem(Seq.empty,a.weight);
    else Elem(Seq(a.label),a.weight);
  }

  def promoteOnlyWeight(w: W) = Elem(Seq.empty,w);

}
