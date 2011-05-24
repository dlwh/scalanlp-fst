package scalanlp.newfst

/**
 * 
 * @author dlwh
 */
trait AutomatonTransformer[-CC,+CC2] {
  def apply(auto: CC):CC2;
}