package scalanlp.fst.util

import scala.collection.mutable.Map

/**
 * Brokers mutable Maps based on a set of types. can be much more efficient.
 * @author dlwh
 */
trait MapMaker[-Container,K,V] {
  def mkMap(container: Container):Map[K,V]
}

trait LowPriorityMapMaker {
  implicit def basicMapMaker[C,K,V] = new MapMaker[C,K,V] {
    def mkMap(cc: C) = new collection.mutable.HashMap[K,V];
  }
}

object MapMaker extends LowPriorityMapMaker;
