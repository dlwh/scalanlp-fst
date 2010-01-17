package scalanlp.fst

/**
* An arc represents an edge from a state to another state, with input label in, output label out, and weight W.
*/
final case class Arc[@specialized("Double") +W, +State, @specialized("Char") +T](from: State, to: State, label: T, weight: W);