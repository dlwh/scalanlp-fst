package scalanlp.fst

/**
* An arc represents an edge from a state to another state, with input label in, output label out, and weight W.
*/
final case class Arc[+W, +State, +In, +Out](from: State,
    to: State, in: In, out: Out, weight: W);
