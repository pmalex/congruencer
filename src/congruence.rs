pub trait Congruence<P> {
    /// Returns true, if a partition of some algebraic object is a congruence.
    fn is_congruence(&self, partition: &P) -> bool;
}
