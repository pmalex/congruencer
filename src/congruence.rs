use rayon::prelude::{IntoParallelIterator, ParallelIterator};

use crate::partition::{self, Partition};

pub trait Congruence {
    /// Returns true, if a partition of some algebraic object is a congruence.
    fn is_congruence(&self, partition: &Partition) -> bool;

    /// Returns the number of elements in algebraic structure.
    fn size(&self) -> usize;

    #[inline]
    fn new_congruence_set(&self) -> Vec<Partition>
    where
        Self: Sync,
    {
        partition::new_partitions_set(self.size())
            .into_par_iter()
            .filter(|partition| self.is_congruence(partition))
            .collect()
    }
}
