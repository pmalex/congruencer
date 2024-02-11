use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::partition::{self, Partition};

pub trait Congruence {
    /// Returns true, if a partition of some algebraic object is a congruence.
    fn is_congruence(&self, partition: &Partition) -> bool;

    #[inline]
    /// Создание решётки конгруэнций из всевозможных разбиений элементов некоторого алгебраического объекта.
    /// Простейшая (наивная) имплементация.
    fn new_congruence_set<'a>(&self, elements_names: &'a [&'a str]) -> Vec<Partition<'a>>
    where
        Self: Sync,
    {
        partition::new_partitions_set(elements_names)
            .into_par_iter()
            .filter(|partition| self.is_congruence(partition))
            .collect()
    }
}
