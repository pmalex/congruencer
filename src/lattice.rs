use rayon::{
    iter::{IndexedParallelIterator, IntoParallelRefMutIterator},
    prelude::{IntoParallelIterator, ParallelIterator},
};
use std::sync::atomic::{AtomicBool, Ordering};

use crate::magma::Magma;

pub struct Lattice {
    sup: Magma,
    inf: Magma,
}

impl Lattice {
    /// Проверка того, что на переданном объекте удовлетворяются элементарные решёточные тождества.
    pub fn is_lattice(&self) -> bool {
        let Self { sup, inf, .. } = self;

        let ordering = Ordering::Relaxed;

        let result = AtomicBool::new(true);

        rayon::scope(|s| {
            // Supremum group.
            s.spawn(|_| {
                if result.load(ordering) && !sup.is_idempotent() {
                    result.store(false, ordering);
                }
            });
            s.spawn(|_| {
                if result.load(ordering) && !sup.is_commutative() {
                    result.store(false, ordering);
                }
            });
            s.spawn(|_| {
                if result.load(ordering) && !sup.is_associative() {
                    result.store(false, ordering);
                }
            });

            // Infimum group.
            s.spawn(|_| {
                if result.load(ordering) && !inf.is_idempotent() {
                    result.store(false, ordering);
                }
            });
            s.spawn(|_| {
                if result.load(ordering) && !inf.is_commutative() {
                    result.store(false, ordering);
                }
            });
            s.spawn(|_| {
                if result.load(ordering) && !inf.is_associative() {
                    result.store(false, ordering);
                }
            });

            s.spawn(|_| {
                if result.load(ordering) && !self.is_absorbing() {
                    result.store(false, ordering);
                }
            });
        });

        result.load(ordering)
    }

    pub fn is_absorbing(&self) -> bool {
        let Self { sup, inf, .. } = self;

        let number_of_elements = sup.set_size;

        (0..number_of_elements * number_of_elements)
            .into_par_iter()
            .all(|index_2d| {
                let a = index_2d / number_of_elements;
                let b = index_2d % number_of_elements;

                inf.call(a, sup.call(a, b)) == a && sup.call(a, inf.call(a, b)) == a
            })
    }

    /// Возвращает `true`, если решётка удовлетворяет тождеству модулярности:
    /// ```text
    /// x ∧ (y ∨ z) = x ∧ ((y ∧ (x ∨ z)) ∨ z).
    /// ```
    pub fn is_modular(&self) -> bool {
        let Self { sup, inf } = self;

        let number_of_elements = sup.set_size;
        let number_of_elements_squared = number_of_elements * number_of_elements;

        (0..number_of_elements_squared * number_of_elements)
            .into_par_iter()
            .all(|index_3d| {
                let z = index_3d / number_of_elements_squared;

                let index_2d = index_3d % number_of_elements_squared;

                let x = index_2d / number_of_elements;
                let y = index_2d % number_of_elements;

                // x ∧ (y ∨ z) = x ∧ ((y ∧ (x ∨ z)) ∨ z)
                inf.call(x, sup.call(y, z)) == inf.call(x, sup.call(inf.call(y, sup.call(x, z)), z))
            })
    }

    /// Возвращает `true`, если решётка удовлетворяет тождеству дистрибутивности.
    pub fn is_distributive(&self) -> bool {
        let Self { sup, inf, .. } = self;

        let number_of_elements = sup.set_size;
        let number_of_elements_squared = number_of_elements * number_of_elements;

        (0..number_of_elements_squared * number_of_elements)
            .into_par_iter()
            .all(|index_3d| {
                let z = index_3d / number_of_elements_squared;

                let index_2d = index_3d % number_of_elements_squared;

                let x = index_2d / number_of_elements;
                let y = index_2d % number_of_elements;

                // Checking that x ∧ (y ∨ z) = (x ∧ z) ∨ (x ∧ y)
                //           and x ∨ (y ∧ z) = (x ∨ z) ∧ (x ∨ y).
                inf.call(x, sup.call(y, z)) == sup.call(inf.call(x, z), inf.call(x, y))
                    && sup.call(x, inf.call(y, z)) == inf.call(sup.call(x, z), sup.call(x, y))
            })
    }
}

impl<P> From<&[P]> for Lattice
where
    P: PartialOrd + Sync,
{
    /// Конструирует решётку из частично упорядоченного множества.
    fn from(poset: &[P]) -> Self {
        let number_of_elements = poset.len();

        let (sup, inf) = rayon::join(
            || {
                let mut sup = Magma::zero(poset.len());

                sup.cayley_table
                    .par_iter_mut()
                    .enumerate()
                    .for_each(|(absolute_index, result_index)| {
                        // Here we calculate an indexes that lie inside the 0..number_of_elements range.
                        let element_a_index = absolute_index / number_of_elements; // The row number in Cayley table.
                        let element_b_index = absolute_index % number_of_elements; // The column number in Cayley table.

                        *result_index = if element_a_index == element_b_index {
                            // sup(a, a) = a
                            element_a_index
                        } else if poset[element_a_index] <= poset[element_b_index] {
                            // sup(a, b) = b for a <= b
                            element_b_index
                        } else if poset[element_b_index] <= poset[element_a_index] {
                            // sup(a, b) = a for b <= a
                            element_a_index
                        } else {
                            // The a and b are incomparable.
                            (0..number_of_elements)
                                .into_par_iter()
                                .fold(
                                    || None,
                                    |sup: Option<usize>, poset_element_index| {
                                        if let Some(sup_index) = sup {
                                            if poset[poset_element_index] <= poset[sup_index]
                                                && poset[element_a_index] <= poset[poset_element_index]
                                                && poset[element_b_index] <= poset[poset_element_index]
                                            {
                                                Some(poset_element_index)
                                            } else {
                                                Some(sup_index)
                                            }
                                        } else if poset[element_a_index] <= poset[poset_element_index]
                                            && poset[element_b_index] <= poset[poset_element_index]
                                        {
                                            Some(poset_element_index)
                                        } else {
                                            None
                                        }
                                    },
                                )
                                .reduce(
                                    || None,
                                    |absolute_sup_index: Option<usize>,
                                    partial_sup_index: Option<usize>| {
                                        if let Some(absolute_sup_index) = absolute_sup_index {
                                            if let Some(partial_sup_index) = partial_sup_index {
                                                if poset[absolute_sup_index] <= poset[partial_sup_index] {
                                                    Some(absolute_sup_index)
                                                } else if poset[partial_sup_index]
                                                    <= poset[absolute_sup_index]
                                                {
                                                    Some(partial_sup_index)
                                                } else {
                                                    None
                                                }
                                            } else {
                                                Some(absolute_sup_index)
                                            }
                                        } else {
                                            partial_sup_index
                                        }
                                    },
                                )
                                .expect("The poset is not an upper semilattice.")
                                }
                    });

                sup
            },
            || {
                let mut inf = Magma::zero(poset.len());

                inf.cayley_table
                    .par_iter_mut()
                    .enumerate()
                    .for_each(|(absolute_index, result_index)| {
                        // Here we calculate an indexes that lie inside the 0..number_of_elements range.
                        let element_a_index = absolute_index / number_of_elements; // The row number in Cayley table.
                        let element_b_index = absolute_index % number_of_elements; // The column number in Cayley table.

                        *result_index = if element_a_index == element_b_index {
                            // inf(a, a) = a
                            element_a_index
                        } else if poset[element_a_index] >= poset[element_b_index] {
                            // inf(a, b) = b for a >= b
                            element_b_index
                        } else if poset[element_b_index] >= poset[element_a_index] {
                            // inf(a, b) = a for b >= a
                            element_a_index
                        } else {
                            // The a and b are incomparable.
                            (0..number_of_elements)
                                .into_par_iter()
                                .fold(
                                    || None,
                                    |inf: Option<usize>, poset_element_index| {
                                        if let Some(inf_index) = inf {
                                            if poset[poset_element_index] >= poset[inf_index]
                                                && poset[element_a_index] >= poset[poset_element_index]
                                                && poset[element_b_index] >= poset[poset_element_index]
                                            {
                                                Some(poset_element_index)
                                            } else {
                                                Some(inf_index)
                                            }
                                        } else if poset[element_a_index] >= poset[poset_element_index]
                                            && poset[element_b_index] >= poset[poset_element_index]
                                        {
                                            Some(poset_element_index)
                                        } else {
                                            None
                                        }
                                    },
                                )
                                .reduce(
                                    || None,
                                    |absolute_inf_index: Option<usize>,
                                    partial_inf_index: Option<usize>| {
                                        if let Some(absolute_inf_index) = absolute_inf_index {
                                            if let Some(partial_inf_index) = partial_inf_index {
                                                if poset[absolute_inf_index] >= poset[partial_inf_index] {
                                                    Some(absolute_inf_index)
                                                } else if poset[partial_inf_index]
                                                    >= poset[absolute_inf_index]
                                                {
                                                    Some(partial_inf_index)
                                                } else {
                                                    None
                                                }
                                            } else {
                                                Some(absolute_inf_index)
                                            }
                                        } else {
                                            partial_inf_index
                                        }
                                    },
                                )
                                .expect("The poset is not a lower semilattice.")
                        }
                    });

                inf
            },
        );

        Lattice { sup, inf }
    }
}

#[cfg(test)]
mod test {
    use super::Lattice;
    use crate::partition::raw_partition::{self, RawPartition};

    #[test]
    fn lattice_from_partitions() {
        for n in 2..=6usize {
            print!("\t{}...", n);

            let lattice: Lattice = raw_partition::new_partitions_set(n).as_slice().into();

            assert_eq!(lattice.is_lattice(), true);

            println!("ok");
        }
    }

    #[test]
    #[should_panic]
    /// Trying to build a lattice from a poset (not a lattice).
    fn lattice_from_poset_1() {
        let partition_size = 5;

        // The poset, not a lattice.
        //
        //   (abe)(cd)      (ab)(cde)
        //       | \          / |
        //       |   \      /   |
        //       |     \  /     |
        //       |      /\      |
        //       |    /   \     |
        //       |  /       \   |
        //       |/           \ |
        //     (ab)           (cd)
        //       \             /
        //         \         /
        //           \     /
        //             \ /
        //              Δ

        let abe_cd = RawPartition::new(&[0b10011, 0b01100], partition_size);
        let ab_cde = RawPartition::new(&[0b00011, 0b11100], partition_size);
        let ab = RawPartition::new(&[0b00011], partition_size);
        let cd = RawPartition::new(&[0b01100], partition_size);
        let delta = RawPartition::new(&[0], partition_size);

        let partitions_set = [abe_cd, ab_cde, ab, cd, delta];

        let _lattice: Lattice = (&partitions_set[..]).into();
    }

    #[test]
    #[should_panic]
    /// Trying to build a lattice from a poset (not a lattice).
    fn lattice_from_poset_2() {
        let partition_size = 5;

        // The poset, not a lattice.
        //
        //              ∇
        //            /   \
        //          /       \
        //        /           \
        //   (abe)(cd)      (ab)(cde)
        //       | \          / |
        //       |   \      /   |
        //       |     \  /     |
        //       |      /\      |
        //       |    /   \     |
        //       |  /       \   |
        //       |/           \ |
        //     (ab)           (cd)

        let nabla = RawPartition::new(&[0b11111], partition_size);
        let abe_cd = RawPartition::new(&[0b10011, 0b01100], partition_size);
        let ab_cde = RawPartition::new(&[0b00011, 0b11100], partition_size);
        let ab = RawPartition::new(&[0b00011], partition_size);
        let cd = RawPartition::new(&[0b01100], partition_size);

        let partitions_set = [nabla, abe_cd, ab_cde, ab, cd];

        let _lattice: Lattice = (&partitions_set[..]).into();
    }

    #[test]
    fn is_distributive() {
        let lattice: Lattice = raw_partition::new_partitions_set(3).as_slice().into();

        assert_eq!(lattice.is_lattice(), true);
        assert_eq!(lattice.is_distributive(), false);
    }

    #[test]
    fn is_modular() {
        let lattice: Lattice = raw_partition::new_partitions_set(3).as_slice().into();

        assert!(lattice.is_lattice());
        assert!(lattice.is_modular());
    }

    #[test]
    fn is_not_distributive() {
        let lattice: Lattice = raw_partition::new_partitions_set(4).as_slice().into();

        assert!(lattice.is_lattice());
        assert!(!lattice.is_distributive());
    }

    #[test]
    fn is_not_modular() {
        let lattice: Lattice = raw_partition::new_partitions_set(4).as_slice().into();

        assert!(lattice.is_lattice());
        assert!(!lattice.is_modular());
    }
}
