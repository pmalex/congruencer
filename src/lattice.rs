use itertools::{iproduct, Itertools};
use rayon::{
    iter::IndexedParallelIterator,
    prelude::{IntoParallelIterator, ParallelIterator},
};
use std::sync::atomic::{AtomicBool, Ordering};

use crate::binary_operation::BinaryOperation;

pub struct Lattice {
    sup: BinaryOperation,
    inf: BinaryOperation,
}

impl Lattice {
    /// Cheks the basic lattice identities.
    pub fn is_lattice(&self) -> bool {
        let Self { sup, inf } = self;

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
        let Self { sup, inf } = self;

        (0..sup.get_number_of_elements())
            .cartesian_product(0..sup.get_number_of_elements())
            .collect_vec()
            .into_par_iter()
            .all(|(a, b)| inf.apply(a, sup.apply(a, b)) == a && sup.apply(a, inf.apply(a, b)) == a)
    }

    pub fn is_modular(&self) -> bool {
        todo!()
    }

    pub fn is_distributive(&self) -> bool {
        debug_assert_eq!(
            self.sup.get_number_of_elements(),
            self.inf.get_number_of_elements()
        );

        let number_of_elements = self.sup.get_number_of_elements();

        iproduct!(
            (0..number_of_elements),
            (0..number_of_elements),
            (0..number_of_elements)
        )
        .collect_vec()
        .into_par_iter();

        todo!()
    }
}

impl<T> Into<Lattice> for &[T]
where
    T: PartialOrd + Sync,
{
    fn into(self) -> Lattice {
        let poset = self;

        let number_of_elements = poset.len();

        let (sup, inf) = rayon::join(
            || {
                let mut sup = BinaryOperation::new(poset.len());

                sup.par_iter_mut()
                    .enumerate()
                    .for_each(|(absolute_index, result_index)| {
                        // Here we calculate an indexes that lie inside the 0..number_of_elements range.
                        let element_a_index = absolute_index / number_of_elements; // The row number in Cayley table.
                        let element_b_index = absolute_index % number_of_elements; // The column number in Cayley table.

                        *result_index = if element_a_index == element_b_index {
                            // sup(a, a) = a
                            element_a_index
                        } else if &poset[element_a_index] <= &poset[element_b_index] {
                            // sup(a, b) = b for a <= b
                            element_b_index
                        } else if &poset[element_b_index] <= &poset[element_a_index] {
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
                                            if &poset[poset_element_index] <= &poset[sup_index]
                                                && &poset[element_a_index] <= &poset[poset_element_index]
                                                && &poset[element_b_index] <= &poset[poset_element_index]
                                            {
                                                Some(poset_element_index)
                                            } else {
                                                Some(sup_index)
                                            }
                                        } else if &poset[element_a_index] <= &poset[poset_element_index]
                                            && &poset[element_b_index] <= &poset[poset_element_index]
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
                                                if &poset[absolute_sup_index] <= &poset[partial_sup_index] {
                                                    Some(absolute_sup_index)
                                                } else if &poset[partial_sup_index]
                                                    <= &poset[absolute_sup_index]
                                                {
                                                    Some(partial_sup_index)
                                                } else {
                                                    None
                                                }
                                            } else {
                                                Some(absolute_sup_index)
                                            }
                                        } else if let Some(partial_sup) = partial_sup_index {
                                            Some(partial_sup)
                                        } else {
                                            None
                                        }
                                    },
                                )
                                .expect("The poset is not an upper lattice.")
                                }
                    });

                sup
            },
            || {
                let mut inf = BinaryOperation::new(poset.len());

                inf.par_iter_mut()
                    .enumerate()
                    .for_each(|(absolute_index, result_index)| {
                        // Here we calculate an indexes that lie inside the 0..number_of_elements range.
                        let element_a_index = absolute_index / number_of_elements; // The row number in Cayley table.
                        let element_b_index = absolute_index % number_of_elements; // The column number in Cayley table.

                        *result_index = if element_a_index == element_b_index {
                            // inf(a, a) = a
                            element_a_index
                        } else if &poset[element_a_index] >= &poset[element_b_index] {
                            // inf(a, b) = b for a >= b
                            element_b_index
                        } else if &poset[element_b_index] >= &poset[element_a_index] {
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
                                            if &poset[poset_element_index] >= &poset[inf_index]
                                                && &poset[element_a_index] >= &poset[poset_element_index]
                                                && &poset[element_b_index] >= &poset[poset_element_index]
                                            {
                                                Some(poset_element_index)
                                            } else {
                                                Some(inf_index)
                                            }
                                        } else if &poset[element_a_index] >= &poset[poset_element_index]
                                            && &poset[element_b_index] >= &poset[poset_element_index]
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
                                                if &poset[absolute_inf_index] >= &poset[partial_inf_index] {
                                                    Some(absolute_inf_index)
                                                } else if &poset[partial_inf_index]
                                                    >= &poset[absolute_inf_index]
                                                {
                                                    Some(partial_inf_index)
                                                } else {
                                                    None
                                                }
                                            } else {
                                                Some(absolute_inf_index)
                                            }
                                        } else if let Some(partial_inf) = partial_inf_index {
                                            Some(partial_inf)
                                        } else {
                                            None
                                        }
                                    },
                                )
                                .expect("The poset is not a lower lattice.")
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
    use crate::partition::Partition;

    #[test]
    fn lattice_from_partitions() {
        for n in 2..=6usize {
            print!("\t{}...", n);

            let lattice: Lattice = Partition::new_partition_set(n).as_slice().into();

            assert_eq!(lattice.is_lattice(), true);

            println!("ok");
        }
    }

    #[test]
    #[should_panic]
    /// A try to build a lattice from a poset (not a lattice).
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

        let abe_cd = Partition::new(&[0b10011, 0b01100], partition_size);
        let ab_cde = Partition::new(&[0b00011, 0b11100], partition_size);
        let ab = Partition::new(&[0b00011], partition_size);
        let cd = Partition::new(&[0b01100], partition_size);
        let delta = Partition::new(&[0], partition_size);

        let partitions_set = [abe_cd, ab_cde, ab, cd, delta];

        let _lattice: Lattice = (&partitions_set[..]).into();
    }

    #[test]
    #[should_panic]
    /// A try to build a lattice from a poset (not a lattice).
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

        let nabla = Partition::new(&[0b11111], partition_size);
        let abe_cd = Partition::new(&[0b10011, 0b01100], partition_size);
        let ab_cde = Partition::new(&[0b00011, 0b11100], partition_size);
        let ab = Partition::new(&[0b00011], partition_size);
        let cd = Partition::new(&[0b01100], partition_size);

        let partitions_set = [nabla, abe_cd, ab_cde, ab, cd];

        let _lattice: Lattice = (&partitions_set[..]).into();
    }
}
