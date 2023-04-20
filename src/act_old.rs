use crate::{congruence::Congruence, partition::Partition, ElementIndex};
use rayon::prelude::{IntoParallelIterator, ParallelIterator};

pub struct Act {
    cayley_table: Vec<ElementIndex>,
    columns: usize,
}

impl Act {
    pub fn new_from_cayley_table(table: &[ElementIndex], columns: usize) -> Self {
        assert!(columns > 0 && columns <= table.len());
        assert!(!table.is_empty() && table.len() % columns == 0);

        Self {
            cayley_table: table.to_owned(),
            columns,
        }
    }

    #[inline(always)]
    pub fn get_semigroup_size(&self) -> usize {
        self.columns
    }

    /// Multiply a set of an act's elements by a semigroup element `s`.
    pub fn m(&self, mut class: ElementIndex, s: usize) -> ElementIndex {
        // Check that the number (class) and the table are compatible.
        debug_assert!(
            ((ElementIndex::BITS - class.leading_zeros() - 1) as usize) // Position of the most significant unity.
            <
            self.cayley_table.len() / self.columns, // Number of rows.
            "The class({:b}) is too big, the table does not contains enough values",
            class
        );

        debug_assert!(s < self.columns);

        let mut result = 0;

        for i in 0..ElementIndex::BITS as usize {
            if class == 0 {
                break;
            }

            if class & 0b1 == 0b1 {
                result |= 1 << self.cayley_table[i * self.columns + s];
            }

            class >>= 1;
        }

        result
    }
}

impl Congruence for Act {
    fn is_congruence(&self, partition: &Partition) -> bool {
        let act = self;

        partition.par_iter().all(|&class| {
            debug_assert!(class > 0);

            (0..act.get_semigroup_size()).into_par_iter().all(|s| {
                let new_class = act.m(class as ElementIndex, s) as u16;

                debug_assert!(new_class > 0);

                partition
                    .par_iter()
                    .any(|&base_class| (new_class | base_class) == base_class)
            })
        })
    }

    #[inline(always)]
    fn size(&self) -> usize {
        self.cayley_table.len() / self.columns
    }
}

#[cfg(test)]
mod test {
    use crate::{act_old::Act, congruence::Congruence, partition::Partition};

    #[test]
    fn act_multiplication() {
        #[rustfmt::skip]
        let cayley_table = [
            /* 0 */ 1, 0, 3, 2, 2,
            /* 1 */ 3, 1, 1, 0, 2,
            /* 2 */ 2, 3, 3, 1, 2,
            /* 3 */ 3, 3, 2, 3, 2,
        ];

        let act = Act::new_from_cayley_table(&cayley_table, 5);

        assert_eq!(act.m(0b01010, 0), 0b01000);
        assert_eq!(act.m(0b01010, 1), 0b01010);
        assert_eq!(act.m(0b01010, 2), 0b00110);
        assert_eq!(act.m(0b01010, 3), 0b01001);
        assert_eq!(act.m(0b01101, 4), 0b00100);
    }

    #[test]
    fn is_congruence_1() {
        #[rustfmt::skip]
        let cayley_table = [
            /* 0 */ 1, 0, 3, 2, 2,
            /* 1 */ 3, 1, 1, 0, 2,
            /* 2 */ 2, 3, 3, 1, 2,
            /* 3 */ 3, 3, 2, 3, 2,
        ];

        let act = Act::new_from_cayley_table(&cayley_table, 5);

        let partition = Partition::new(&[0b1010, 0b0101], 4);

        assert_eq!(act.is_congruence(&partition), false);
    }

    #[test]
    fn is_congruence_2() {
        #[rustfmt::skip]
        let cayley_table = [
            /* 0 */ 1, 3, 2, 2,
            /* 1 */ 3, 0, 2, 1,
            /* 2 */ 3, 1, 2, 0,
            /* 3 */ 3, 2, 0, 3,
        ];

        let act = Act::new_from_cayley_table(&cayley_table, 4);

        let partition = Partition::new(&[0b1010, 0b0101], 4);

        assert_eq!(act.is_congruence(&partition), true);
    }
}
